# global.R

# --- 1. Cargar Librerías ---
library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(stringr)
library(bslib)
library(shinyWidgets)
library(htmltools)
library(scales)
library(shinycssloaders)
library(jsonlite)
library(here)

# --- 2. Configuración de Rutas y Detección de Ciclos ---

ruta_carpeta_datos <- here::here("datos")

# Buscamos todos los archivos de Gerencia de Ventas para identificar qué ciclos existen
archivos_gv_raw <- list.files(
  path = ruta_carpeta_datos, 
  pattern = "CUBOgv_\\d{6}\\.xlsx"
)

# Extraemos los números de ciclo (ej: "202517") usando Expresiones Regulares
# Esto busca exactamente 6 dígitos consecutivos en el nombre del archivo
ciclos_encontrados <- stringr::str_extract(archivos_gv_raw, "\\d{6}")

# Ordenamos descendente (el más reciente primero) y eliminamos duplicados si los hubiera
ciclos_disponibles <- sort(unique(ciclos_encontrados), decreasing = TRUE)

if (length(ciclos_disponibles) == 0) {
  warning("ALERTA: No se encontraron archivos con el patrón 'CUBOgv_YYYYCC.xlsx' en la carpeta datos.")
  # Valor por defecto para evitar crash, aunque la app no mostrará datos
  ciclos_disponibles <- c("Sin Datos") 
}

ruta_json <- file.path(ruta_carpeta_datos, "update_timestamp.json")


# --- 3. Funciones Auxiliares ---

division_segura <- function(numerador, denominador){
  # 1. División estándar de R (Maneja vectores vs escalares automáticamente)
  resultado <- numerador / denominador

  # 2. Limpieza: Buscar valores que no sean finitos
  # Esto atrapa: Inf (n/0), -Inf (-n/0), NaN (0/0) y NA
  # Y los convierte todos a 0
  resultado[!is.finite(resultado)] <- 0

  return(resultado)
}

clean_to_numeric <- function(x) {
  # Si ya es numérico, devolverlo tal cual
  if (is.numeric(x)) return(x)
  
  # Convertir a caracter para manipular
  x <- as.character(x)
  
  # 1. Reemplazar guiones aislados ("-") por 0 (común en reportes financieros)
  x[x == "-"] <- "0"
  
  # 2. Eliminar todo lo que NO sea dígitos, punto decimal o signo menos
  # Esto quita: comas, espacios, símbolos de moneda ($), letras, etc.
  x_clean <- gsub("[^0-9.-]", "", x)
  
  # 3. Convertir a numérico silenciando advertencias
  suppressWarnings(as.numeric(x_clean))
}

#' Leer Datos Crudos (Solo lectura)
leer_excel_raw <- function(ruta_archivo) {
  # Solo leemos y limpiamos espacios en blanco de los nombres de columnas
  # para que R no tenga problemas, pero NO tocamos los datos.
  df <- read_excel(ruta_archivo, sheet = "CUBO")
  names(df) <- trimws(names(df)) 
  return(df)
}


# --- 4. Funciones de Procesamiento de Datos (ETL) ---

#' Procesar Datos de Gerencia de Ventas
generar_tablas_gv <- function(ruta_archivo) {
  
  cubo_pais_df_raw <- read_excel(ruta_archivo, sheet = "CUBO")
  
  # Limpieza de Nombres de Columnas (Quitar espacios al inicio/final)
  names(cubo_pais_df_raw) <- trimws(names(cubo_pais_df_raw))
  
  # Limpieza inicial
  cubo_pais_df_clean <- cubo_pais_df_raw %>%
    filter(grepl("GV [0-9]{1,2}", `Nombre GV`)) %>% 
    # Aplicamos la limpieza a TODAS las columnas excepto las de texto identificadoras
    mutate(across(-c(`Nombre GV`, `Código GV`), clean_to_numeric)) %>%
    filter(
      stringr::str_detect(`Nombre GV`, "GV"),
      !(`Nombre GV` %in% c("GV PRUEBAS DE PROMO", "GV COLABORADORES CELAYA"))
    )
  
  # Agregación (Suma por Gerencia)
  cubo_pais_agregado <- cubo_pais_df_clean %>%
    group_by(`Código GV`, `Nombre GV`) %>%
    summarise(
      across(where(is.numeric), ~sum(., na.rm = TRUE)), .groups = 'drop'
    ) %>%
    arrange(`Código GV`)
  
  
  # Tabla 1: Inactividad
  tabla_inactividad_equipos <- cubo_pais_agregado %>%
    select(
      GV = `Código GV`, Equipo = `Nombre GV`, Disp_Real_C7 = `Disponibles Real`,
      Inact_Real_3 = `Inactiva 3 Real`, Inact_2 = `Inactiva 2 Real`, Inact_1 = `Inactiva 1 Real`
    ) %>%
    mutate(
      Porc_Inact_3 = division_segura(Inact_Real_3, Disp_Real_C7),
      Porc_Inact_2 = division_segura(Inact_2, Disp_Real_C7),
      Porc_Inact_1 = division_segura(Inact_1, Disp_Real_C7)
    )
  total_inactividad <- tabla_inactividad_equipos %>%
    summarise(GV = NA, Equipo = "TOTAL PAIS", across(where(is.numeric), ~sum(., na.rm = TRUE))) %>%
    mutate(
      Porc_Inact_3 = division_segura(Inact_Real_3, Disp_Real_C7),
      Porc_Inact_2 = division_segura(Inact_2, Disp_Real_C7),
      Porc_Inact_1 = division_segura(Inact_1, Disp_Real_C7)
    )
  t_inactividad <- bind_rows(tabla_inactividad_equipos, total_inactividad)
  
  # Tabla 2: Operaciones
  tabla_disponibles_equipos <- cubo_pais_agregado %>%
    select(
      GV = `Código GV`, Equipo = `Nombre GV`,
      Disp_Meta = `Disponibles Meta`, Disp_Real = `Disponibles Real`,
      Saldo_Meta = `Saldo Disponibles Meta`, Saldo_Real = `Saldo Disponibles Real`,
      Inicios_Meta = `Inicios + Reinicios Meta`, Inicios_Real = `Inicios + Reinicios Real`,
      Recuperos_Meta = `Recuperos Meta`, Recuperos_Real = `Recuperos Real`
    ) %>%
    mutate(
      Disp_Alcanz     = division_segura(Disp_Real, Disp_Meta),
      Saldo_Alcanz    = division_segura(Saldo_Real, Saldo_Meta),
      Inicios_Cump    = division_segura(Inicios_Real, Inicios_Meta),
      Inicios_vs_Disp = division_segura(Inicios_Real, Disp_Real),
      Recuperos_Cump  = division_segura(Recuperos_Real, Recuperos_Meta),
      Recuperos_vs_Disp = division_segura(Recuperos_Real, Disp_Real)
    )
  total_disponibles <- tabla_disponibles_equipos %>%
    summarise(GV = NA, Equipo = "TOTAL PAIS", across(where(is.numeric), ~sum(., na.rm = TRUE))) %>%
    mutate(
      Disp_Alcanz     = division_segura(Disp_Real, Disp_Meta),
      Saldo_Alcanz    = division_segura(Saldo_Real, Saldo_Meta),
      Inicios_Cump    = division_segura(Inicios_Real, Inicios_Meta),
      Inicios_vs_Disp = division_segura(Inicios_Real, Disp_Real),
      Recuperos_Cump  = division_segura(Recuperos_Real, Recuperos_Meta),
      Recuperos_vs_Disp = division_segura(Recuperos_Real, Disp_Real)
    )
  t_disponibles <- bind_rows(tabla_disponibles_equipos, total_disponibles)
  
  # Tabla 3: Actividad
  tabla_actividad_equipos <- cubo_pais_agregado %>%
    select(
      GV = `Código GV`, Equipo = `Nombre GV`,
      Actividad_Porc_Meta = `% Actividad Meta`, Activas_Meta = `Activas Total Meta`,
      Activas_Real = `Activas Total Real`, Act_Frec_Porc_Meta = `% Actividad Frecuente Meta`,
      Act_Frec_Porc_Real = `% Actividad Frecuente Real`, Disp_Real = `Disponibles Real`
    ) %>%
    mutate(
      Actividad_Porc_Meta = Actividad_Porc_Meta / 100, 
      Act_Frec_Porc_Meta = Act_Frec_Porc_Meta / 100,
      Act_Frec_Porc_Real = Act_Frec_Porc_Real / 100,
      Actividad_Porc_Real = division_segura(Activas_Real, Disp_Real),
      Actividad_Porc_Alcanz = division_segura(Actividad_Porc_Real, Actividad_Porc_Meta),
      Activas_Alcanz = division_segura(Activas_Real, Activas_Meta),
      Act_Frec_Porc_Alcanz = division_segura(Act_Frec_Porc_Real, Act_Frec_Porc_Meta)
    ) %>%
    select(-Disp_Real)
  total_actividad <- tabla_actividad_equipos %>%
    summarise(
      GV = NA, Equipo = "TOTAL PAIS",
      Activas_Meta = sum(Activas_Meta, na.rm = TRUE), 
      Activas_Real = sum(Activas_Real, na.rm = TRUE)
    )
  t_actividad <- bind_rows(tabla_actividad_equipos, total_actividad)
  
  # Tabla 4: Productividad
  tabla_productividad_equipos <- cubo_pais_agregado %>%
    select(
      GV = `Código GV`, Equipo = `Nombre GV`,
      Prod_Dolar_Real = `Productividad Real`, Fact_Meta = `Facturación Total Meta`,
      Fact_Real = `Facturación Total Real`
    ) %>%
    mutate(Fact_Alcanz = division_segura(Fact_Real, Fact_Meta))
  total_productividad <- tabla_productividad_equipos %>%
    summarise(GV = NA, Equipo = "TOTAL PAIS", across(where(is.numeric), ~sum(., na.rm = TRUE))) %>%
    mutate(Fact_Alcanz = division_segura(Fact_Real, Fact_Meta))
  t_productividad <- bind_rows(tabla_productividad_equipos, total_productividad)
  
  list(inactividad = t_inactividad, disponibles = t_disponibles, actividad = t_actividad, productividad = t_productividad)
}

#' Procesar Datos de Sectores
#' Procesar Datos de Sectores (VERSIÓN ROBUSTA ANTI-ACENTOS)
generar_tabla_sectores <- function(ruta_archivo) {
  
  # 1. Leer archivo
  cubo_sector_df_raw <- read_excel(ruta_archivo, sheet = "CUBO")
  
  # --- DIAGNÓSTICO EN CONSOLA (Para ver qué está leyendo R) ---
  # message("Columnas encontradas en archivo Sector: ")
  # print(names(cubo_sector_df_raw))
  
  # 2. LIMPIEZA DE ENCABEZADOS (Clave para corregir el error)
  # Quitamos acentos manualmente para evitar problemas de codificación
  nuevos_nombres <- names(cubo_sector_df_raw)
  nuevos_nombres <- iconv(nuevos_nombres, to = "ASCII//TRANSLIT") # Intenta convertir a ASCII
  nuevos_nombres <- gsub("ó", "o", nuevos_nombres) # Refuerzo manual
  nuevos_nombres <- gsub("Ó", "O", nuevos_nombres)
  nuevos_nombres <- gsub("é", "e", nuevos_nombres)
  nuevos_nombres <- gsub("í", "i", nuevos_nombres) 
  nuevos_nombres <- gsub("á", "a", nuevos_nombres)
  nuevos_nombres <- trimws(nuevos_nombres) # Quitar espacios
  
  names(cubo_sector_df_raw) <- nuevos_nombres
  
  # message("Columnas normalizadas: ")
  # print(names(cubo_sector_df_raw))
  
  # 3. Limpieza de Datos
  # Ahora usamos los nombres SIN ACENTOS en la lógica
  cubo_clean <- cubo_sector_df_raw %>%
    filter(grepl("GV[0-9]{1,2}", `Nombre Sector`)) %>%
    # Solo aplicamos clean_to_numeric a las columnas que NO son Identificadores
    mutate(across(-c(`Nombre Sector`, `Codigo Sector`), clean_to_numeric))
  
  # 4. Verificar existencia de la columna crítica antes de operar
  if (!"Facturacion Total Real" %in% names(cubo_clean)) {
    stop("ERROR CRÍTICO: No se encuentra la columna 'Facturacion Total Real' (sin tilde). Revisa los logs de nombres.")
  }
  
  total_facturacion_gerencia <- sum(cubo_clean$`Facturacion Total Real`, na.rm = TRUE)
  if(total_facturacion_gerencia == 0) total_facturacion_gerencia <- 1 
  

  # 5. Cálculos
  resumen_general <- cubo_clean %>%
    mutate(
      # Usamos "Facturacion" sin tilde
      `%Sect /GV` = division_segura(`Facturacion Total Real`, total_facturacion_gerencia),
      
      `Facturacion % Cumpl` = division_segura(`Facturacion Total Real`, `Facturacion Total Meta`),
      `Facturacion Faltan 95%` = (`Facturacion Total Meta` * 0.95) - `Facturacion Total Real`,
      `Facturacion Faltan 100%` = `Facturacion Total Meta` - `Facturacion Total Real`,
      
      `Disponibles % Cumpl` = division_segura(`Disponibles Real`, `Disponibles Meta`),
      `Disponibles Faltan 95%` = (`Disponibles Meta` * 0.95) - `Disponibles Real`,
      `Disponibles Faltan 100%` = `Disponibles Meta` - `Disponibles Real`,
      
      `Activas % Cumpl` = division_segura(`Activas Total Real`, `Activas Total Meta`),
      `Activas Faltan 95%` = (`Activas Total Meta` * 0.95) - `Activas Total Real`,
      `Activas Faltan 100%` = `Activas Total Meta` - `Activas Total Real`,
      
      `Saldo % Cumpl` = division_segura(`Saldo Disponibles Real`, `Saldo Disponibles Meta`),
      `Saldo Faltan!` = `Saldo Disponibles Meta` - `Saldo Disponibles Real`,
      
      `Productividad Meta` = division_segura(`Facturacion Total Meta`, `Activas Total Meta`),
      `Productividad Real` = division_segura(`Facturacion Total Real`, `Activas Total Real`),
      `Productividad % Cumpl` = division_segura(`Productividad Real`, `Productividad Meta`),
      
      `% Actividad Real` = division_segura(`Activas Total Real`, `Disponibles Real`),
      `% Actividad % Cumpl` = division_segura(`% Actividad Real`, `% Actividad Meta`),
      
      `Inicios + Reinicios % Cumpl` = division_segura(`Inicios + Reinicios Real`, `Inicios + Reinicios Meta`),
      `Inicios + Reinicios Faltan!` = `Inicios + Reinicios Meta` - `Inicios + Reinicios Real`,
      
      `Recuperos % Cumpl` = division_segura(`Recuperos Real`, `Recuperos Meta`),
      `Recuperos Faltan!` = if_else(is.na(`Recuperos Meta`), 0, `Recuperos Meta` - `Recuperos Real`),
      
      `% I3` = division_segura(`Inactiva 3 Real`, `Disponibles Real`),
      `% I2` = division_segura(`Inactiva 2 Real`, `Disponibles Real`)
    )
  
  # 6. Selección y Renombre Final (Aquí volvemos a poner los acentos para la visualización bonita)
  resumen_final <- resumen_general %>%
    select(
      `Código Sector` = `Codigo Sector`, # De vuelta a bonito
      `Sector` = `Nombre Sector`,
      `%Sect /GV`,
      
      `Facturación Meta` = `Facturacion Total Meta`,
      `Facturación Real` = `Facturacion Total Real`,
      `Facturacion % Cumpl`,
      `Facturacion Faltan 95%`,
      `Facturacion Faltan 100%`,
      
      `Disponibles Meta`,
      `Disponibles Real`,
      `Disponibles % Cumpl`,
      `Disponibles Faltan 95%`,
      `Disponibles Faltan 100%`,
      
      `Activas Meta` = `Activas Total Meta`,
      `Activas Real` = `Activas Total Real`,
      `Activas % Cumpl`,
      `Activas Faltan 95%`,
      `Activas Faltan 100%`,
      
      `Saldo Real` = `Saldo Disponibles Real`,
      `Saldo Meta` = `Saldo Disponibles Meta`,
      `Saldo % Cumpl`,
      `Saldo Faltan!`,
      
      `Productividad Meta`, 
      `Productividad Real`, 
      `Productividad % Cumpl`,
      
      `% Actividad Real`, 
      `% Actividad Meta`,
      `% Actividad % Cumpl`,
      
      `Inicios + Reinicios Meta`,
      `Inicios + Reinicios Real`,
      `Inicios + Reinicios % Cumpl`,
      `Inicios + Reinicios Faltan!`,
      
      `Recuperos Meta`,
      `Recuperos Real`,
      `Recuperos % Cumpl`,
      `Recuperos Faltan!`,
      
      `Inact 3 Real` = `Inactiva 3 Real`,
      `% I3`,
      `Inact 2 Real` = `Inactiva 2 Real`,
      `% I2`,
      
      `Indisponibles Total` = `Indisponibles Real`,
      `I4` = `Inactiva 4 Real`,
      `I5` = `Inactiva 5 Real`,
      `I6` = `Inactiva 6 Real`
    )
  
  return(resumen_final)
}



# --- 5. Estilos y Configuraciones UI Globales ---

theme_natura <- bslib::bs_theme(
  version = 5,
  
  # --- Tus colores actuales ---
  primary = "#FF9526",      
  secondary = "#2E2D2C",     
  info = "#33AAFF",         
  danger = "#A40025",       
  success = "#105243",      
  #bg = "#FFFFFF",           
  #fg = "#2E2D2C",           
  
  # --- Tus fuentes ---
  base_font = bslib::font_google("Montserrat", local = FALSE),
  heading_font = bslib::font_google("Lato", local = FALSE),
  
  # --- Ajustes Navbar ---
  "navbar-bg" = "#013731", 
  "navbar-light-color" = "#E1D1C1",
  "navbar-light-hover-color" = "#FF9526",
  "navbar-light-active-color" = "#FF9526",
  
  "bslib-sidebar-bg" = "#7373730d",
  "link-color" = "#FF9526",
  "link-hover-color" = "#CF3100"
  
) %>% 
  # AGREGAMOS ESTA REGLA CSS:
  bslib::bs_add_rules("
    /* Pone en negritas todo el texto dentro de la barra de navegación */
    .navbar, .navbar .nav-link, .navbar-brand, .navbar-text {
      font-weight: 900 !important; 
    }
    
    /* Corrección para el título del Sidebar */
    .bslib-sidebar-layout .sidebar-title {
      color: #013731 !important; /* Verde oscuro Natura */
      font-weight: 700;
    }
    
     /* --- 1. Arreglar Etiquetas del Sidebar (Filtros) --- */
  .bslib-sidebar-layout .sidebar-content .control-label, 
  .bslib-sidebar-layout .sidebar-content label {
    color: #2E2D2C !important; /* Gris oscuro casi negro */
    font-weight: 600;
  }

  /* --- 2. Arreglar Texto dentro de los Selectores (PickerInput) --- */
  .bootstrap-select .dropdown-toggle .filter-option-inner-inner {
    color: #2E2D2C !important; /* Asegura texto oscuro en el botón */
  }
  
  /* Arreglar el contador '230 items selected' si se ve claro */
  .bootstrap-select .dropdown-toggle {
    color: #2E2D2C !important;
  }
  
  /* --- 3. Arreglar Virtual Select (El último filtro) --- */
  .vscomp-toggle-button {
    color: #2E2D2C !important;
    background-color: #fff;
    border: 1px solid #ced4da;
  }
  .vscomp-wrapper {
    color: #2E2D2C !important; /* Texto de las opciones */
  }
    
    /* Asegura que el texto dentro del botón del selector también sea negrita */
    .navbar .filter-option-inner-inner {
      font-weight: 900 !important;
    }
  ")

opciones_espanol <- list(
  sProcessing     = "Procesando...",
  sLengthMenu     = "Mostrar _MENU_ registros",
  sZeroRecords    = "No se encontraron resultados",
  sEmptyTable     = "Ningún dato disponible en esta tabla",
  sInfo           = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
  sInfoEmpty      = "Mostrando registros del 0 al 0 de un total de 0 registros",
  sInfoFiltered   = "(filtrado de un total de _MAX_ registros)",
  sSearch         = "Buscar:",
  oPaginate = list(sFirst = "Primero", sLast = "Último", sNext = "Siguiente", sPrevious = "Anterior")
)

estructura_columnas <- list(
  "Facturación" = list(
    "Meta" = "Facturación Meta", "Real" = "Facturación Real", "% Cumpl" = "Facturacion % Cumpl",
    "Faltan 95%" = "Facturacion Faltan 95%", "Faltan 100%" = "Facturacion Faltan 100%"
  ),
  "Disponibles" = list(
    "Meta" = "Disponibles Meta", "Real" = "Disponibles Real", "% Cumpl" = "Disponibles % Cumpl",
    "Faltan 95%" = "Disponibles Faltan 95%", "Faltan 100%" = "Disponibles Faltan 100%"
  ),
  "Activas" = list(
    "Meta" = "Activas Meta", "Real" = "Activas Real", "% Cumpl" = "Activas % Cumpl",
    "Faltan 95%" = "Activas Faltan 95%", "Faltan 100%" = "Activas Faltan 100%"
  ),
  "Saldo" = list(
    "Real" = "Saldo Real", "Meta" = "Saldo Meta", "% Cumpl" = "Saldo % Cumpl", "Faltan!" = "Saldo Faltan!"
  ),
  "Productividad" = list(
    "Meta" = "Productividad Meta", "Real" = "Productividad Real", "% Cumpl" = "Productividad % Cumpl"
  ),
  "% Actividad" = list(
    "Real" = "% Actividad Real", "Meta" = "% Actividad Meta", "% Cumpl" = "% Actividad % Cumpl"
  ),
  "Inicios + Reinicios" = list(
    "Meta" = "Inicios + Reinicios Meta", "Real" = "Inicios + Reinicios Real", 
    "% Cumpl" = "Inicios + Reinicios % Cumpl", "Faltan!" = "Inicios + Reinicios Faltan!"
  ),
  "Recuperos" = list(
    "Meta" = "Recuperos Meta", "Real" = "Recuperos Real", "% Cumpl" = "Recuperos % Cumpl", "Faltan!" = "Recuperos Faltan!"
  ),
  "Inact 3" = list("Real" = "Inact 3 Real", "% I3" = "% I3"),
  "Inact 2" = list("Real" = "Inact 2 Real", "% I2" = "% I2"),
  "Indisponibles" = list("Total" = "Indisponibles Total", "Inact 4" = "I4", "Inact 5" = "I5", "Inact 6" = "I6")
)

choices_virtual_select <- estructura_columnas