# app.R - El Director de Orquesta (Versión Dinámica)

# --- 1. Cargar Lógica Global y Módulos ---
source("global.R") 

# Carga las funciones UI y Server de nuestros módulos
source("R/mod_gerencia_ventas.R")
source("R/mod_sectores.R")
source("R/mod_data_raw.R")


# --- 2. Definir la Interfaz de Usuario (UI) ---

ui <- page_navbar(
  
  title = "Cómo Vamos Mx",
  theme = theme_natura,
  
  header = tags$head(
    includeCSS(here::here("www", "styles.css"))
  ),
  
  # --- SELECTOR DE CICLO (Global para toda la app) ---

  # Pestaña 1: Gerencia de Ventas
  nav_panel(
    title = "Gerencia de Ventas",
    # Icono opcional de carga mientras se procesan datos
    mod_gerencia_ventas_ui("gerencia_ventas")
  ),
  
  # Pestaña 2: Sectores
  nav_panel(
    title = "Sectores",
    mod_sectores_ui("sectores")
  ),
  
  # Pestaña 3: Datos Crudos (NUEVO)
  nav_panel(title = "Base de Datos", mod_data_raw_ui("data_raw")), 
  
  nav_spacer(),
  
  # Muestra el ciclo activo en la barra superior
  nav_item(
    div(
      # 1. CSS Corregido: 'align-items' centra verticalmente, 'justify-content' horizontalmente
      style = "display: flex; gap: 1rem; align-items: center; justify-content: center;",
      
      pickerInput(
        inputId = "ciclo_seleccionado",
        label = NULL,
        choices = ciclos_disponibles,
        selected = ciclos_disponibles[1],
        # 2. Ancho fijo para evitar que rompa el navbar
        width = "140px", 
        options = list(
          `live-search` = TRUE,
          # 3. 'btn-sm' se ve mejor aquí dentro para hacer el botón más compacto
          style = "btn-light btn-sm" 
        )
      ),
      
      # 4. Envolver el output para evitar saltos de línea indeseados
      div(
        style = "white-space: nowrap; margin-bottom: 0px;", 
        uiOutput("last_updated_display")
      )
    )
  )
)


# --- 3. Definir la Lógica del Servidor (Server) ---

server <- function(input, output, session) {
  
  # --- A. Lógica de Carga de Datos PROCESADOS (Para gráficas y KPIs) ---
  datos_gv_procesados <- reactive({
    req(input$ciclo_seleccionado)
    ciclo <- input$ciclo_seleccionado
    ruta <- here::here("datos", paste0("CUBOgv_", ciclo, ".xlsx"))
    
    id_notif <- showNotification(paste("Procesando GV", ciclo, "..."), duration = NULL, type = "message")
    on.exit(removeNotification(id_notif), add = TRUE)
    
    tryCatch({ generar_tablas_gv(ruta) }, error = function(e) { NULL })
  })
  
  datos_sectores_procesados <- reactive({
    req(input$ciclo_seleccionado)
    ciclo <- input$ciclo_seleccionado
    ruta <- here::here("datos", paste0("CUBOsector_", ciclo, ".xlsx"))
    
    tryCatch({ generar_tabla_sectores(ruta) }, error = function(e) { NULL })
  })
  
  # --- B. Lógica de Carga de Datos CRUDOS (NUEVO - Para la tabla raw) ---
  # Leemos los archivos otra vez pero SIN PROCESAR para mostrarlos tal cual
  
  datos_gv_raw_reactivos <- reactive({
    req(input$ciclo_seleccionado)
    ruta <- here::here("datos", paste0("CUBOgv_", input$ciclo_seleccionado, ".xlsx"))
    tryCatch({ leer_excel_raw(ruta) }, error = function(e) { NULL })
  })
  
  datos_sector_raw_reactivos <- reactive({
    req(input$ciclo_seleccionado)
    ruta <- here::here("datos", paste0("CUBOsector_", input$ciclo_seleccionado, ".xlsx"))
    tryCatch({ leer_excel_raw(ruta) }, error = function(e) { NULL })
  })
  
  
  # --- C. Llamada a los Módulos ---
  
  # Módulos de reporte (usan datos procesados)
  mod_gerencia_ventas_server("gerencia_ventas", datos = datos_gv_procesados)
  mod_sectores_server("sectores", datos = datos_sectores_procesados)
  
  # Módulo de Datos Crudos (usa datos raw)
  mod_data_raw_server("data_raw", 
                      datos_gv = datos_gv_raw_reactivos, 
                      datos_sector = datos_sector_raw_reactivos)
  
  
  # --- D. Elementos Visuales Globales ---
  output$ciclo_label_nav <- renderText({ paste("Ciclo:", input$ciclo_seleccionado) })
  
  output$last_updated_display <- renderUI({
    if (file.exists(ruta_json)) {
      data <- jsonlite::fromJSON(ruta_json)
      timestamp_val_fijo <- format(as.POSIXct(data$last_successful_update, tz = "UTC"), "%d %b %Y, %H:%M")
    } else {
      timestamp_val_fijo <- "No disp."
    }
    tags$div(
      style = "font-size: 0.8em; color: #E1D1C1; ",
      icon("clock"), "Data actualizada:", tags$strong(timestamp_val_fijo) 
    )
  })
  
}

# --- 4. Ejecutar la Aplicación ---
shinyApp(ui = ui, server = server)