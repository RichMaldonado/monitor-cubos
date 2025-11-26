# R/mod_sectores.R

# ---------------------------------
# --      Función de la UI       --
# ---------------------------------
mod_sectores_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      width = "25rem",
      title = "Filtros",
      
      shinyWidgets::pickerInput(
        inputId = ns("pkr_gv_sector"),
        label = "Elección de Gerencia de Venta",
        choices = NULL, # Se llena dinámicamente desde el server
        selected = NULL,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3",
          selectAllText = "Seleccionar Todo", deselectAllText = "Quitar Todo",
          noneSelectedText = "Cargando..."
        )
      ),
      shinyWidgets::pickerInput(
        inputId = ns("pkr_str_sector"),
        label = "Elección de Sector",
        choices = NULL, # Se llena dinámicamente
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 5",
          selectAllText = "Seleccionar Todo", deselectAllText = "Quitar Todo",
          noneSelectedText = "Ningún sector"
        )
      ),
      shinyWidgets::virtualSelectInput(
        inputId = ns("columnas_seleccionadas"),
        label = "Selecciona las métricas a mostrar:",
        choices = choices_virtual_select, # Este sí es fijo de global.R (estructura)
        multiple = TRUE,
        selected = unlist(choices_virtual_select, use.names = FALSE),
        search = TRUE,
        showValueAsTags = FALSE,
        placeholder = "Seleccionar métricas...",
        disableOptionGroupCheckbox = FALSE,
        optionsCount = 7
      )
    ),
    
    div(style = "display: flex; justify-content: space-between; align-items: center;",
        h4("Resumen General por Sector"),
        downloadButton(ns("descargar_sector"), "Descargar Vista en Excel", class = "btn-success btn-sm")
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(outputId = ns("tabla_resumen_sector"))
    )
  )
}


# ------------------------------------
# --      Función del Servidor      --
# ------------------------------------
mod_sectores_server <- function(id, datos, lista_gvs) {
  
  moduleServer(id, function(input, output, session) {
    
    # --- 1. Actualizar Filtros al Cargar Datos Nuevos ---
    observe({
      req(datos())
      

      df <- datos()
      lista_gvs = lista_gvs()
      todos_sectores <- unique(df$Sector)
      
      # Regex para encontrar GVs (ej. "GV 15", "GV15")
      nombres_gvs <- grep("GV\\s?[0-9]{1,2}", todos_sectores, value = TRUE)
      nombres_gvs <- sort(unique(nombres_gvs))
      
      if(length(nombres_gvs) == 0) {
        showNotification("No se encontraron Gerencias en la columna Sector", type = "warning")
      }
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "pkr_gv_sector",
        choices = lista_gvs,
        selected = lista_gvs 
      )
    })
    
    # --- 2. Lógica de Filtrado en Cascada ---
    observeEvent(input$pkr_gv_sector, {
      req(datos())
      df <- datos()
      todos_sectores <- unique(df$Sector)
      nombres_sectores_raw <- grep("GV\\s?[0-9]{1,2}", todos_sectores, value = TRUE)
      
      if (is.null(input$pkr_gv_sector) || length(input$pkr_gv_sector) == 0) {
        sectores_filtrados <- character(0)
      } else {
        numeros_gv <- stringr::str_extract(input$pkr_gv_sector, "[0-9]+")
        numeros_gv <- unique(numeros_gv[!is.na(numeros_gv)])
        
        if (length(numeros_gv) == 0) {
          sectores_filtrados <- character(0)
        } else {
          patron <- paste0("GV\\s?(", paste(numeros_gv, collapse = "|"), ")")
          sectores_filtrados <- nombres_sectores_raw[grepl(patron, nombres_sectores_raw)]
        }
      }
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "pkr_str_sector",
        choices = sort(unique(sectores_filtrados)),
        selected = sort(unique(sectores_filtrados))
      )
      
    }, ignoreNULL = FALSE)
    
    # --- 3. Dataset Reactivo Final ---
    datos_filtrados_sector <- reactive({
      req(datos(), input$pkr_str_sector) 
      datos() %>% dplyr::filter(Sector %in% input$pkr_str_sector)
    })
    
    # --- 4. Renderizado de Tabla ---
    output$tabla_resumen_sector <- DT::renderDT({
      req(input$columnas_seleccionadas)
      
      df_base <- datos_filtrados_sector()
      cols_seleccionadas <- input$columnas_seleccionadas
      
      # === VALIDACIÓN DE COLUMNAS ===
      # Verificamos qué columnas seleccionadas NO existen en el dataset
      cols_faltantes <- setdiff(cols_seleccionadas, names(df_base))
      
      if (length(cols_faltantes) > 0) {
        warning(paste("Las siguientes columnas no existen en los datos y serán ignoradas:", 
                      paste(cols_faltantes, collapse = ", ")))
        # Nos quedamos solo con las que sí existen
        cols_seleccionadas <- intersect(cols_seleccionadas, names(df_base))
      }
      
      req(length(cols_seleccionadas) > 0) # Detener si no quedan columnas válidas
      
      # === SELECCIÓN DE DATOS ===
      # Quitamos "%Sect /GV" y usamos any_of para seguridad
      datos_mostrados <- df_base %>%
        dplyr::select(
          `Código Sector`, 
          `Sector`, 
          dplyr::any_of(cols_seleccionadas) # any_of evita error si una col no existe
        )
      
      estilo_borde <- '2px solid #666'
      
      # === CONSTRUCCIÓN DEL SKETCH (ENCABEZADOS) (MODIFICADO) ===
      lookup_nombres <- setNames(names(unlist(estructura_columnas)), unlist(estructura_columnas))
      
      # Inicializamos vacío porque ya no está la columna fija de % Part
      cols_inicio_grupo <- c() 
      
      # Fila 1: Solo Código y Sector fijos ahora
      header_row_1 <- list(
        tags$th(rowspan = 2, 'Código'), 
        tags$th(rowspan = 2, 'Sector', style = paste0("border-right:", estilo_borde))
        # SE ELIMINÓ EL TH DE % PART AQUÍ
      )
      
      header_row_2 <- list()
      
      for (grupo in names(estructura_columnas)) {
        columnas_del_grupo <- unlist(estructura_columnas[[grupo]], use.names = FALSE)
        # Intersección segura con las columnas que realmente existen en el df seleccionado
        seleccionadas_en_grupo <- intersect(columnas_del_grupo, names(datos_mostrados))
        
        num_seleccionadas <- length(seleccionadas_en_grupo)
        
        if (num_seleccionadas > 0) {
          header_row_1 <- append(header_row_1, list(
            tags$th(colspan = num_seleccionadas, class = 'dt-center', 
                    style = paste0("border-left:", estilo_borde), grupo)
          ))
          for (i in seq_along(seleccionadas_en_grupo)) {
            col_id <- seleccionadas_en_grupo[i]
            nombre_display <- if(!is.na(lookup_nombres[col_id])) sub(".*\\.", "", lookup_nombres[col_id]) else col_id
            
            if (i == 1) {
              header_row_2 <- append(header_row_2, list(tags$th(style = paste0("border-left:", estilo_borde), nombre_display)))
              cols_inicio_grupo <- c(cols_inicio_grupo, col_id)
            } else {
              header_row_2 <- append(header_row_2, list(tags$th(nombre_display)))
            }
          }
        }
      }
      
      sketch <- tags$table(class = 'display', tags$thead(tags$tr(header_row_1), tags$tr(header_row_2)))
      
      # === CREACIÓN DT ===
      dt <- datatable(
        datos_mostrados, container = sketch, rownames = FALSE, 
        options = list(
          language = opciones_espanol,
          columnDefs = list(list(className = 'dt-center', targets = '_all'), list(targets = 1, className = 'dt-left')),
          fixedColumns = list(leftColumns = 2), # Sigue siendo 2 (Código y Sector)
          scrollX = TRUE
        )
      )
      
      # === FORMATOS (MODIFICADO) ===
      cols_visibles <- names(datos_mostrados)
      
      # Se eliminó "%Sect /GV" de esta lista
      cols_pct <- intersect(c("Facturacion % Cumpl", "Disponibles % Cumpl", "Activas % Cumpl", "Saldo % Cumpl", "Productividad % Cumpl", "% Actividad Real", "% Actividad Meta", "% Actividad % Cumpl", "Inicios + Reinicios % Cumpl", "Recuperos % Cumpl", "% I3", "% I2"), cols_visibles)
      if (length(cols_pct) > 0) dt <- dt %>% formatPercentage(cols_pct, digits = 1)
      
      cols_curr <- intersect(c('Facturación Meta', 'Facturación Real', 'Facturacion Faltan 95%', 'Facturacion Faltan 100%', 'Productividad Meta', 'Productividad Real'), cols_visibles)
      if (length(cols_curr) > 0) dt <- dt %>% formatCurrency(cols_curr, currency = "$", digits = 0)
      
      # Semáforos condicionales (se mantienen igual)
      if ("Facturacion % Cumpl" %in% cols_visibles) dt <- dt %>% formatStyle('Facturacion % Cumpl', backgroundColor = styleInterval(c(0.9, 1), c("#F8696B", "white", "#63BE7B")))
      if ("Activas % Cumpl" %in% cols_visibles) dt <- dt %>% formatStyle('Activas % Cumpl', backgroundColor = styleInterval(c(0.8, 1), c("#F8696B", "white", "#63BE7B")))
      if ("Disponibles % Cumpl" %in% cols_visibles) dt <- dt %>% formatStyle('Disponibles % Cumpl', backgroundColor = styleInterval(c(0.95, 1.05), c("#F8696B", "white", "#63BE7B")))
      if ("Productividad % Cumpl" %in% cols_visibles) dt <- dt %>% formatStyle('Productividad % Cumpl', backgroundColor = styleInterval(c(1), c("#F8696B", "#63BE7B")))
      if ("Inicios + Reinicios % Cumpl" %in% cols_visibles) dt <- dt %>% formatStyle('Inicios + Reinicios % Cumpl', backgroundColor = styleInterval(c(1), c("#F8696B", "#63BE7B")))
      
      # Bordes (se mantienen igual, la lógica es dinámica basada en cols_inicio_grupo)
      cols_borde_final <- intersect(cols_inicio_grupo, cols_visibles)
      if (length(cols_borde_final) > 0) dt <- dt %>% formatStyle(cols_borde_final, borderLeft = estilo_borde)
      
      return(dt)
    })
    
    output$descargar_sector <- downloadHandler(
      filename = function() { paste0("Reporte_Sectores_", Sys.Date(), ".xlsx") },
      content = function(file) {
        req(datos_filtrados_sector())
        # Actualizado select para descarga también
        writexl::write_xlsx(datos_filtrados_sector() %>% dplyr::select(`Código Sector`, `Sector`, dplyr::any_of(input$columnas_seleccionadas)), path = file)
      }
    )
  })
}














