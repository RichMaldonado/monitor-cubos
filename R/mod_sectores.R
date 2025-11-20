# R/mod_sectores.R

# ---------------------------------
# --      Función de la UI       --
# ---------------------------------
# Define la interfaz de usuario para el módulo de Sectores.
mod_sectores_ui <- function(id) {
  ns <- NS(id)
  
  # Copiamos el layout_sidebar de la pestaña "Sectores" del app.R original.
  # Todos los inputId y outputId son envueltos en la función ns().
  layout_sidebar(
    sidebar = sidebar(
      width = "25rem",
      title = "Filtros",
      
      shinyWidgets::pickerInput(
        inputId = ns("pkr_gv_sector"),
        label = "Elección de Gerencia de Venta",
        choices = names_gerencias, # Objeto global de global.R
        selected = names_gerencias,
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3",
          selectAllText = "Seleccionar Todo", deselectAllText = "Quitar Todo",
          noneSelectedText = "Ninguna gerencia"
        )
      ),
      shinyWidgets::pickerInput(
        inputId = ns("pkr_str_sector"),
        label = "Elección de Sector",
        choices = c(), # Se llenará dinámicamente
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
        choices = choices_virtual_select, # Objeto global de global.R
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
        downloadButton(ns("descargar_sector"), "Descargar Vista en Excel", class = "btn-success")
    ),
    shinycssloaders::withSpinner(
      DT::DTOutput(outputId = ns("tabla_resumen_sector"))
    )
  )
}


# ------------------------------------
# --      Función del Servidor      --
# ------------------------------------
# Define la lógica del servidor para el módulo de Sectores.
mod_sectores_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    # --- LÓGICA REACTIVA ---
    # Este código es un copy-paste directo del server original.
    # Shiny se encarga de que input$pkr_gv_sector se refiera al input
    # correcto dentro del módulo.
    
    observeEvent(input$pkr_gv_sector, {
      
      if (is.null(input$pkr_gv_sector) || length(input$pkr_gv_sector) == 0) {
        sectores_filtrados <- character(0)
      } else {
        extracted_parts <- regmatches(input$pkr_gv_sector, gregexpr("GV \\d{2}", input$pkr_gv_sector))
        codigos_gv <- gsub(" ", "", unlist(extracted_parts))
        
        if (length(codigos_gv) == 0) {
          sectores_filtrados <- character(0)
        } else {
          patron_busqueda <- paste0("(", paste(codigos_gv, collapse = "|"), ")$")
          sectores_filtrados <- names_sectores[grepl(patron_busqueda, names_sectores)]
        }
      }
      
      # IMPORTANTE: Al usar funciones 'update*', el inputId se pasa sin el
      # namespace ns(). La 'session' del módulo se encarga de dirigirlo
      # al input correcto.
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "pkr_str_sector", # Sin ns()
        label = "Elección de Sector",
        choices = sectores_filtrados,
        selected = sectores_filtrados
      )
      
    }, ignoreNULL = FALSE)
    
    
    datos_filtrados_sector <- reactive({
      req(input$pkr_str_sector)
      resumen_final %>%
        filter(Sector %in% input$pkr_str_sector)
    })
    
    # --- RENDERIZADO DE LA TABLA ---
    output$tabla_resumen_sector <- DT::renderDT({
      cols_seleccionadas <- input$columnas_seleccionadas
      req(cols_seleccionadas)
      
      datos_mostrados <- datos_filtrados_sector() %>%
        dplyr::select(
          `Código Sector`, `Sector`, `%Sect /GV`,
          all_of(cols_seleccionadas)
        )
      
      lookup_nombres <- setNames(
        names(unlist(estructura_columnas)), 
        unlist(estructura_columnas)
      )
      
      header_row_1 <- list(
        tags$th(rowspan = 2, 'Código'), 
        tags$th(rowspan = 2, 'Sector'), 
        tags$th(rowspan = 2, '% Part.')
      )
      header_row_2 <- list()
      
      for (grupo in names(estructura_columnas)) {
        columnas_del_grupo <- unlist(estructura_columnas[[grupo]], use.names = FALSE)
        seleccionadas_en_grupo <- intersect(columnas_del_grupo, cols_seleccionadas)
        num_seleccionadas <- length(seleccionadas_en_grupo)
        
        if (num_seleccionadas > 0) {
          header_row_1 <- append(header_row_1, list(tags$th(colspan = num_seleccionadas, class = 'dt-center', grupo)))
          etiquetas_metricas <- lapply(seleccionadas_en_grupo, function(id) tags$th(sub(".*\\.", "", lookup_nombres[id])))
          header_row_2 <- append(header_row_2, etiquetas_metricas)
        }
      }
      
      sketch <- tags$table(
        class = 'display',
        tags$thead(tags$tr(header_row_1), tags$tr(header_row_2))
      )
      
      dt <- datatable(
        datos_mostrados, container = sketch, rownames = FALSE, 
        options = list(
          pageLength = 15, scrollX = TRUE, scrollY = "600px", dom = 'Bfrtip',
          columnDefs = list(list(className = 'dt-center', targets = '_all'), list(targets = 1, className = 'dt-left')),
          fixedColumns = list(leftColumns = 2)
        )
      )
      
      cols_visibles <- names(datos_mostrados)
      cols_pct <- intersect(
        c("%Sect /GV", "Facturacion % Cumpl", "Disponibles % Cumpl", "Activas % Cumpl", 
          "Saldo % Cumpl", "Productividad % Cumpl", "% Actividad Real", "% Actividad Meta", 
          "% Actividad % Cumpl", "Inicios + Reinicios % Cumpl", "Recuperos % Cumpl", "% I3", "% I2"), 
        cols_visibles
      )
      cols_curr <- intersect(
        c('Facturación Meta', 'Facturación Real', 'Facturacion Faltan 95%', 
          'Facturacion Faltan 100%', 'Productividad Meta', 'Productividad Real'), 
        cols_visibles
      )
      
      if (length(cols_pct) > 0) { dt <- dt %>% formatPercentage(cols_pct, digits = 1) }
      if (length(cols_curr) > 0) { dt <- dt %>% formatCurrency(cols_curr, currency = "$", digits = 0) }
      
      if ("Facturacion % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Facturacion % Cumpl', backgroundColor = styleInterval(c(0.9, 1), c("#F8696B", "white", "#63BE7B"))) }
      if ("Activas % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Activas % Cumpl', backgroundColor = styleInterval(c(0.8, 1), c("#F8696B", "white", "#63BE7B"))) }
      if ("Disponibles % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Disponibles % Cumpl', backgroundColor = styleInterval(c(0.95, 1.05), c("#F8696B", "white", "#63BE7B"))) }
      if ("Productividad % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Productividad % Cumpl', backgroundColor = styleInterval(c(1), c("#F8696B", "#63BE7B"))) }
      if ("Inicios + Reinicios % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Inicios + Reinicios % Cumpl', backgroundColor = styleInterval(c(1), c("#F8696B", "#63BE7B"))) }
      
      dt
    })
    
    # --- LÓGICA DE DESCARGA ---
    output$descargar_sector <- downloadHandler(
      filename = function() {
        paste0("Reporte_Sectores_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        datos_filtrados <- datos_filtrados_sector()
        datos_a_descargar <- datos_filtrados %>%
          dplyr::select(
            `Código Sector`, `Sector`, `%Sect /GV`,
            all_of(input$columnas_seleccionadas)
          )
        writexl::write_xlsx(datos_a_descargar, path = file)
      }
    )
    
  })
}