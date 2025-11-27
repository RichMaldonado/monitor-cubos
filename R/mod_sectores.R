# R/mod_sectores.R

# ==============================================================================
# 1. DEFINICIÓN DE LEYENDAS Y HELPER
# ==============================================================================

# --- CORRECCIÓN IMPORTANTE ---
# Cambiamos el orden: primero 'items', luego 'titulo' con un valor por defecto.
# Así, tus llamadas antiguas crear_leyenda(leyenda_inactividad) seguirán funcionando.
crear_leyenda <- function(items, titulo = "Referencias") {
  tags$div(
    style = "display: flex; flex-wrap: wrap; gap: 15px; margin-bottom: 5px; align-items: center; font-size: 0.8em; background-color: #f8f9fa; padding: 5px 10px; border-radius: 5px; border: 1px solid #dee2e6;",
    tags$strong(paste0(titulo, ":")), 
    lapply(items, function(item) {
      tags$div(
        style = "display: flex; align-items: center; gap: 5px;",
        tags$span(
          style = paste0(
            "width: 10px; height: 10px; display: inline-block; border-radius: 50%; border: 1px solid #999;", 
            "background-color:", item$color, ";"
          )
        ),
        tags$span(item$label)
      )
    })
  )
}

# Colores Estándar
c_critico <- "#f8ccd1" # Rojo
c_bajo    <- "#ffdca8" # Naranja
c_medio   <- "#fff3cd" # Amarillo
c_alto    <- "#d4edda" # Verde

# Definición de listas (Breaks coinciden con tu lógica)
leyenda_general <- list(
  list(color = c_critico, label = "< 20%"),
  list(color = c_bajo,    label = "20% - 28%"),
  list(color = c_medio,   label = "28% - 35%"),
  list(color = c_alto,    label = "> 35%")
)

leyenda_disp <- list(
  list(color = c_critico, label = "< 94%"),
  list(color = c_bajo,    label = "94% - 95%"),
  list(color = c_medio,   label = "95% - 96%"),
  list(color = c_alto,    label = "> 96%")
)

leyenda_inact <- list(
  list(color = c_alto,    label = "Óptimo (Bajo)"),
  list(color = c_medio,   label = "Regular"),
  list(color = c_bajo,    label = "Alerta"),
  list(color = c_critico, label = "Crítico (Alto)")
)

# ==============================================================================
# 2. UI
# ==============================================================================

mod_sectores_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    fillable = TRUE, 
    sidebar = sidebar(
      width = "25rem",
      title = "Filtros",
      
      shinyWidgets::pickerInput(
        inputId = ns("pkr_gv_sector"),
        label = "Elección de Gerencia de Venta",
        choices = NULL, 
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
        choices = NULL, 
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 5",
          selectAllText = "Seleccionar Todo", deselectAllText = "Quitar Todo",
          noneSelectedText = "Ningún sector"
        )
      ),
      shinyWidgets::pickerInput(
        inputId = ns("columnas_seleccionadas"),
        label = "Selecciona las métricas a mostrar:",
        choices = choices_virtual_select, 
        multiple = TRUE,
        selected = unlist(choices_virtual_select, use.names = FALSE),
        options = pickerOptions(
          size = 6,
          actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 5",
          selectAllText = "Seleccionar Todo", deselectAllText = "Quitar Todo",
          noneSelectedText = "Ningún sector"
        )
      )
    ),
    
    div(
      style = "display: flex; flex-direction: column; height: 100%; overflow: hidden;",
      
      # 1. Cabecera
      div(
        style = "flex: 0 0 auto; display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
        h4("Resumen General por Sector", style = "margin: 0;"),
        downloadButton(ns("descargar_sector"), "Descargar Excel", class = "btn-success btn-sm")
      ),
      
      # 2. Zona de Leyendas
      div(
        style = "flex: 0 0 auto;",
        # AQUI USAMOS EL NUEVO ORDEN: (ITEMS, TITULO)
        crear_leyenda(leyenda_general, "Cumplimiento (Fact/Prod/Act)"),
        div(style = "display: flex; gap: 10px;", 
            div(style="flex: 1;", crear_leyenda(leyenda_disp, "Disponibilidad")),
            div(style="flex: 1;", crear_leyenda(leyenda_inact, "Inactividad"))
        )
      ),
      
      # 3. Tabla con Scroll Interno
      div(
        style = "flex: 1 1 auto; overflow: hidden; position: relative;",
        shinycssloaders::withSpinner(
          DT::DTOutput(outputId = ns("tabla_resumen_sector"), height = "100%")
        )
      )
    )
  )
}


# ==============================================================================
# 3. SERVIDOR 
# ==============================================================================

mod_sectores_server <- function(id, datos, lista_gvs) {
  
  moduleServer(id, function(input, output, session) {
    
    # --- 1. Inicialización ---
    observeEvent(datos(), {
      req(datos())
      df <- datos()
      gvs_actuales <- if(is.reactive(lista_gvs)) lista_gvs() else lista_gvs
      
      shinyWidgets::updatePickerInput(session = session, inputId = "pkr_gv_sector",
                                      choices = gvs_actuales, selected = gvs_actuales)
      
      todos_sectores <- sort(unique(df$Sector))
      shinyWidgets::updatePickerInput(session = session, inputId = "pkr_str_sector", 
                                      choices = todos_sectores, selected = todos_sectores)
    })
    
    # --- 2. Filtros Cascada ---
    observeEvent(input$pkr_gv_sector, {
      req(datos())
      df <- datos()
      todos_sectores <- unique(df$Sector)
      
      if (is.null(input$pkr_gv_sector) || length(input$pkr_gv_sector) == 0) {
        sectores_filtrados <- character(0) 
      } else {
        numeros_gv <- stringr::str_extract(input$pkr_gv_sector, "[0-9]+")
        numeros_gv <- unique(numeros_gv[!is.na(numeros_gv)])
        if (length(numeros_gv) == 0) {
          sectores_filtrados <- character(0)
        } else {
          patron <- paste0("GV\\s?(", paste(numeros_gv, collapse = "|"), ")")
          sectores_filtrados <- grep(patron, todos_sectores, value = TRUE)
        }
      }
      shinyWidgets::updatePickerInput(session = session, inputId = "pkr_str_sector",
                                      choices = sort(unique(sectores_filtrados)),
                                      selected = sort(unique(sectores_filtrados)))
    }, ignoreNULL = FALSE)
    
    # --- 3. Datos ---
    datos_filtrados_sector <- reactive({
      req(datos(), input$pkr_str_sector)
      df <- datos() %>% dplyr::filter(Sector %in% input$pkr_str_sector)
      names(df) <- gsub("Facturación", "Facturación", names(df)) 
      return(df)
    })
    
    # --- 4. Cols Expandidas ---
    columnas_expandidas <- reactive({
      req(input$columnas_seleccionadas, datos())
      df_names <- names(datos())
      cols_finales <- c()
      
      for (grupo in input$columnas_seleccionadas) {
        cols_encontradas <- c()
        if (grupo == "Facturación") {
          cols_encontradas <- grep("Facturación", df_names, value = TRUE)
        } else if (grupo == "Indisponibles") {
          cols_encontradas <- grep("Indisponibles|I4|I5|I6", df_names, value = TRUE)
        } else if (grupo == "Disponibles") {
          todos_disponibles <- grep("Disponibles", df_names, value = TRUE)
          cols_encontradas <- todos_disponibles[!grepl("Indisponibles", todos_disponibles)]
        } else if (grupo == "Inact 3") {
          cols_encontradas <- grep("Inact 3|% I3", df_names, value = TRUE)
        } else if (grupo == "Inact 2") {
          cols_encontradas <- grep("Inact 2|% I2", df_names, value = TRUE)
        } else {
          tryCatch({
            cols_encontradas <- grep(grupo, df_names, value = TRUE, ignore.case = TRUE)
          }, error = function(e) return(NULL))
        }
        cols_finales <- c(cols_finales, cols_encontradas)
      }
      return(cols_finales)
    })
    
    # --- 5. Render DT ---
    output$tabla_resumen_sector <- DT::renderDT({
      req(datos_filtrados_sector(), input$columnas_seleccionadas)
      df_base <- datos_filtrados_sector()
      if (nrow(df_base) == 0) return(NULL)
      names_disponibles <- names(df_base)
      estilo_borde <- '2px solid #666'
      
      header_row_1 <- list(tags$th(rowspan = 2, 'Código'), tags$th(rowspan = 2, 'Sector', style = paste0("border-right:", estilo_borde)))
      header_row_2 <- list()
      cols_a_mostrar <- c()       
      cols_inicio_grupo <- c()    
      
      for (grupo in input$columnas_seleccionadas) {
        cols_del_grupo_actual <- c()
        if (grupo == "Facturación") {
          cols_del_grupo_actual <- grep("Facturación", names_disponibles, value = TRUE, fixed = TRUE)
        } else if (grupo == "Indisponibles") {
          cols_del_grupo_actual <- grep("Indisponibles|I4|I5|I6", names_disponibles, value = TRUE)
        } else if (grupo == "Disponibles") {
          matches <- grep("Disponibles", names_disponibles, value = TRUE)
          cols_del_grupo_actual <- matches[!grepl("Indisponibles", matches)]
        } else if (grupo == "Inact 3") {
          cols_del_grupo_actual <- grep("Inact 3|% I3", names_disponibles, value = TRUE)
        } else if (grupo == "Inact 2") {
          cols_del_grupo_actual <- grep("Inact 2|% I2", names_disponibles, value = TRUE)
        } else {
          cols_del_grupo_actual <- grep(grupo, names_disponibles, value = TRUE, ignore.case = TRUE)
        }
        
        if (length(cols_del_grupo_actual) > 0) {
          cols_a_mostrar <- c(cols_a_mostrar, cols_del_grupo_actual)
          header_row_1 <- append(header_row_1, list(tags$th(colspan = length(cols_del_grupo_actual), class = 'dt-center', style = paste0("border-left:", estilo_borde), grupo)))
          for (i in seq_along(cols_del_grupo_actual)) {
            col_full <- cols_del_grupo_actual[i]
            short <- gsub(grupo, "", col_full, ignore.case = TRUE)
            short <- gsub("Facturación", "", short); short <- gsub("Total", "", short); short <- trimws(short)
            if(short == "") short <- col_full 
            estilo <- if(i==1) paste0("border-left:", estilo_borde) else ""
            header_row_2 <- append(header_row_2, list(tags$th(style = estilo, short)))
            if(i==1) cols_inicio_grupo <- c(cols_inicio_grupo, col_full)
          }
        }
      }
      
      datos_mostrados <- df_base[, c("Código Sector", "Sector", cols_a_mostrar), drop = FALSE]
      sketch <- tags$table(class = 'display', tags$thead(tags$tr(header_row_1), tags$tr(header_row_2)))
      
      dt <- datatable(
        datos_mostrados, 
        container = sketch, 
        rownames = FALSE, 
        options = list(
          language = opciones_espanol,
          columnDefs = list(list(className = 'dt-center', targets = '_all'), list(targets = 1, className = 'dt-left')),
          extensions = 'FixedColumns', 
          scrollX = TRUE,
          # CALCULO DINÁMICO DEL ALTO
          scrollY = "calc(100vh - 280px)", 
          scrollCollapse = FALSE, 
          paging = FALSE, 
          fixedColumns = list(leftColumns = 2)
        )
      ) %>%
        # AQUI ESTA LA MAGIA:
        formatStyle(
          columns = names(datos_mostrados)[2],     # Seleccionamos la 2da columna (por nombre)
          `border-right` = estilo_borde # Aplicamos el borde derecho
        )
      
      cols_visibles <- names(datos_mostrados)
      
      cols_pct <- intersect(c("Facturación % Cumpl", "Disponibles % Cumpl", "Activas % Cumpl", "Saldo % Cumpl", "Productividad % Cumpl", "% Actividad Real", "% Actividad Meta", "% Actividad % Cumpl", "Inicios + Reinicios % Cumpl", "Recuperos % Cumpl", "% I3", "% I2"), cols_visibles)
      if (length(cols_pct) > 0) dt <- dt %>% formatPercentage(cols_pct, digits = 2)
      
      cols_curr <- intersect(c('Facturación Meta', 'Facturación Real', 'Facturación Faltan 95%', 'Facturación Faltan 100%', 'Productividad Meta', 'Productividad Real'), cols_visibles)
      if (length(cols_curr) > 0) dt <- dt %>% formatCurrency(cols_curr, currency = "$", digits = 0)
      
      cols_nums <- intersect(c("Disponibles Faltan 95%", "Disponibles Faltan 100%", "Activas Faltan 95%", "Activas Faltan 100%", "Saldo Faltan!", "Inicios + Reinicios Faltan!", "Recuperos Faltan!"), cols_visibles)
      if (length(cols_nums) > 0) dt <- dt %>% formatRound(cols_nums, digits = 0)
      
      clrs_std <- c(c_critico, c_bajo, c_medio, c_alto)
      clrs_inv <- c(c_alto, c_medio, c_bajo, c_critico) 
      
      brks_finan <- c(0.20, 0.28, 0.35)
      for(col in intersect(c("Facturación % Cumpl", "Productividad % Cumpl"), cols_visibles)) {
        dt <- dt %>% formatStyle(col, backgroundColor = styleInterval(brks_finan, clrs_std))
      }
      
      brks_disp <- c(0.94, 0.95, 0.96)
      if("Disponibles % Cumpl" %in% cols_visibles) {
        dt <- dt %>% formatStyle("Disponibles % Cumpl", backgroundColor = styleInterval(brks_disp, clrs_std))
      }
      
      brks_saldo <- c(0, 0.50, 0.90)
      if("Saldo % Cumpl" %in% cols_visibles) {
        dt <- dt %>% formatStyle("Saldo % Cumpl", backgroundColor = styleInterval(brks_saldo, clrs_std))
      }
      
      brks_ops <- c(0.15, 0.25, 0.40)
      for(col in intersect(c("Inicios + Reinicios % Cumpl", "Recuperos % Cumpl"), cols_visibles)) {
        dt <- dt %>% formatStyle(col, backgroundColor = styleInterval(brks_ops, clrs_std))
      }
      
      brks_act <- c(0.20, 0.28, 0.38)
      for(col in intersect(c("% Actividad % Cumpl", "Activas % Cumpl"), cols_visibles)) {
        dt <- dt %>% formatStyle(col, backgroundColor = styleInterval(brks_act, clrs_std))
      }
      
      if("% I3" %in% cols_visibles) {
        dt <- dt %>% formatStyle("% I3", backgroundColor = styleInterval(c(0.08, 0.095, 0.11), clrs_inv))
      }
      if("% I2" %in% cols_visibles) {
        dt <- dt %>% formatStyle("% I2", backgroundColor = styleInterval(c(0.115, 0.13, 0.14), clrs_inv))
      }
      
      cols_borde_final <- intersect(unique(cols_inicio_grupo), cols_visibles)
      if (length(cols_borde_final) > 0) {
        dt <- dt %>% formatStyle(cols_borde_final, borderLeft = estilo_borde)
      }
      
      return(dt)
    })
    
    output$descargar_sector <- downloadHandler(
      filename = function() { paste0("Reporte_Sectores_", Sys.Date(), ".xlsx") },
      content = function(file) {
        req(columnas_expandidas(), datos_filtrados_sector())
        df_export <- datos_filtrados_sector() %>% dplyr::select(`Código Sector`, `Sector`, dplyr::all_of(columnas_expandidas()))
        writexl::write_xlsx(df_export, path = file)
      }
    )
  })
}