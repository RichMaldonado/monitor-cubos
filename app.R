source("global.R")

theme_natura <- bslib::bs_theme(
  version = 5,
  
  preset = "simplex",
  
  primary = "#FF9526", secondary = "#2E2D2C", info = "#33AAFF",
  danger = "#A40025", success = "#105243", 
  
  bg = "#FFFFFF", fg = "#2E2D2C",
  base_font = bslib::font_google("Montserrat", local = FALSE),
  
  "navbar-bg" = "#00466E", 
  "navbar-light-color" = "#E4E7F0",
  "navbar-light-hover-color" = "#E4E7F0",
  "navbar-light-active-color" = "#E4E7F0",
  
  "link-color" = "#FF9526", "link-hover-color" = "#CF3100"
)



ui <- page_navbar(
  
  # 1. Título y Logo
  title = "Cómo Vamos Mx",
  
  # 2. Tema Natura
  theme = theme_natura,
  
  
  # 3. Pestaña "Gerencia de Ventas" con layout_sidebar
  nav_panel(
    title = "Gerencia de Ventas",
    layout_sidebar(
      sidebar = sidebar(
        title = "Reportes de Gerencia",
        navset_pill(
          nav_panel(title = "Inactividad", value = "panel_inactividad"),
          nav_panel(title = "Operaciones", value = "panel_operaciones"),
          nav_panel(title = "Actividad", value = "panel_actividad"),
          nav_panel(title = "Productividad", value = "panel_productividad"),
          id = "gerencia_nav"
        )
      ),
      
      conditionalPanel(
        condition = "input.gerencia_nav == 'panel_inactividad'",
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            h4("Reporte de Inactividad"),
            downloadButton("descargar_inactividad", "Descargar Reporte", class = "btn-success")
        ),
        shinycssloaders::withSpinner(DTOutput("tabla_inactividad"))
      ),
      
      conditionalPanel(
        condition = "input.gerencia_nav == 'panel_operaciones'",
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            h4("Disponibles, Saldos, Inicios y Recuperos"),
            downloadButton("descargar_operaciones", "Descargar Reporte", class = "btn-success")
        ),
        shinycssloaders::withSpinner(DTOutput("tabla_disponibles"))
      ),
      
      conditionalPanel(
        condition = "input.gerencia_nav == 'panel_actividad'",
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            h4("Reporte de Actividad"),
            downloadButton("descargar_actividad", "Descargar Reporte", class = "btn-success")
        ),
        shinycssloaders::withSpinner(DTOutput("tabla_actividad"))
      ),
      
      conditionalPanel(
        condition = "input.gerencia_nav == 'panel_productividad'",
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            h4("Productividad y Facturación"),
            downloadButton("descargar_productividad", "Descargar Reporte", class = "btn-success")
        ),
        shinycssloaders::withSpinner(DTOutput("tabla_productividad"))
      )
    )
  ),
  
  
  # 4. Pestaña "Sectores" con layout_sidebar (como antes)
  # -----------------------------------------------------
  nav_panel(
    title = "Sectores",
    layout_sidebar(
      sidebar = sidebar(
        width = "25rem",
        title = "Filtros",
        
        
        
        shinyWidgets::pickerInput(
          inputId = "pkr_gv_sector",
          label = "Elección de Gerencia de Venta",
          choices = names_gerencias,
          selected = names_gerencias,
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 3",
            selectAllText = "Seleccionar Todo", deselectAllText = "Quitar Todo",
            noneSelectedText = "Ninguna gerencia"
          )
        ),
        shinyWidgets::pickerInput(
          inputId = "pkr_str_sector",
          label = "Elección de Sector",
          choices = c(),
          multiple = TRUE,
          options = pickerOptions(
            actionsBox = TRUE, liveSearch = TRUE, selectedTextFormat = "count > 5",
            selectAllText = "Seleccionar Todo", deselectAllText = "Quitar Todo",
            noneSelectedText = "Ningún sector"
          )
        ),
        shinyWidgets::virtualSelectInput(
          inputId = "columnas_seleccionadas",
          label = "Selecciona las métricas a mostrar:",
          choices = choices_virtual_select, 
          multiple = TRUE,
          selected = unlist(choices_virtual_select, use.names = FALSE), # Seleccionar todo por defecto
          search = TRUE,
          showValueAsTags = F,
          placeholder = "Seleccionar métricas...",
          disableOptionGroupCheckbox = FALSE,
          optionsCount = 7
        )
      ),
      # h4("Resumen General por Sector"),
      div(style = "display: flex; justify-content: space-between; align-items: center;",
          h4("Resumen General por Sector"),
          downloadButton("descargar_sector", "Descargar Vista en Excel", class = "btn-success")
      ),
      shinycssloaders::withSpinner(
        DT::DTOutput(outputId = "tabla_resumen_sector")
      )
    )
  ),
  
  
  
  # 5. Espaciador y Fecha de Actualización
  # ----------------------------------------
  nav_spacer(), 
  nav_item(
    uiOutput("last_updated_display")
  )
  
  
  
  
)



# --- 3. Definir la Lógica del Servidor (Server) ---
server <- function(input, output, session) {
  
  
# Este bloque de código se ejecuta solo una vez cuando el usuario se conecta.
if (file.exists(ruta_json)) {
  data <- jsonlite::fromJSON(ruta_json)
  timestamp_str <- data$last_successful_update
  
  timestamp_val_fijo <- format(
    as.POSIXct(timestamp_str, tz = "UTC"), 
    "%d de %B de %Y, %H:%M:%S"
  )
} else {
  timestamp_val_fijo <- "No disponible" # Valor por defecto si no existe
}

# 2. RENDERIZAR LA UI USANDO EL VALOR FIJO
output$last_updated_display <- renderUI({
  
  tags$div(
    style = "font-size: 0.8em; color: #FFFFFF; text-align: right; padding-top: 10px;",
    icon("clock"), 
    "Última actualización:",
    tags$strong(paste("", timestamp_val_fijo)) 
  )
})
  
  
  
  observeEvent(input$pkr_gv_sector, {
    
    # Si no se selecciona ninguna gerencia, se vacían las opciones de sector.
    if (is.null(input$pkr_gv_sector) || length(input$pkr_gv_sector) == 0) {
      sectores_filtrados <- character(0)
    } else {
      
      # 1. Extraer los códigos de gerencia (ej: "GV 03", "GV 11") usando regex.
      #    "GV \\d{2}" busca el texto "GV ", seguido de exactamente dos dígitos.
      extracted_parts <- regmatches(input$pkr_gv_sector, gregexpr("GV \\d{2}", input$pkr_gv_sector))
      
      # 2. Quitar el espacio en blanco para que coincida con el formato de los sectores.
      #    (ej: "GV 03" se convierte en "GV03")
      #    unlist es necesario porque regmatches devuelve una lista.
      codigos_gv <- gsub(" ", "", unlist(extracted_parts))
      
      # Si por alguna razón no se encontraran códigos, evitar un error.
      if (length(codigos_gv) == 0) {
        sectores_filtrados <- character(0)
      } else {
        # 3. Crear el patrón de búsqueda que buscará cualquier sector que TERMINE ($)
        #    con alguno de los códigos seleccionados.
        patron_busqueda <- paste0("(", paste(codigos_gv, collapse = "|"), ")$")
        
        # 4. Filtrar el vector de nombres de sectores que coincidan con el patrón.
        sectores_filtrados <- names_sectores[grepl(patron_busqueda, names_sectores)]
      }
    }
    
    # 5. Actualizar las opciones del pickerInput de sectores con la lista filtrada.
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "pkr_str_sector",
      label = "Elección de Sector",
      choices = sectores_filtrados,
      selected = sectores_filtrados # Opcional: selecciona por defecto todos los sectores filtrados.
    )
    
  }, ignoreNULL = FALSE) 
  
  
  # PASO B: CONEXIÓN CON LA INTERFAZ
  # ===========================================================================
  
  output$tabla_inactividad <- renderDT({
    tabla_inactividad_display <- tabla_inactividad %>% select(GV, Equipo, Disp_Real_C7_1 = Disp_Real_C7, Inact_Real_3, Porc_Inact_3, Disp_Real_C7_2 = Disp_Real_C7, Inact_2, Porc_Inact_2, Disp_Real_C7_3 = Disp_Real_C7, Inact_1, Porc_Inact_1)
    sketch = withTags(table(class = 'display', thead(tr(th(rowspan = 2, 'GV'), th(rowspan = 2, 'Equipo'), th(colspan = 3, class = 'dt-center', 'Inact3%'), th(colspan = 3, class = 'dt-center', 'Inact2%'), th(colspan = 3, class = 'dt-center', 'Inact1%')), tr(th('Disp Real C7'), th('Inact Real 3'), th('%'), th('Disp Real C7'), th('Inact 2'), th('%'), th('Disp Real C7'), th('Inact 1'), th('%')))))
    brks_inact3 <- c(0.04, 0.06, 0.08); clrs_inact3 <- c("#d4edda", "#fff3cd", "#fd7e14", "#dc3545")
    brks_inact21 <- c(0.25, 0.35, 0.45); clrs_inact21 <- c("#d4edda", "#fff3cd", "#fd7e14", "#dc3545")
    datatable(tabla_inactividad_display, container = sketch, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = c(4, 7))))) %>%
      formatRound(c('Disp_Real_C7_1', 'Inact_Real_3', 'Disp_Real_C7_2', 'Inact_2', 'Disp_Real_C7_3', 'Inact_1'), mark = ",", digits = 0) %>% formatPercentage(c('Porc_Inact_3', 'Porc_Inact_2', 'Porc_Inact_1'), digits = 1) %>%
      formatStyle('Porc_Inact_3', backgroundColor = styleInterval(brks_inact3, clrs_inact3)) %>% formatStyle('Porc_Inact_2', backgroundColor = styleInterval(brks_inact21, clrs_inact21)) %>% formatStyle('Porc_Inact_1', backgroundColor = styleInterval(brks_inact21, clrs_inact21))
  })
  
  # output$tabla_disponibles <- renderDT({
  #   sketch_ops <- withTags(table(class = 'display', 
  #                                thead(tr(th(rowspan = 2, 'GV'), 
  #                                         th(rowspan = 2, 'Equipo'), 
  #                                         th(colspan = 3, class = 'dt-center', 'DISPONIBLES'), 
  #                                         th(colspan = 3, class = 'dt-center', 'SALDO'), 
  #                                         th(colspan = 4, class = 'dt-center', 'INICIOS + REINICIOS'), 
  #                                         th(colspan = 4, class = 'dt-center', 'RECUPEROS')), 
  #                                      tr(lapply(c('Meta', 'Real', 'Alcanz.', 'Meta', 'Real', 'Alcanz.', 'Meta', 'Real', '% Cump', '% vs Disp', 'Meta', 'Real', '% Cump', '% vs Disp'), th)))))
  #   
  #   brks_alcanz <- c(0.99, 1); clrs_alcanz <- c("#f8d7da", "#fff3cd", "#d4edda")
  #   brks_cump <- c(0.1, 0.15, 0.25); clrs_cump <- c("#f8d7da", "#fff3cd", "#c8e6c9", "#d4edda")
  #   brks_vs_disp <- c(0.003, 0.005, 0.008); clrs_vs_disp <- c("#f8d7da", "#fff3cd", "#c8e6c9", "#d4edda")
  #   
  #   
  #   datatable(tabla_disponibles, container = sketch_ops, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) %>%
  #     formatRound(c('Disp_Meta', 'Disp_Real', 
  #                   'Saldo_Meta', 'Saldo_Real', 
  #                   'Inicios_Meta', 'Inicios_Real', 
  #                   'Recuperos_Meta', 'Recuperos_Real'), mark = ",", digits = 0) %>% 
  #     formatPercentage(c('Disp_Alcanz', 'Saldo_Alcanz', 'Inicios_Cump', 'Inicios_vs_Disp', 'Recuperos_Cump', 'Recuperos_vs_Disp'), digits = 1) %>%
  #     formatStyle('Disp_Alcanz', backgroundColor = styleInterval(brks_alcanz, clrs_alcanz)) %>% 
  #     formatStyle('Inicios_Cump', backgroundColor = styleInterval(brks_cump, clrs_cump)) %>% 
  #     formatStyle('Inicios_vs_Disp', backgroundColor = styleInterval(brks_vs_disp, clrs_vs_disp)) %>% 
  #     formatStyle('Recuperos_vs_Disp', backgroundColor = styleInterval(brks_vs_disp, clrs_vs_disp))
  # })
  
  output$tabla_disponibles <- renderDT({
    sketch_ops <- withTags(table(class = 'display',
                                 thead(tr(th(rowspan = 2, 'GV'),
                                          th(rowspan = 2, 'Equipo'),
                                          th(colspan = 3, class = 'dt-center', 'DISPONIBLES'),
                                          th(colspan = 3, class = 'dt-center', 'SALDO'),
                                          th(colspan = 4, class = 'dt-center', 'INICIOS + REINICIOS'),
                                          th(colspan = 4, class = 'dt-center', 'RECUPEROS')),
                                       tr(lapply(c('Meta', 'Real', 'Alcanz.', 'Meta', 'Real', 'Alcanz.', 'Meta', 'Real', '% Cump', '% vs Disp', 'Meta', 'Real', '% Cump', '% vs Disp'), th)))))
    
    brks_alcanz <- c(0.99, 1); clrs_alcanz <- c("#f8d7da", "#fff3cd", "#d4edda")
    brks_cump <- c(0.1, 0.15, 0.25); clrs_cump <- c("#f8d7da", "#fff3cd", "#c8e6c9", "#d4edda")
    brks_vs_disp <- c(0.003, 0.005, 0.008); clrs_vs_disp <- c("#f8d7da", "#fff3cd", "#c8e6c9", "#d4edda")
    
    # REORDENAR LAS COLUMNAS DEL DATAFRAME
    # Asegúrate de que el orden de las columnas aquí coincida exactamente
    # con el orden visual que deseas en la tabla final.
    tabla_disponibles_ordenada <- tabla_disponibles[, c("GV", "Equipo",
                                                        "Disp_Meta", "Disp_Real", "Disp_Alcanz",
                                                        "Saldo_Meta", "Saldo_Real", "Saldo_Alcanz",
                                                        "Inicios_Meta", "Inicios_Real", "Inicios_Cump", "Inicios_vs_Disp",
                                                        "Recuperos_Meta", "Recuperos_Real", "Recuperos_Cump", "Recuperos_vs_Disp")]
    
    
    datatable(tabla_disponibles_ordenada, container = sketch_ops, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatRound(c('Disp_Meta', 'Disp_Real',
                    'Saldo_Meta', 'Saldo_Real',
                    'Inicios_Meta', 'Inicios_Real',
                    'Recuperos_Meta', 'Recuperos_Real'), mark = ",", digits = 0) %>%
      formatPercentage(c('Disp_Alcanz', 'Saldo_Alcanz', 'Inicios_Cump', 'Inicios_vs_Disp', 'Recuperos_Cump', 'Recuperos_vs_Disp'), digits = 1) %>%
      formatStyle('Disp_Alcanz', backgroundColor = styleInterval(brks_alcanz, clrs_alcanz)) %>%
      formatStyle('Inicios_Cump', backgroundColor = styleInterval(brks_cump, clrs_cump)) %>%
      formatStyle('Inicios_vs_Disp', backgroundColor = styleInterval(brks_vs_disp, clrs_vs_disp)) %>%
      formatStyle('Recuperos_vs_Disp', backgroundColor = styleInterval(brks_vs_disp, clrs_vs_disp))
  })
  
  output$tabla_actividad <- renderDT({
    tabla_actividad_display <- tabla_actividad %>% select(GV, Equipo, Actividad_Porc_Meta, Actividad_Porc_Real, Actividad_Porc_Alcanz, Activas_Meta, Activas_Real, Activas_Alcanz, Act_Frec_Porc_Meta, Act_Frec_Porc_Real, Act_Frec_Porc_Alcanz)
    sketch_act <- withTags(table(class = 'display', thead(tr(th(rowspan = 2, 'GV'), th(rowspan = 2, 'Equipo'), th(colspan = 3, class = 'dt-center', '% ACTIVIDAD'), th(colspan = 3, class = 'dt-center', 'ACTIVAS'), th(colspan = 3, class = 'dt-center', '% ACTIVIDAD FRECUENTE')), tr(lapply(rep(c('Meta', 'Real', 'Alcanz.'), 3), th)))))
    brks_act_alcanz <- c(0.20, 0.30, 0.40); clrs_act_alcanz <- c("#fd7e14", "#fff3cd", "#c8e6c9", "#d4edda")
    datatable(tabla_actividad_display, container = sketch_act, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatRound(c('Activas_Meta', 'Activas_Real'), mark = ",", digits = 0) %>% formatPercentage(c('Actividad_Porc_Meta', 'Actividad_Porc_Real', 'Actividad_Porc_Alcanz', 'Activas_Alcanz', 'Act_Frec_Porc_Meta', 'Act_Frec_Porc_Real', 'Act_Frec_Porc_Alcanz'), digits = 2) %>%
      formatStyle('Actividad_Porc_Alcanz', backgroundColor = styleInterval(brks_act_alcanz, clrs_act_alcanz)) %>% formatStyle('Activas_Alcanz', backgroundColor = styleInterval(brks_act_alcanz, clrs_act_alcanz))
  })
  
  output$tabla_productividad <- renderDT({
    tabla_productividad_display <- tabla_productividad %>%
      mutate(Prod_Meta = "-", Prod_Alcanz = "-") %>%
      select(GV, Equipo, Prod_Meta, Prod_Dolar_Real, Prod_Alcanz, Fact_Meta, Fact_Real, Fact_Alcanz)
    sketch_prod <- withTags(table(class = 'display', thead(tr(th(rowspan = 2, 'GV'), th(rowspan = 2, 'Equipo'), th(colspan = 3, class = 'dt-center', '$ PRODUCTIVIDAD'), th(colspan = 3, class = 'dt-center', 'FACTURACIÓN')), tr(lapply(rep(c('Meta', 'Real', 'Alcanz.'), 2), th)))))
    brks_fact_alcanz <- c(0.25, 0.40, 0.50); clrs_fact_alcanz <- c("#fd7e14", "#fff3cd", "#c8e6c9", "#d4edda")
    datatable(tabla_productividad_display, container = sketch_prod, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) %>%
      formatCurrency('Prod_Dolar_Real', currency = "$", digits = 2) %>%
      formatRound(c('Fact_Meta', 'Fact_Real'), mark = ",", digits = 0) %>%
      formatPercentage('Fact_Alcanz', digits = 1) %>%
      formatStyle('Fact_Alcanz', backgroundColor = styleInterval(brks_fact_alcanz, clrs_fact_alcanz))
  })
  
  
  

  
  datos_filtrados_sector <- reactive({
    
    req(input$pkr_str_sector)
    
    resumen_final %>%
      filter(Sector %in% input$pkr_str_sector)
    
  })
  
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
        # Usamos nuestro 'lookup' corregido para encontrar la etiqueta correcta
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
      #extensions = c('FixedColumns'),
      options = list(
        pageLength = 15, scrollX = TRUE, scrollY = "600px", dom = 'Bfrtip',
        columnDefs = list(list(className = 'dt-center', targets = '_all'), list(targets = 1, className = 'dt-left')),
        fixedColumns = list(leftColumns = 2)
      )
    )
    
    # Definimos las columnas a formatear usando los nombres reales del dataframe
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
    
    # Formato condicional con los nombres correctos
    if ("Facturacion % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Facturacion % Cumpl', backgroundColor = styleInterval(c(0.9, 1), c("#F8696B", "white", "#63BE7B"))) }
    if ("Activas % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Activas % Cumpl', backgroundColor = styleInterval(c(0.8, 1), c("#F8696B", "white", "#63BE7B"))) }
    if ("Disponibles % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Disponibles % Cumpl', backgroundColor = styleInterval(c(0.95, 1.05), c("#F8696B", "white", "#63BE7B"))) }
    if ("Productividad % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Productividad % Cumpl', backgroundColor = styleInterval(c(1), c("#F8696B", "#63BE7B"))) }
    if ("Inicios + Reinicios % Cumpl" %in% cols_visibles) { dt <- dt %>% formatStyle('Inicios + Reinicios % Cumpl', backgroundColor = styleInterval(c(1), c("#F8696B", "#63BE7B"))) }
    
    dt
  })
  
  # --- LÓGICA DE DESCARGA PARA LA PESTAÑA INACTIVIDAD ---
  output$descargar_inactividad <- downloadHandler(
    filename = function() {
      paste0("Reporte_Inactividad_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(tabla_inactividad, path = file)
    }
  )
  
  # --- LÓGICA DE DESCARGA PARA LA PESTAÑA OPERACIONES ---
  output$descargar_operaciones <- downloadHandler(
    filename = function() {
      paste0("Reporte_Operaciones_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(tabla_disponibles, path = file)
    }
  )
  
  # --- LÓGICA DE DESCARGA PARA LA PESTAÑA ACTIVIDAD ---
  output$descargar_actividad <- downloadHandler(
    filename = function() {
      paste0("Reporte_Actividad_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(tabla_actividad, path = file)
    }
  )
  
  # --- LÓGICA DE DESCARGA PARA LA PESTAÑA PRODUCTIVIDAD ---
  output$descargar_productividad <- downloadHandler(
    filename = function() {
      paste0("Reporte_Productividad_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(tabla_productividad, path = file)
    }
  )
  
  
  # --- LÓGICA DE DESCARGA PARA SECTORES ---
  output$descargar_sector <- downloadHandler(
    
    filename = function() {
      paste0("Reporte_Sectores_", Sys.Date(), ".xlsx")
    },
    
    content = function(file) {
      # 1. Tomar los datos ya filtrados por los inputs del usuario
      datos_filtrados <- datos_filtrados_sector()
      
      # 2. Asegurarnos de descargar solo las columnas que el usuario está viendo
      datos_a_descargar <- datos_filtrados %>%
        dplyr::select(
          `Código Sector`, `Sector`, `%Sect /GV`,
          all_of(input$columnas_seleccionadas)
        )
      
      # 3. Escribir los datos al archivo Excel
      writexl::write_xlsx(datos_a_descargar, path = file)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)