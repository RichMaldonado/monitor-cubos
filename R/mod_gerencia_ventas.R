# R/mod_gerencia_ventas.R

# ---------------------------------
# --      Función de la UI       --
# ---------------------------------
# Esta función crea la interfaz de usuario para nuestro módulo.
# Recibe un 'id' que Shiny usará para crear un "namespace" (espacio de nombres),
# asegurando que los IDs de este módulo no choquen con otros.

mod_gerencia_ventas_ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Reportes de Gerencia",
      navset_pill(
        nav_panel(title = "Inactividad", value = "panel_inactividad"),
        nav_panel(title = "Operaciones", value = "panel_operaciones"),
        nav_panel(title = "Actividad", value = "panel_actividad"),
        nav_panel(title = "Productividad", value = "panel_productividad"),
        # IMPORTANTE: El id de este navset_pill también debe usar el namespace.
        id = ns("gerencia_nav")
      )
    ),
    

    conditionalPanel(
      condition = paste0("input['", ns("gerencia_nav"), "'] == 'panel_inactividad'"),
      div(style = "display: flex; justify-content: space-between; align-items: center;",
          h4("Reporte de Inactividad"),
          # Todos los IDs de inputs/outputs deben ser envueltos en ns().
          downloadButton(ns("descargar_inactividad"), "Descargar Reporte", class = "btn-success")
      ),
      shinycssloaders::withSpinner(DTOutput(ns("tabla_inactividad")))
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("gerencia_nav"), "'] == 'panel_operaciones'"),
      div(style = "display: flex; justify-content: space-between; align-items: center;",
          h4("Disponibles, Saldos, Inicios y Recuperos"),
          downloadButton(ns("descargar_operaciones"), "Descargar Reporte", class = "btn-success")
      ),
      shinycssloaders::withSpinner(DTOutput(ns("tabla_disponibles")))
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("gerencia_nav"), "'] == 'panel_actividad'"),
      div(style = "display: flex; justify-content: space-between; align-items: center;",
          h4("Reporte de Actividad"),
          downloadButton(ns("descargar_actividad"), "Descargar Reporte", class = "btn-success")
      ),
      shinycssloaders::withSpinner(DTOutput(ns("tabla_actividad")))
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("gerencia_nav"), "'] == 'panel_productividad'"),
      div(style = "display: flex; justify-content: space-between; align-items: center;",
          h4("Productividad y Facturación"),
          downloadButton(ns("descargar_productividad"), "Descargar Reporte", class = "btn-success")
      ),
      shinycssloaders::withSpinner(DTOutput(ns("tabla_productividad")))
    )
  )
}


# ------------------------------------
# --      Función del Servidor      --
# ------------------------------------
# Esta función contiene la lógica del servidor para nuestro módulo.
# Se envuelve en `moduleServer()`. El código interno es casi idéntico
# al original, pero ahora está contenido y aislado.

mod_gerencia_ventas_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    

    output$tabla_inactividad <- renderDT({
      
      # 1. Selección de datos
      tabla_inactividad_display <- tabla_inactividad %>% 
        select(GV, Equipo, 
               Disp_Real_C7_1 = Disp_Real_C7, Inact_Real_3, Porc_Inact_3, 
               Disp_Real_C7_2 = Disp_Real_C7, Inact_2, Porc_Inact_2, 
               Disp_Real_C7_3 = Disp_Real_C7, Inact_1, Porc_Inact_1)
      
      # Definimos el estilo del borde (Gris oscuro, 2px de grosor)
      estilo_borde <- '2px solid #666'
      
      # 2. Sketch (Encabezado): Solo aplicamos bordes para alinear con el cuerpo
      sketch = withTags(table(class = 'display', 
                              thead(
                                tr(
                                  th(rowspan = 2, 'GV'), 
                                  # Borde derecho en Equipo para separar la info del equipo de los datos
                                  th(rowspan = 2, 'Equipo', style = paste0("border-right:", estilo_borde)), 
                                  
                                  # Encabezados de Grupos: Borde a la izquierda
                                  th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'Inact3%'), 
                                  th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'Inact2%'), 
                                  th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'Inact1%')
                                ), 
                                tr(
                                  # Sub-encabezados: La primera columna de cada grupo lleva borde izquierdo
                                  th('Disp Real C7', style = paste0("border-left:", estilo_borde)), th('Inact Real 3'), th('%'), 
                                  th('Disp Real C7', style = paste0("border-left:", estilo_borde)), th('Inact 2'), th('%'), 
                                  th('Disp Real C7', style = paste0("border-left:", estilo_borde)), th('Inact 1'), th('%')
                                )
                              )
      ))
      
      # Definición de colores para los semáforos (lógica original)
      brks_inact3 <- c(0.04, 0.06, 0.08); clrs_inact3 <- c("#d4edda", "#fff3cd", "#fd7e14", "#dc3545")
      brks_inact21 <- c(0.25, 0.35, 0.45); clrs_inact21 <- c("#d4edda", "#fff3cd", "#fd7e14", "#dc3545")
      
      # 3. Generación de la tabla
      datatable(tabla_inactividad_display, container = sketch, rownames = FALSE, 
                options = list(pageLength = 20, scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = c(4, 7))))) %>%
        
        # Formatos numéricos originales
        formatRound(c('Disp_Real_C7_1', 'Inact_Real_3', 'Disp_Real_C7_2', 'Inact_2', 'Disp_Real_C7_3', 'Inact_1'), mark = ",", digits = 0) %>% 
        formatPercentage(c('Porc_Inact_3', 'Porc_Inact_2', 'Porc_Inact_1'), digits = 1) %>%
        
        # Semáforos originales
        formatStyle('Porc_Inact_3', backgroundColor = styleInterval(brks_inact3, clrs_inact3)) %>% 
        formatStyle('Porc_Inact_2', backgroundColor = styleInterval(brks_inact21, clrs_inact21)) %>% 
        formatStyle('Porc_Inact_1', backgroundColor = styleInterval(brks_inact21, clrs_inact21)) %>%
        
        # --- NUEVO: SEPARACIÓN EN EL CUERPO ---
        # Aplicamos el borde izquierdo a las columnas donde inicia cada grupo
        formatStyle(c('Disp_Real_C7_1', 'Disp_Real_C7_2', 'Disp_Real_C7_3'), borderLeft = estilo_borde)
    })
    
    output$tabla_disponibles <- renderDT({
      
      # 1. Definir orden de columnas
      tabla_disponibles_ordenada <- tabla_disponibles[, c("GV", "Equipo",
                                                          "Disp_Meta", "Disp_Real", "Disp_Alcanz",
                                                          "Saldo_Meta", "Saldo_Real", "Saldo_Alcanz",
                                                          "Inicios_Meta", "Inicios_Real", "Inicios_Cump", "Inicios_vs_Disp",
                                                          "Recuperos_Meta", "Recuperos_Real", "Recuperos_Cump", "Recuperos_vs_Disp")]
      
      # 2. Definir Estilo de Borde
      estilo_borde <- '2px solid #666'
      
      # 3. Construir Sketch con bordes
      sketch_ops <- withTags(table(class = 'display',
                                   thead(
                                     tr(
                                       th(rowspan = 2, 'GV'),
                                       # Borde derecho en Equipo para separar metadatos de métricas
                                       th(rowspan = 2, 'Equipo', style = paste0("border-right:", estilo_borde)), 
                                       
                                       # Encabezados Superiores con Borde Izquierdo
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'DISPONIBLES'),
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'SALDO'),
                                       th(colspan = 4, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'INICIOS + REINICIOS'),
                                       th(colspan = 4, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'RECUPEROS')
                                     ),
                                     tr(
                                       # Desglosamos las columnas manualmente para aplicar estilos específicos
                                       
                                       # Grupo Disponibles
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                       
                                       # Grupo Saldo
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                       
                                       # Grupo Inicios
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('% Cump'), th('% vs Disp'),
                                       
                                       # Grupo Recuperos
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('% Cump'), th('% vs Disp')
                                     )
                                   )
      ))
      
      brks_alcanz <- c(0.99, 1); clrs_alcanz <- c("#f8d7da", "#fff3cd", "#d4edda")
      brks_cump <- c(0.1, 0.15, 0.25); clrs_cump <- c("#f8d7da", "#fff3cd", "#c8e6c9", "#d4edda")
      brks_vs_disp <- c(0.003, 0.005, 0.008); clrs_vs_disp <- c("#f8d7da", "#fff3cd", "#c8e6c9", "#d4edda")
      
      datatable(tabla_disponibles_ordenada, container = sketch_ops, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) %>%
        formatRound(c('Disp_Meta', 'Disp_Real',
                      'Saldo_Meta', 'Saldo_Real',
                      'Inicios_Meta', 'Inicios_Real',
                      'Recuperos_Meta', 'Recuperos_Real'), mark = ",", digits = 0) %>%
        formatPercentage(c('Disp_Alcanz', 'Saldo_Alcanz', 'Inicios_Cump', 'Inicios_vs_Disp', 'Recuperos_Cump', 'Recuperos_vs_Disp'), digits = 1) %>%
        
        # Formatos condicionales de colores (semáforos)
        formatStyle('Disp_Alcanz', backgroundColor = styleInterval(brks_alcanz, clrs_alcanz)) %>%
        formatStyle('Inicios_Cump', backgroundColor = styleInterval(brks_cump, clrs_cump)) %>%
        formatStyle('Inicios_vs_Disp', backgroundColor = styleInterval(brks_vs_disp, clrs_vs_disp)) %>%
        formatStyle('Recuperos_vs_Disp', backgroundColor = styleInterval(brks_vs_disp, clrs_vs_disp)) %>%
        
        # --- NUEVO: Separación Vertical en el Cuerpo ---
        # Aplicamos el borde izquierdo a la columna "Meta" de cada grupo
        formatStyle(c('Disp_Meta', 'Saldo_Meta', 'Inicios_Meta', 'Recuperos_Meta'), borderLeft = estilo_borde)
    })
    
    output$tabla_actividad <- renderDT({
      
      # 1. Selección de datos
      tabla_actividad_display <- tabla_actividad %>% 
        select(GV, Equipo, 
               Actividad_Porc_Meta, Actividad_Porc_Real, Actividad_Porc_Alcanz, 
               Activas_Meta, Activas_Real, Activas_Alcanz, 
               Act_Frec_Porc_Meta, Act_Frec_Porc_Real, Act_Frec_Porc_Alcanz)
      
      # 2. Definir Estilo de Borde
      estilo_borde <- '2px solid #666'
      
      # 3. Construcción del Sketch (Encabezado)
      sketch_act <- withTags(table(class = 'display', 
                                   thead(
                                     tr(
                                       th(rowspan = 2, 'GV'), 
                                       # Borde derecho para separar equipo de los datos
                                       th(rowspan = 2, 'Equipo', style = paste0("border-right:", estilo_borde)), 
                                       
                                       # Encabezados de grupo con borde izquierdo
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), '% ACTIVIDAD'), 
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'ACTIVAS'), 
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), '% ACTIVIDAD FRECUENTE')
                                     ), 
                                     tr(
                                       # Desglose manual para aplicar borde a la primera columna de cada grupo (Meta)
                                       
                                       # Grupo % Actividad
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                       
                                       # Grupo Activas
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                       
                                       # Grupo % Actividad Frecuente
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.')
                                     )
                                   )
      ))
      
      brks_act_alcanz <- c(0.20, 0.30, 0.40); clrs_act_alcanz <- c("#fd7e14", "#fff3cd", "#c8e6c9", "#d4edda")
      
      # 4. Generación de la tabla
      datatable(tabla_actividad_display, container = sketch_act, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) %>%
        
        # Formatos numéricos originales
        formatRound(c('Activas_Meta', 'Activas_Real'), mark = ",", digits = 0) %>% 
        formatPercentage(c('Actividad_Porc_Meta', 'Actividad_Porc_Real', 'Actividad_Porc_Alcanz', 'Activas_Alcanz', 'Act_Frec_Porc_Meta', 'Act_Frec_Porc_Real', 'Act_Frec_Porc_Alcanz'), digits = 2) %>%
        
        # Semáforos originales
        formatStyle('Actividad_Porc_Alcanz', backgroundColor = styleInterval(brks_act_alcanz, clrs_act_alcanz)) %>% 
        formatStyle('Activas_Alcanz', backgroundColor = styleInterval(brks_act_alcanz, clrs_act_alcanz)) %>%
        
        # --- NUEVO: Separación Vertical en el Cuerpo ---
        # Aplicamos el borde a las columnas 'Meta' donde inicia cada grupo
        formatStyle(c('Actividad_Porc_Meta', 'Activas_Meta', 'Act_Frec_Porc_Meta'), borderLeft = estilo_borde)
    })
    
    output$tabla_productividad <- renderDT({
      
      # 1. Preparación de datos
      tabla_productividad_display <- tabla_productividad %>%
        mutate(Prod_Meta = "-", Prod_Alcanz = "-") %>%
        select(GV, Equipo, Prod_Meta, Prod_Dolar_Real, Prod_Alcanz, Fact_Meta, Fact_Real, Fact_Alcanz)
      
      # 2. Definir Estilo de Borde
      estilo_borde <- '2px solid #666'
      
      # 3. Construcción del Sketch (Encabezado)
      sketch_prod <- withTags(table(class = 'display', 
                                    thead(
                                      tr(
                                        th(rowspan = 2, 'GV'), 
                                        # Borde derecho en Equipo
                                        th(rowspan = 2, 'Equipo', style = paste0("border-right:", estilo_borde)), 
                                        
                                        # Encabezados de grupo con borde izquierdo
                                        th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), '$ PRODUCTIVIDAD'), 
                                        th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'FACTURACIÓN')
                                      ), 
                                      tr(
                                        # Desglose manual para aplicar borde a la primera columna de cada grupo (Meta)
                                        
                                        # Grupo Productividad
                                        th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                        
                                        # Grupo Facturación
                                        th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.')
                                      )
                                    )
      ))
      
      brks_fact_alcanz <- c(0.25, 0.40, 0.50); clrs_fact_alcanz <- c("#fd7e14", "#fff3cd", "#c8e6c9", "#d4edda")
      
      # 4. Generación de la tabla
      datatable(tabla_productividad_display, container = sketch_prod, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE)) %>%
        
        # Formatos originales
        formatCurrency('Prod_Dolar_Real', currency = "$", digits = 2) %>%
        formatRound(c('Fact_Meta', 'Fact_Real'), mark = ",", digits = 0) %>%
        formatPercentage('Fact_Alcanz', digits = 1) %>%
        
        # Semáforo original
        formatStyle('Fact_Alcanz', backgroundColor = styleInterval(brks_fact_alcanz, clrs_fact_alcanz)) %>%
        
        # --- NUEVO: Separación Vertical en el Cuerpo ---
        # Aplicamos el borde a las columnas iniciales de cada grupo: Prod_Meta y Fact_Meta
        formatStyle(c('Prod_Meta', 'Fact_Meta'), borderLeft = estilo_borde)
    })
    
    # --- LÓGICA DE DESCARGA ---
    output$descargar_inactividad <- downloadHandler(
      filename = function() { paste0("Reporte_Inactividad_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(tabla_inactividad, path = file) }
    )
    
    output$descargar_operaciones <- downloadHandler(
      filename = function() { paste0("Reporte_Operaciones_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(tabla_disponibles, path = file) }
    )
    
    output$descargar_actividad <- downloadHandler(
      filename = function() { paste0("Reporte_Actividad_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(tabla_actividad, path = file) }
    )
    
    output$descargar_productividad <- downloadHandler(
      filename = function() { paste0("Reporte_Productividad_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(tabla_productividad, path = file) }
    )
    
  })
}