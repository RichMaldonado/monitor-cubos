# R/mod_gerencia_ventas.R

# ---------------------------------
# --      Función de la UI       --
# ---------------------------------
# Esta función crea la interfaz de usuario para nuestro módulo.
# Recibe un 'id' que Shiny usará para crear un "namespace" (espacio de nombres),
# asegurando que los IDs de este módulo no choquen con otros.

mod_gerencia_ventas_ui <- function(id) {
  # ns() es una función auxiliar que añade el namespace del módulo al ID.
  # Por ejemplo, si id es "gerencia", ns("tabla_inactividad") se convierte en "gerencia-tabla_inactividad".
  ns <- NS(id)
  
  # El código aquí es un copy-paste directo del `nav_panel("Gerencia de Ventas", ...)`
  # del app.R original.
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
    
    # Cada conditionalPanel ahora debe leer el input con el namespace.
    # La condición cambia de "input.gerencia_nav == ..." a "input['id_del_modulo-gerencia_nav'] == ..."
    # Shiny maneja esta sintaxis de Javascript automáticamente.
    
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
    
    # Todo el código del servidor para las tablas y descargas de esta pestaña
    # se mueve aquí. Es un copy-paste directo.
    
    # Los objetos como `tabla_inactividad` están disponibles porque se crearon
    # en global.R, que se ejecuta antes que todo lo demás.
    
    output$tabla_inactividad <- renderDT({
      tabla_inactividad_display <- tabla_inactividad %>% select(GV, Equipo, Disp_Real_C7_1 = Disp_Real_C7, Inact_Real_3, Porc_Inact_3, Disp_Real_C7_2 = Disp_Real_C7, Inact_2, Porc_Inact_2, Disp_Real_C7_3 = Disp_Real_C7, Inact_1, Porc_Inact_1)
      sketch = withTags(table(class = 'display', thead(tr(th(rowspan = 2, 'GV'), th(rowspan = 2, 'Equipo'), th(colspan = 3, class = 'dt-center', 'Inact3%'), th(colspan = 3, class = 'dt-center', 'Inact2%'), th(colspan = 3, class = 'dt-center', 'Inact1%')), tr(th('Disp Real C7'), th('Inact Real 3'), th('%'), th('Disp Real C7'), th('Inact 2'), th('%'), th('Disp Real C7'), th('Inact 1'), th('%')))))
      brks_inact3 <- c(0.04, 0.06, 0.08); clrs_inact3 <- c("#d4edda", "#fff3cd", "#fd7e14", "#dc3545")
      brks_inact21 <- c(0.25, 0.35, 0.45); clrs_inact21 <- c("#d4edda", "#fff3cd", "#fd7e14", "#dc3545")
      datatable(tabla_inactividad_display, container = sketch, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = c(4, 7))))) %>%
        formatRound(c('Disp_Real_C7_1', 'Inact_Real_3', 'Disp_Real_C7_2', 'Inact_2', 'Disp_Real_C7_3', 'Inact_1'), mark = ",", digits = 0) %>% formatPercentage(c('Porc_Inact_3', 'Porc_Inact_2', 'Porc_Inact_1'), digits = 1) %>%
        formatStyle('Porc_Inact_3', backgroundColor = styleInterval(brks_inact3, clrs_inact3)) %>% formatStyle('Porc_Inact_2', backgroundColor = styleInterval(brks_inact21, clrs_inact21)) %>% formatStyle('Porc_Inact_1', backgroundColor = styleInterval(brks_inact21, clrs_inact21))
    })
    
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