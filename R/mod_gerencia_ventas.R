# R/mod_gerencia_ventas.R

# ---------------------------------
# --      Función de la UI       --
# ---------------------------------
mod_gerencia_ventas_ui <- function(id) {
  ns <- NS(id)
  
  # Ya no necesitas definir las listas ni la función aquí dentro.
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Reportes de Gerencia",
      navset_pill(
        nav_panel(title = "Inactividad", value = "panel_inactividad"),
        nav_panel(title = "Operaciones", value = "panel_operaciones"),
        nav_panel(title = "Actividad", value = "panel_actividad"),
        nav_panel(title = "Productividad", value = "panel_productividad"),
        id = ns("gerencia_nav")
      )
    ),
    
    # --- Panel Inactividad ---
    conditionalPanel(
      condition = paste0("input['", ns("gerencia_nav"), "'] == 'panel_inactividad'"),
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0px;",
          h4("Reporte de Inactividad"),
          downloadButton(ns("descargar_inactividad"), "Descargar Reporte", class = "btn-success btn-sm")
      ),
      # Usas la variable global directmente
      crear_leyenda(leyenda_inactividad),
      shinycssloaders::withSpinner(DTOutput(ns("tabla_inactividad"))),
      tags$small(style = "color: #666; font-style: italic;",
                 "* Los colores se asignan según el desempeño relativo (cuartiles) del año en curso.")
    ),
    
    # --- Panel Operaciones ---
    conditionalPanel(
      condition = paste0("input['", ns("gerencia_nav"), "'] == 'panel_operaciones'"),
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0px;",
          h4("Disponibles, Saldos, Inicios y Recuperos"),
          downloadButton(ns("descargar_operaciones"), "Descargar Reporte", class = "btn-success btn-sm")
      ),
      crear_leyenda(leyenda_operaciones),
      shinycssloaders::withSpinner(DTOutput(ns("tabla_disponibles"))),
      tags$small(style = "color: #666; font-style: italic;",
                 "* Los colores se asignan según el desempeño relativo (cuartiles) del año en curso.")
    ),
    
    # --- Panel Actividad ---
    conditionalPanel(
      condition = paste0("input['", ns("gerencia_nav"), "'] == 'panel_actividad'"),
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0px;",
          h4("Reporte de Actividad"),
          downloadButton(ns("descargar_actividad"), "Descargar Reporte", class = "btn-success btn-sm")
      ),
      crear_leyenda(leyenda_actividad),
      shinycssloaders::withSpinner(DTOutput(ns("tabla_actividad"))),
      tags$small(style = "color: #666; font-style: italic;",
                 "* Los colores se asignan según el desempeño relativo (cuartiles) del año en curso.")
    ),
    
    # --- Panel Productividad ---
    conditionalPanel(
      condition = paste0("input['", ns("gerencia_nav"), "'] == 'panel_productividad'"),
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0px;",
          h4("Productividad y Facturación"),
          downloadButton(ns("descargar_productividad"), "Descargar Reporte", class = "btn-success btn-sm")
      ),
      crear_leyenda(leyenda_productividad),
      shinycssloaders::withSpinner(DTOutput(ns("tabla_productividad"))),
      tags$small(style = "color: #666; font-style: italic;",
                 "* Los colores se asignan según el desempeño relativo (cuartiles) del año en curso.")
    )
  )
}


# ------------------------------------
# --      Función del Servidor      --
# ------------------------------------
mod_gerencia_ventas_server <- function(id, datos, lista_gvs) {
  
  moduleServer(id, function(input, output, session) {
    
    # --- TABLA 1: INACTIVIDAD ---
    output$tabla_inactividad <- renderDT({
      d <- datos()
      req(d) 
      
      tabla_inactividad_display <- d$inactividad %>% 
        select(GV, Equipo, 
               Disp_Real_C7_1 = Disp_Real_C7, Inact_Real_3, Porc_Inact_3, 
               Disp_Real_C7_2 = Disp_Real_C7, Inact_2, Porc_Inact_2, 
               Disp_Real_C7_3 = Disp_Real_C7, Inact_1, Porc_Inact_1)
      
      estilo_borde <- '2px solid #666'
      
      sketch = withTags(table(class = 'display', 
                              thead(
                                tr(
                                  th(rowspan = 2, 'GV'), 
                                  th(rowspan = 2, 'Equipo', style = paste0("border-right:", estilo_borde)), 
                                  th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'Inact3%'), 
                                  th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'Inact2%'), 
                                  th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'Inact1%')
                                ), 
                                tr(
                                  th('Disp Real C7', style = paste0("border-left:", estilo_borde)), th('Inact Real 3'), th('%'), 
                                  th('Disp Real C7', style = paste0("border-left:", estilo_borde)), th('Inact 2'), th('%'), 
                                  th('Disp Real C7', style = paste0("border-left:", estilo_borde)), th('Inact 1'), th('%')
                                )
                              )
      ))
      
      # --- DEFINICIÓN DE COLORES ---
      # Verde, Amarillo, Naranja, Rojo
      clrs_semaforo <- c("#e2f5e8", "#fff9e3", "#ffdca8", "#f8ccd1")
      
      # --- CÁLCULO DE CORTES (Breaks) ---
      
      # 1. Porc_Inact_3 (Año actual: Min 6.2% - Max 12.1%)
      # Históricamente esto rondaba el 6%. El año actual está peor.
      # Verde: < 8% (Premia a los equipos que se acercan al histórico)
      # Amarillo: 8% - 9.5% (Por debajo de la media actual de 9.6%)
      # Naranja: 9.5% - 11% (Por encima de la media, alerta)
      # Rojo: > 11% (Casos críticos actuales)
      brks_inact3 <- c(0.08, 0.095, 0.11) 
      
      # 2. Porc_Inact_2 (Año actual: Min 9.7% - Max 15.2%)
      # Históricamente rondaba el 10%.
      # Verde: < 11.5% (Premia al cuartil superior actual)
      # Amarillo: 11.5% - 13.0% (Hasta la media actual)
      # Naranja: 13.0% - 14.0% (Tercer cuartil)
      # Rojo: > 14.0% (El 25% peor de la tabla actual)
      brks_inact2 <- c(0.115, 0.13, 0.14)
      
      # 3. Porc_Inact_1 (Año actual: Min 36% - Max 64%)
      # ANOMALÍA: Al no cerrar el año, estos valores son altísimos (Media 55% vs Histórico 14%).
      # No podemos usar históricos. Usamos distribución relativa actual.
      # Verde: < 45% (Solo para los equipos excepcionales este año)
      # Amarillo: 45% - 55% (Rango "Normal" hasta la media)
      # Naranja: 55% - 60% (Rango preocupante, hasta el 3er cuartil)
      # Rojo: > 60% (Los casos más extremos del año en curso)
      brks_inact1 <- c(0.45, 0.55, 0.60)
      
      datatable(tabla_inactividad_display, 
                container = sketch, 
                rownames = FALSE,
                extensions = 'FixedColumns',  
                options = list(
                  pageLength = 10, 
                  language = opciones_espanol,
                  columnDefs = list(list(orderable = FALSE, targets = c(4, 7))),
                  
                  scrollX = TRUE,              
                  scrollY = "600px",           
                  scrollCollapse = TRUE,       
                  fixedColumns = list(leftColumns = 2) 
                )) %>%
        formatStyle(
          columns = names(tabla_inactividad_display)[2],     # Seleccionamos la 2da columna (por nombre)
          `border-right` = estilo_borde # Aplicamos el borde derecho
        ) %>%
        formatRound(c('Disp_Real_C7_1', 'Inact_Real_3', 'Disp_Real_C7_2', 'Inact_2', 'Disp_Real_C7_3', 'Inact_1'), mark = ",", digits = 0) %>% 
        formatPercentage(c('Porc_Inact_3', 'Porc_Inact_2', 'Porc_Inact_1'), digits = 1) %>%
        
        # Aplicación de estilos con los nuevos cortes
        formatStyle('Porc_Inact_3', backgroundColor = styleInterval(brks_inact3, clrs_semaforo)) %>% 
        formatStyle('Porc_Inact_2', backgroundColor = styleInterval(brks_inact2, clrs_semaforo)) %>% 
        formatStyle('Porc_Inact_1', backgroundColor = styleInterval(brks_inact1, clrs_semaforo)) %>%
        
        formatStyle(c('Disp_Real_C7_1', 'Disp_Real_C7_2', 'Disp_Real_C7_3'), borderLeft = estilo_borde)
    })
    
    
    # --- TABLA 2: OPERACIONES ---
    output$tabla_disponibles <- renderDT({
      d <- datos()
      req(d)
      
      # Selección de columnas
      tabla_disponibles_ordenada <- d$disponibles[, c("GV", "Equipo",
                                                      "Disp_Meta", "Disp_Real", "Disp_Alcanz",
                                                      "Saldo_Meta", "Saldo_Real", "Saldo_Alcanz",
                                                      "Inicios_Meta", "Inicios_Real", "Inicios_Cump", "Inicios_vs_Disp",
                                                      "Recuperos_Meta", "Recuperos_Real", "Recuperos_Cump", "Recuperos_vs_Disp")]
      
      estilo_borde <- '2px solid #666'
      
      sketch_ops <- withTags(table(class = 'display',
                                   thead(
                                     tr(
                                       th(rowspan = 2, 'GV'),
                                       th(rowspan = 2, 'Equipo', style = paste0("border-right:", estilo_borde)), 
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'DISPONIBLES'),
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'SALDO'),
                                       th(colspan = 4, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'INICIOS + REINICIOS'),
                                       th(colspan = 4, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'RECUPEROS')
                                     ),
                                     tr(
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('% Cump'), th('% vs Disp'),
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('% Cump'), th('% vs Disp')
                                     )
                                   )
      ))
      
      # --- DEFINICIÓN DE COLORES (Escala Ascendente: Rojo -> Verde) ---
      # Rojo (Malo), Naranja (Regular), Amarillo (Bueno), Verde (Excelente)
      clrs_semaforo <- c("#f8ccd1", "#ffdca8", "#fff9e3", "#d4edda")

      # --- CORTES (BREAKS) ---
      
      # 1. Disp_Alcanz (Datos: 93% a 97%)
      # Escala exigente.
      # < 94% (Rojo), 94-95% (Naranja), 95-96% (Amarillo), > 96% (Verde)
      brks_disp <- c(0.94, 0.95, 0.96)
      
      # 2. Saldo_Alcanz (Datos: Mayoría negativos, Media -265%)
      # Como la mayoría pierde saldo, el corte es drástico en 0.
      # < 0% (Rojo: Decrecimiento), 0-50% (Naranja), 50-90% (Amarillo), > 90% (Verde)
      # Nota: Con tus datos actuales, casi todo saldrá rojo, lo cual es CORRECTO para alertar la caída de saldo.
      brks_saldo <- c(0, 0.50, 0.90)
      
      # 3. Inicios_Cump (Datos: Media 16%, Max 44%)
      # Adaptado al avance del año actual.
      # < 10% (Rojo), 10-20% (Naranja), 20-35% (Amarillo), > 35% (Verde)
      brks_inicios <- c(0.10, 0.20, 0.35)
      
      # 4. Recuperos_Cump (Datos: Media 25%, Max 53%)
      # Adaptado al avance del año actual.
      # < 15% (Rojo), 15-25% (Naranja), 25-40% (Amarillo), > 40% (Verde)
      brks_recuperos <- c(0.15, 0.25, 0.40)
      
      datatable(tabla_disponibles_ordenada, 
                container = sketch_ops, 
                rownames = FALSE, 
                extensions = 'FixedColumns', 
                options = list(
                  language = opciones_espanol, 
                  pageLength = 10, 
                  scrollX = TRUE,              
                  scrollY = "600px",           
                  scrollCollapse = TRUE,
                  fixedColumns = list(leftColumns = 2) 
                )) %>%
        formatStyle(
          columns = names(tabla_disponibles_ordenada)[2],     # Seleccionamos la 2da columna (por nombre)
          `border-right` = estilo_borde # Aplicamos el borde derecho
        ) %>%
        formatRound(c('Disp_Meta', 'Disp_Real', 'Saldo_Meta', 'Saldo_Real',
                      'Inicios_Meta', 'Inicios_Real', 'Recuperos_Meta', 'Recuperos_Real'), mark = ",", digits = 0) %>%
        formatPercentage(c('Disp_Alcanz', 'Saldo_Alcanz', 'Inicios_Cump', 'Inicios_vs_Disp', 'Recuperos_Cump', 'Recuperos_vs_Disp'), digits = 1) %>%
        
        # --- APLICACIÓN DE ESTILOS ---
        formatStyle('Disp_Alcanz', backgroundColor = styleInterval(brks_disp, clrs_semaforo)) %>%
        formatStyle('Saldo_Alcanz', backgroundColor = styleInterval(brks_saldo, clrs_semaforo)) %>%
        formatStyle('Inicios_Cump', backgroundColor = styleInterval(brks_inicios, clrs_semaforo)) %>%
        formatStyle('Recuperos_Cump', backgroundColor = styleInterval(brks_recuperos, clrs_semaforo)) %>%
        
        # Bordes de separación de grupos
        formatStyle(c('Disp_Meta', 'Saldo_Meta', 'Inicios_Meta', 'Recuperos_Meta'), borderLeft = estilo_borde)
    })
    
    
    # --- TABLA 3: ACTIVIDAD ---
    output$tabla_actividad <- renderDT({
      d <- datos()
      req(d)
      
      # Selección de variables
      tabla_actividad_display <- d$actividad %>% 
        select(GV, Equipo, 
               Actividad_Porc_Meta, Actividad_Porc_Real, Actividad_Porc_Alcanz, 
               Activas_Meta, Activas_Real, Activas_Alcanz, 
               Act_Frec_Porc_Meta, Act_Frec_Porc_Real, Act_Frec_Porc_Alcanz)
      
      estilo_borde <- '2px solid #666'
      
      sketch_act <- withTags(table(class = 'display', 
                                   thead(
                                     tr(
                                       th(rowspan = 2, 'GV'), 
                                       th(rowspan = 2, 'Equipo', style = paste0("border-right:", estilo_borde)), 
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), '% ACTIVIDAD'), 
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'ACTIVAS'), 
                                       th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), '% ACTIVIDAD FRECUENTE')
                                     ), 
                                     tr(
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                       th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.')
                                     )
                                   )
      ))
      
      # --- DEFINICIÓN DE COLORES ---
      # Rojo (Malo), Naranja (Regular), Amarillo (Bueno), Verde (Excelente)
      clrs_semaforo <- c("#f8ccd1", "#ffdca8", "#fff3cd", "#d4edda")
    
      
      # --- CÁLCULO DE CORTES (Based on Current Year Quartiles) ---
      
      # 1. Porcentajes de Actividad (Actividad_Porc_Alcanz y Act_Frec_Porc_Alcanz)
      # Rango actual: 0.15 a 0.61. Media ~0.30.
      # < 20% (Rojo: Cuartil inferior)
      # 20% - 28% (Naranja: Bajo la media)
      # 28% - 38% (Amarillo: Sobre la media)
      # > 38% (Verde: Top performers actuales Y años históricos)
      brks_porc_alcanz <- c(0.20, 0.28, 0.38)
      
      # 2. Conteo de Activas (Activas_Alcanz)
      # Rango actual ligeramente más bajo: Media 0.28.
      # < 18% (Rojo)
      # 18% - 25% (Naranja)
      # 25% - 35% (Amarillo)
      # > 35% (Verde)
      brks_activas_alcanz <- c(0.18, 0.25, 0.35)
      
      datatable(
        tabla_actividad_display, 
        container = sketch_act, 
        rownames = FALSE,
        extensions = 'FixedColumns', 
        options = list(
          language = opciones_espanol, 
          pageLength = 10, 
          scrollX = TRUE,              
          scrollY = "600px",           
          scrollCollapse = TRUE,
          fixedColumns = list(leftColumns = 2) 
        )
      ) %>%
        formatStyle(
          columns = names(tabla_actividad_display)[2],     # Seleccionamos la 2da columna (por nombre)
          `border-right` = estilo_borde # Aplicamos el borde derecho
        ) %>%
        formatRound(c('Activas_Meta', 'Activas_Real'), mark = ",", digits = 0) %>% 
        formatPercentage(c('Actividad_Porc_Meta', 'Actividad_Porc_Real', 'Actividad_Porc_Alcanz', 'Activas_Alcanz', 'Act_Frec_Porc_Meta', 'Act_Frec_Porc_Real', 'Act_Frec_Porc_Alcanz'), digits = 2) %>%
        
        # --- APLICACIÓN DE ESTILOS (Solo columnas *_Alcanz) ---
        
        # Columna 1: % Actividad Alcanzada
        formatStyle('Actividad_Porc_Alcanz', backgroundColor = styleInterval(brks_porc_alcanz, clrs_semaforo)) %>% 
        
        # Columna 2: Activas Alcanzada (Usa cortes ligeramente más bajos)
        formatStyle('Activas_Alcanz', backgroundColor = styleInterval(brks_activas_alcanz, clrs_semaforo)) %>%
        
        # Columna 3: % Actividad Frecuente Alcanzada
        formatStyle('Act_Frec_Porc_Alcanz', backgroundColor = styleInterval(brks_porc_alcanz, clrs_semaforo)) %>%
        
        # Bordes visuales
        formatStyle(c('Actividad_Porc_Meta', 'Activas_Meta', 'Act_Frec_Porc_Meta'), borderLeft = estilo_borde)
    })
    
    
    # --- TABLA 4: PRODUCTIVIDAD ---
    output$tabla_productividad <- renderDT({
      d <- datos()
      req(d)
      
      # Usamos d$productividad
      tabla_productividad_display <- d$productividad %>%
        mutate(Prod_Meta = "-", Prod_Alcanz = "-") %>%
        select(GV, Equipo, Prod_Meta, Prod_Dolar_Real, Prod_Alcanz, Fact_Meta, Fact_Real, Fact_Alcanz)
      
      estilo_borde <- '2px solid #666'
      
      sketch_prod <- withTags(table(class = 'display', 
                                    thead(
                                      tr(
                                        th(rowspan = 2, 'GV'), 
                                        th(rowspan = 2, 'Equipo', style = paste0("border-right:", estilo_borde)), 
                                        th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), '$ PRODUCTIVIDAD'), 
                                        th(colspan = 3, class = 'dt-center', style = paste0("border-left:", estilo_borde), 'FACTURACIÓN')
                                      ), 
                                      tr(
                                        th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.'),
                                        th('Meta', style = paste0("border-left:", estilo_borde)), th('Real'), th('Alcanz.')
                                      )
                                    )
      ))
      
      # --- DEFINICIÓN DE COLORES ---
      # Rojo (Crítico), Naranja (Bajo), Amarillo (Medio/Bueno), Verde (Top Performance)
      clrs_semaforo <- c("#f8ccd1", "#ffdca8", "#fff9e3", "#d4edda")
      
      # --- CORTES (Breaks) BASADOS EN EL AÑO EN CURSO ---
      # Min: 16% | Q1: 20% | Mediana: 30% | Q3: 34% | Max: 59%
      
      # 1. Rojo (< 20%): El 25% inferior de la tabla actual.
      # 2. Naranja (20% - 28%): Por debajo de la mediana actual.
      # 3. Amarillo (28% - 35%): Alrededor de la mediana y Q3.
      # 4. Verde (> 35%): Los equipos líderes del año (y todos los años históricos).
      brks_fact_alcanz <- c(0.20, 0.28, 0.35)
      
      datatable(tabla_productividad_display, 
                container = sketch_prod, 
                rownames = FALSE, 
                extensions = 'FixedColumns', 
                options = list(
                  language = opciones_espanol, 
                  pageLength = 10, 
                  scrollX = TRUE,              
                  scrollY = "600px",           
                  scrollCollapse = TRUE,
                  fixedColumns = list(leftColumns = 2) 
                )) %>%
        # AQUI ESTA LA MAGIA:
        formatStyle(
          columns = names(tabla_productividad_display)[2],     # Seleccionamos la 2da columna (por nombre)
          `border-right` = estilo_borde # Aplicamos el borde derecho
        ) %>%
        formatCurrency('Prod_Dolar_Real', currency = "$", digits = 2) %>%
        formatRound(c('Fact_Meta', 'Fact_Real'), mark = ",", digits = 0) %>%
        formatPercentage('Fact_Alcanz', digits = 1) %>%
        
        # --- APLICACIÓN DE ESTILO ---
        # Solo a la columna Fact_Alcanz
        formatStyle('Fact_Alcanz', backgroundColor = styleInterval(brks_fact_alcanz, clrs_semaforo)) %>%
        
        # Bordes
        formatStyle(c('Prod_Meta', 'Fact_Meta'), borderLeft = estilo_borde)
    })
    
    
    # --- LOGICA DE DESCARGA ---
    # Nota: Para descargar, también necesitamos acceder a los datos reactivos 'd'
    
    output$descargar_inactividad <- downloadHandler(
      filename = function() { paste0("Reporte_Inactividad_", Sys.Date(), ".xlsx") },
      content = function(file) { 
        req(datos()) # Asegurar datos
        writexl::write_xlsx(datos()$inactividad, path = file) 
      }
    )
    
    output$descargar_operaciones <- downloadHandler(
      filename = function() { paste0("Reporte_Operaciones_", Sys.Date(), ".xlsx") },
      content = function(file) { 
        req(datos())
        writexl::write_xlsx(datos()$disponibles, path = file) 
      }
    )
    
    output$descargar_actividad <- downloadHandler(
      filename = function() { paste0("Reporte_Actividad_", Sys.Date(), ".xlsx") },
      content = function(file) { 
        req(datos())
        writexl::write_xlsx(datos()$actividad, path = file) 
      }
    )
    
    output$descargar_productividad <- downloadHandler(
      filename = function() { paste0("Reporte_Productividad_", Sys.Date(), ".xlsx") },
      content = function(file) { 
        req(datos())
        writexl::write_xlsx(datos()$productividad, path = file) 
      }
    )
    
  })
}