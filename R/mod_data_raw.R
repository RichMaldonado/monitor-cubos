# R/mod_data_raw.R

# ---------------------------------
# --      Función de la UI       --
# ---------------------------------
mod_data_raw_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Título general (opcional, si quieres mantener el título que tenía el card)
    h3("Explorador de Datos Crudos (Sin Procesar)"),

    # --- SECCIÓN 1: Gerencia Ventas ---
    div(
      # Título de la sección (antes era el título de la pestaña)
      h4("Origen: Gerencia Ventas", style = "color: #666;"),
      
      # Botón de descarga
      div(
        class = "pb-2",
        downloadButton(ns("descarga_raw_gv"), "Descargar Excel", class = "btn-sm btn-light")
      ),
      
      # Tabla
      # Quitamos height="100%" y fill=TRUE para que la tabla ocupe su espacio natural
      shinycssloaders::withSpinner(
        DT::DTOutput(outputId = ns("tabla_raw_gv"),height = "600px"),
        type = 4
      )
    ),
    
    # Separador visual entre las dos tablas
    # hr(style = "margin: 30px 0; border-top: 2px solid #eee;"),
    
    # --- SECCIÓN 2: Sectores ---
    div(
      h4("Origen: Sectores", style = "color: #666;"),
      
      div(
        class = "pb-2",
        downloadButton(ns("descarga_raw_sector"), "Descargar Excel", class = "btn-sm btn-light")
      ),
      
      shinycssloaders::withSpinner(
        DT::DTOutput(outputId = ns("tabla_raw_sector"),height = "600px"),
        type = 4
      )
    ),
    
    # Espacio final para que no quede pegado al fondo
    br(), br()
  )
}

# ------------------------------------
# --      Función del Servidor      --
# ------------------------------------
mod_data_raw_server <- function(id, datos_gv, datos_sector) {
  moduleServer(id, function(input, output, session) {
    
    # --- Configuración común ---
    # NOTA: Quitamos scrollY fijo porque usaremos fillContainer
    opts_raw <- list(
      scrollX = TRUE,
      paging = TRUE,
      lengthMenu = c(20, 50, 100),
      pageLength = 20,
      dom = 'frtip',
      language = opciones_espanol # Debe existir en global
    )
    
    # Opciones para fijar columnas (ya definidas antes)
    opts_fijas <- list(
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 2)
    )
    
    # Definimos el estilo de la línea divisoria para reutilizarlo
    # "2px solid #dcdcdc" crea una línea gris sólida de 2 pixeles
    estilo_borde <- "3px solid #666" 
    
    # --- 1. Renderizar Tabla GV ---
    output$tabla_raw_gv <- DT::renderDT({
      req(datos_gv())
      df <- datos_gv() # Guardamos el df para acceder a los nombres de columna
      
      datatable(
        df, 
        options = utils::modifyList(opts_raw, opts_fijas),
        rownames = FALSE,
        class = "display nowrap compact",
        fillContainer = TRUE,
        extensions = 'FixedColumns'
      ) %>%
        # AQUI ESTA LA MAGIA:
        formatStyle(
          columns = names(df)[2],     # Seleccionamos la 2da columna (por nombre)
          `border-right` = estilo_borde # Aplicamos el borde derecho
        )
    })
    
    # --- 2. Renderizar Tabla Sector ---
    output$tabla_raw_sector <- DT::renderDT({
      req(datos_sector())
      df <- datos_sector()
      
      datatable(
        df, 
        options = utils::modifyList(opts_raw, opts_fijas),
        rownames = FALSE,
        class = "display nowrap compact",
        fillContainer = TRUE,
        extensions = 'FixedColumns'
      ) %>%
        # Aplicamos la misma linea divisoria
        formatStyle(
          columns = names(df)[2],
          `border-right` = estilo_borde
        )
    })
    
    # --- 3. Descargas ---
    output$descarga_raw_gv <- downloadHandler(
      filename = function() { paste0("Raw_GV_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(datos_gv(), path = file) }
    )
    
    output$descarga_raw_sector <- downloadHandler(
      filename = function() { paste0("Raw_Sector_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(datos_sector(), path = file) }
    )
    
  })
}