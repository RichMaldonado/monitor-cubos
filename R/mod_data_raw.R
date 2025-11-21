# R/mod_data_raw.R

# ---------------------------------
# --      Función de la UI       --
# ---------------------------------
mod_data_raw_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Explorador de Datos Crudos (Sin Procesar)", class = "mb-3"),
    
    # Usamos navset_card_tab para pestañas contenidas en una tarjeta
    bslib::navset_card_tab(
      height = "800px", # Altura fija para scroll
      full_screen = TRUE,
      
      # --- Pestaña 1: GV ---
      nav_panel(
        title = "Origen: Gerencia Ventas",
        div(style = "margin-bottom: 10px;",
            downloadButton(ns("descarga_raw_gv"), "Descargar Excel", class = "btn-sm btn-light")
        ),
        shinycssloaders::withSpinner(
          DT::DTOutput(outputId = ns("tabla_raw_gv"))
        )
      ),
      
      # --- Pestaña 2: Sectores ---
      nav_panel(
        title = "Origen: Sectores",
        div(style = "margin-bottom: 10px;",
            downloadButton(ns("descarga_raw_sector"), "Descargar Excel", class = "btn-sm btn-light")
        ),
        shinycssloaders::withSpinner(
          DT::DTOutput(outputId = ns("tabla_raw_sector"))
        )
      )
    )
  )
}

# ------------------------------------
# --      Función del Servidor      --
# ------------------------------------
mod_data_raw_server <- function(id, datos_gv, datos_sector) {
  moduleServer(id, function(input, output, session) {
    
    # --- Configuración común para tablas raw ---
    opts_raw <- list(
      scrollX = TRUE,
      scrollY = "600px",
      paging = TRUE,
      lengthMenu = c(20, 50, 100),
      pageLength = 20,
      dom = 'frtip', # Sin botones B aquí porque usamos el downloadHandler
      language = opciones_espanol # Variable global definida en global.R
    )
    
    # --- 1. Renderizar Tabla GV ---
    output$tabla_raw_gv <- DT::renderDT({
      req(datos_gv())
      datatable(
        datos_gv(), 
        options = opts_raw,
        rownames = FALSE,
        class = "display nowrap compact"
      )
    })
    
    # --- 2. Renderizar Tabla Sector ---
    output$tabla_raw_sector <- DT::renderDT({
      req(datos_sector())
      datatable(
        datos_sector(), 
        options = opts_raw,
        rownames = FALSE,
        class = "display nowrap compact"
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