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
    
    # --- 1. Renderizar Tabla GV ---
    output$tabla_raw_gv <- DT::renderDT({
      req(datos_gv())
      datatable(
        datos_gv(), 
        options = opts_raw,
        rownames = FALSE,
        class = "display nowrap compact",
        fillContainer = TRUE 
      )
    })
    
    # --- 2. Renderizar Tabla Sector ---
    output$tabla_raw_sector <- DT::renderDT({
      req(datos_sector())
      datatable(
        datos_sector(), 
        options = opts_raw,
        rownames = FALSE,
        class = "display nowrap compact",
        fillContainer = TRUE # <--- CLAVE: Ajusta la tabla al contenedor y activa scroll
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