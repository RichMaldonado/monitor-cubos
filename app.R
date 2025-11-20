# app.R - El Director de Orquesta

# --- 1. Cargar Lógica Global y Módulos ---
# Carga los datos, librerías y objetos pre-procesados.
source("global.R") 

# Carga las funciones UI y Server de nuestros módulos desde la carpeta R.
source("R/mod_gerencia_ventas.R")
source("R/mod_sectores.R")


# --- 2. Definir la Interfaz de Usuario (UI) ---
# La UI principal es ahora muy simple. Solo define la estructura
# y llama a las funciones UI de cada módulo.

ui <- page_navbar(
  
  # Título y Tema (definido en global.R)
  title = "Cómo Vamos Mx",
  theme = theme_natura,
  
  # Pestaña 1: Llama a la UI del módulo de Gerencia de Ventas.
  # Le asignamos un ID único: "gerencia_ventas".
  nav_panel(
    title = "Gerencia de Ventas",
    mod_gerencia_ventas_ui("gerencia_ventas")
  ),
  
  # Pestaña 2: Llama a la UI del módulo de Sectores.
  # Le asignamos otro ID único: "sectores".
  nav_panel(
    title = "Sectores",
    mod_sectores_ui("sectores")
  ),
  
  # Elementos globales de la UI que no pertenecen a ningún módulo.
  nav_spacer(), 
  nav_item(
    uiOutput("last_updated_display")
  )
)


# --- 3. Definir la Lógica del Servidor (Server) ---
# El servidor principal solo activa los servidores de cada módulo
# y maneja la lógica que es verdaderamente global.

server <- function(input, output, session) {
  
  # Activa el servidor del módulo de Gerencia de Ventas.
  # Es CRUCIAL que el ID aquí ("gerencia_ventas") sea el mismo que usamos en la UI.
  mod_gerencia_ventas_server("gerencia_ventas")
  
  # Activa el servidor del módulo de Sectores.
  mod_sectores_server("sectores")
  
  
  # Lógica para la fecha de actualización (este es un elemento global).
  # Este código se queda en el servidor principal.
  if (file.exists(ruta_json)) {
    data <- jsonlite::fromJSON(ruta_json)
    timestamp_str <- data$last_successful_update
    timestamp_val_fijo <- format(
      as.POSIXct(timestamp_str, tz = "UTC"), 
      "%d de %B de %Y, %H:%M:%S"
    )
  } else {
    timestamp_val_fijo <- "No disponible"
  }
  
  output$last_updated_display <- renderUI({
    tags$div(
      style = "font-size: 0.8em; color: #FFFFFF; text-align: right; padding-top: 10px;",
      icon("clock"), 
      "Última actualización:",
      tags$strong(paste("", timestamp_val_fijo)) 
    )
  })
  
}

# --- 4. Ejecutar la Aplicación ---
shinyApp(ui = ui, server = server)