# config_semaforos.R

# --- 1. FUNCIÓN HELPER UI (Se mantiene igual) ---
crear_leyenda <- function(items) {
  tags$div(
    style = "display: flex; flex-wrap: wrap; gap: 15px; margin-bottom: 10px; align-items: center; font-size: 0.85em; background-color: #f8f9fa; padding: 5px 10px; border-radius: 5px;",
    tags$strong("Semáforo:"), # Cambio sutil de "Referencias" a "Semáforo"
    lapply(items, function(item) {
      tags$div(
        style = "display: flex; align-items: center; gap: 5px;",
        tags$span(
          style = paste0(
            "width: 12px; height: 12px; display: inline-block; border-radius: 50%; border: 1px solid #999;", # Hice el círculo redondo para diferenciarlo
            "background-color:", item$color, ";"
          )
        ),
        tags$span(item$label)
      )
    })
  )
}

# --- 2. DEFINICIÓN DE LEYENDAS (Actualizadas a Lógica Relativa) ---

# Inactividad 
# (Aquí MENOS es MEJOR, por eso el verde es "Nivel Bajo")
leyenda_inactividad <- list(
  list(color = "#d4edda", label = "Nivel Óptimo (Baja Inactividad)"),
  list(color = "#fff3cd", label = "Nivel Promedio"),
  list(color = "#ffdca8", label = "Alerta (Sobre la media)"),
  list(color = "#f8ccd1", label = "Crítico (Alta Inactividad)") # Nota: Cambié el rojo a #f8d7da para coincidir con tus breaks
)

# Operaciones 
# (Mezcla Disponibilidad alta con Saldos negativos y Cumplimientos bajos)
leyenda_operaciones <- list(
  list(color = "#f8ccd1", label = "Crítico / Decrecimiento"), # Para Saldos negativos o cumplimiento muy bajo
  list(color = "#ffdca8", label = "Bajo Desempeño"),
  list(color = "#fff3cd", label = "Regular / En Media"),
  list(color = "#d4edda", label = "Meta Cumplida / Top Tier")
)

# Actividad 
# (Basado en Cuartiles del año actual: premia a los mejores del grupo)
leyenda_actividad <- list(
  list(color = "#f8ccd1", label = "Rezagado (Cuartil Inferior)"),
  list(color = "#ffdca8", label = "Bajo la Media"),
  list(color = "#fff3cd", label = "Sobre la Media"),
  list(color = "#d4edda", label = "Líderes (Cuartil Superior)")
)

# Productividad 
# (Igual que actividad, basado en desempeño relativo actual)
leyenda_productividad <- list(
  list(color = "#f8ccd1", label = "Riesgo (< Q1)"),
  list(color = "#ffdca8", label = "Bajo Desempeño"),
  list(color = "#fff3cd", label = "Desempeño Medio"),
  list(color = "#d4edda", label = "Alto Desempeño (> Q3)")
)




# ===========================
# SECTORES

# ==============================================================================
# 1. DEFINICIÓN DE LEYENDAS Y HELPER (ESTRATEGIA VISUAL)
# ==============================================================================

# Helper para renderizar los círculos de colores
crear_leyenda <- function(titulo, items) {
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

# Colores Estándar (Coincidentes con tu Server)
c_critico <- "#f8ccd1" # Rojo
c_bajo    <- "#ffdca8" # Naranja
c_medio   <- "#fff3cd" # Amarillo
c_alto    <- "#d4edda" # Verde

# Leyenda: Cumplimiento General (Facturación, Productividad, Actividad)
# Breaks basados en tu código: 20%, 28%, 35%
leyenda_general <- list(
  list(color = c_critico, label = "< 20%"),
  list(color = c_bajo,    label = "20% - 28%"),
  list(color = c_medio,   label = "28% - 35%"),
  list(color = c_alto,    label = "> 35%")
)

# Leyenda: Disponibilidad
# Breaks basados en tu código: 94%, 95%, 96%
leyenda_disp <- list(
  list(color = c_critico, label = "< 94%"),
  list(color = c_bajo,    label = "94% - 95%"),
  list(color = c_medio,   label = "95% - 96%"),
  list(color = c_alto,    label = "> 96%")
)

# Leyenda: Inactividad (Lógica Inversa: Verde es bajo)
# Breaks aproximados (Tu código tiene escalas distintas para I2 e I3, ponemos ref general)
leyenda_inact <- list(
  list(color = c_alto,    label = "Óptimo (Bajo)"),
  list(color = c_medio,   label = "Regular"),
  list(color = c_bajo,    label = "Alerta"),
  list(color = c_critico, label = "Crítico (Alto)")
)

















