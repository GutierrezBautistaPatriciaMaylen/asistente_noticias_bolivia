# Título: configuración central de medios, pausas y reglas del proyecto
medios <- tibble::tribble(
  ~medio, ~url, ~pausa, ~condiciones,
  "El Deber", "https://eldeber.com.bo", 3,
  "Permite el acceso; se evitan /buscar/, /service/ y otras rutas prohibidas",
  "Opinión", "https://www.opinion.com.bo", 3,
  "Se evitan /api, /comments, /ads y /admin",
  "Los Tiempos", "https://www.lostiempos.com", 10,
  "Pide 10 segundos entre peticiones; se respeta; se evitan /search/, /admin/ y /user/",
  "Brújula Digital", "https://www.brujuladigital.net", 3,
  "Permite búsqueda e indexación; pide no usar el contenido para entrenar IA, aquí solo se usa para un índice de búsqueda con enlace a la fuente",
  "Erbol", "https://www.erbol.com.bo", 10,
  "Pide 10 segundos entre peticiones; se respeta; se evitan /search/, /admin/ y /user/"
)

pausas <- setNames(medios$pausa, medios$medio)

rutas_prohibidas <- "/buscar/|/search/|/user/|/admin/|/comments|/comment/|/api|/service/|/ads/|/login/|/node/add/|/partido_detalle/|/portadas/|/te-puede-interesar/"

dominios_ruido <- c("doubleclick.net", "googlesyndication.com",
                    "googletagmanager.com", "google-analytics.com")

USER_AGENT <- "Proyecto academico UMSA - Mineria de Datos"
ZONA <- "America/La_Paz"