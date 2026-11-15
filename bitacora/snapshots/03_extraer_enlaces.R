# Título: carga de paquetes, configuración y carpetas
library(rvest)
library(httr)
library(tidyverse)
source("R/config.R")
dir.create("datos", showWarnings = FALSE)
dir.create("bitacora", showWarnings = FALSE)

# Título: función que extrae titulares y enlaces de la portada de un medio
extraer_enlaces <- function(url, medio) {
  respuesta <- GET(url, user_agent(USER_AGENT), timeout(30))
  stop_for_status(respuesta)
  pagina <- read_html(content(respuesta, "text", encoding = "UTF-8"))
  dominio <- str_remove(url, "^https?://(www\\.)?")
  
  tibble(
    medio  = medio,
    titulo = pagina %>% html_elements("a") %>% html_text(trim = TRUE),
    enlace = pagina %>% html_elements("a") %>% html_attr("href")
  ) %>%
    mutate(titulo = str_squish(titulo)) %>%
    filter(nchar(titulo) > 40, !is.na(enlace)) %>%
    mutate(enlace = xml2::url_absolute(enlace, url)) %>%
    filter(str_detect(enlace, fixed(dominio)),
           !str_detect(enlace, rutas_prohibidas),
           !str_detect(titulo, "\\{\\{"),
           !str_detect(titulo, regex("^Radio FM", ignore_case = TRUE))) %>%
    distinct(enlace, .keep_all = TRUE) %>%
    mutate(fecha_extraccion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

# Título: recorrido de todos los medios con pausas y manejo de errores
resultados <- list()
bitacora <- tibble()

for (i in seq_len(nrow(medios))) {
  m <- medios[i, ]
  message("Extrayendo: ", m$medio)
  
  res <- tryCatch(
    extraer_enlaces(m$url, m$medio),
    error = function(e) {
      message("  ERROR en ", m$medio, ": ", conditionMessage(e))
      NULL
    }
  )
  
  resultados[[i]] <- res
  bitacora <- bind_rows(bitacora, tibble(
    fecha = format(Sys.Date(), "%Y-%m-%d"),
    hora = format(Sys.time(), "%H:%M:%S"),
    medio = m$medio,
    noticias_encontradas = if (is.null(res)) 0L else nrow(res),
    estado = if (is.null(res)) "ERROR" else if (nrow(res) == 0) "VACIO" else "OK"
  ))
  
  Sys.sleep(m$pausa)
}

# Título: unión de portadas y guardado
portadas <- bind_rows(resultados)
write_excel_csv(portadas, "datos/portadas.csv")
print(bitacora)
