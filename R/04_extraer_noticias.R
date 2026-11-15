# Título: carga de paquetes, configuración y lectura de datos previos
library(rvest)
library(httr)
library(tidyverse)
library(lubridate)
library(robotstxt)
source("R/config.R")

limite_por_medio <- 50   # tareas por medio en esta corrida (bájalo a 20 para el uso diario)
max_intentos <- 3

portadas <- read_csv("datos/portadas.csv",
                     col_types = cols(.default = col_character()),
                     show_col_types = FALSE)

archivo_noticias <- "datos/noticias.csv"
previas <- NULL
if (file.exists(archivo_noticias)) {
  previas <- read_csv(archivo_noticias,
                      col_types = cols(.default = col_character()),
                      show_col_types = FALSE)
  for (col in c("dominios_citados", "estado", "intentos")) {
    if (!col %in% names(previas)) previas[[col]] <- NA_character_
  }
}

# Título: funciones para buscar la fecha de publicación y el dominio de un enlace
buscar_fecha <- function(pag) {
  metas <- c("meta[property='article:published_time']",
             "meta[name='article:published_time']",
             "meta[itemprop='datePublished']",
             "meta[property='og:article:published_time']",
             "meta[name='date']",
             "meta[name='pubdate']",
             "meta[name='publish-date']",
             "meta[name='DC.date.issued']")
  for (m in metas) {
    f <- pag %>% html_element(m) %>% html_attr("content")
    if (!is.na(f)) return(f)
  }
  jsons <- pag %>% html_elements("script[type='application/ld+json']") %>% html_text()
  f <- str_match(jsons, '"datePublished"\\s*:\\s*"([^"]+)"')[, 2]
  f <- f[!is.na(f)]
  if (length(f) > 0) return(f[1])
  
  f <- pag %>% html_element("time") %>% html_attr("datetime")
  if (!is.na(f)) return(f)
  f <- pag %>% html_element("[itemprop='datePublished']") %>% html_attr("content")
  if (!is.na(f)) return(f)
  pag %>% html_element("time") %>% html_text(trim = TRUE)
}

fecha_desde_url <- function(url) {
  num <- str_match(url, "_(\\d{10})/?$")[, 2]
  if (is.na(num)) return(NA_character_)
  format(as.POSIXct(as.numeric(num), origin = "1970-01-01", tz = ZONA),
         "%Y-%m-%d %H:%M:%S")
}

dominio_de <- function(u) {
  str_to_lower(str_match(u, "^https?://(?:www\\.)?([^/:?#]+)")[, 2])
}

# Título: función que extrae fecha, texto y fuentes citadas de una noticia (con reintentos y robots.txt)
extraer_noticia <- function(url) {
  permitido <- tryCatch(robotstxt::paths_allowed(url, warn = FALSE),
                        error = function(e) TRUE)
  if (!isTRUE(all(permitido))) {
    return(tibble(fecha_publicacion = NA_character_, texto = NA_character_,
                  dominios_citados = NA_character_,
                  estado = "bloqueado por robots.txt"))
  }
  
  resp <- NULL
  for (intento in 1:3) {
    resp <- tryCatch(GET(url, user_agent(USER_AGENT), timeout(30)),
                     error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) < 400) break
    Sys.sleep(5)
  }
  if (is.null(resp) || status_code(resp) >= 400) stop("no se pudo descargar tras 3 intentos")
  
  pag <- read_html(content(resp, "text", encoding = "UTF-8"))
  
  fecha <- buscar_fecha(pag)
  if (length(fecha) == 0 || is.na(fecha)) fecha <- fecha_desde_url(url)
  
  parrafos <- pag %>% html_elements("article p") %>% html_text(trim = TRUE)
  usar_article <- length(parrafos) > 0
  if (!usar_article) parrafos <- pag %>% html_elements("p") %>% html_text(trim = TRUE)
  parrafos <- parrafos[nchar(parrafos) > 50]
  
  # fuentes citadas: enlaces externos dentro de los párrafos del cuerpo
  hrefs <- pag %>%
    html_elements(if (usar_article) "article p a" else "p a") %>%
    html_attr("href")
  hrefs <- hrefs[!is.na(hrefs) & str_detect(hrefs, "^https?://")]
  propio <- dominio_de(url)
  doms <- unique(na.omit(dominio_de(hrefs)))
  doms <- doms[!(doms == propio | endsWith(doms, paste0(".", propio)))]
  doms <- doms[!doms %in% dominios_ruido]
  
  tibble(fecha_publicacion = fecha,
         texto = str_squish(paste(parrafos, collapse = " ")),
         dominios_citados = paste(doms, collapse = ";"),
         estado = "ok")
}

# Título: selección de tareas (noticias nuevas primero, luego las pendientes de completar)
ya_vistas <- if (is.null(previas)) character(0) else previas$enlace

tareas_nuevas <- portadas %>%
  filter(!enlace %in% ya_vistas, !str_detect(enlace, rutas_prohibidas)) %>%
  transmute(medio, titulo, enlace, fecha_extraccion,
            prioridad = 1L, intentos_previos = 0L)

tareas_pendientes <- NULL
if (!is.null(previas)) {
  tareas_pendientes <- previas %>%
    mutate(intentos_n = replace_na(suppressWarnings(as.integer(intentos)), 0L)) %>%
    filter(is.na(estado) | (str_detect(estado, "^error") & intentos_n < max_intentos)) %>%
    transmute(medio, titulo, enlace, fecha_extraccion,
              prioridad = 2L, intentos_previos = intentos_n)
}

tareas <- bind_rows(tareas_nuevas, tareas_pendientes) %>%
  arrange(prioridad) %>%
  group_by(medio) %>%
  slice_head(n = limite_por_medio) %>%
  ungroup()

n_tareas <- nrow(tareas)
message("Tareas en esta corrida: ", n_tareas,
        " (nuevas: ", sum(tareas$prioridad == 1L),
        ", por completar: ", sum(tareas$prioridad == 2L), ")")

# Título: recorrido de las tareas con pausa y registro de errores
resultados <- vector("list", n_tareas)
for (i in seq_len(n_tareas)) {
  message(i, "/", n_tareas, " - ", tareas$medio[i])
  res <- tryCatch(
    extraer_noticia(tareas$enlace[i]),
    error = function(e) tibble(fecha_publicacion = NA_character_, texto = NA_character_,
                               dominios_citados = NA_character_,
                               estado = paste0("error: ", conditionMessage(e)))
  )
  res$intentos <- as.character(tareas$intentos_previos[i] + 1L)
  resultados[[i]] <- res
  pausa <- if (tareas$medio[i] %in% names(pausas)) pausas[[tareas$medio[i]]] else 5
  Sys.sleep(pausa)
}

nuevas <- NULL
if (n_tareas > 0) {
  nuevas <- bind_cols(select(tareas, medio, titulo, enlace, fecha_extraccion),
                      bind_rows(resultados))
}

# Título: unión con las noticias previas, conversión de fechas y guardado
reprocesadas <- if (is.null(nuevas)) character(0) else nuevas$enlace
base_previa <- if (is.null(previas)) NULL else filter(previas, !enlace %in% reprocesadas)

noticias <- bind_rows(base_previa, nuevas) %>%
  select(-any_of("fecha_hora")) %>%
  distinct(enlace, .keep_all = TRUE) %>%
  mutate(fecha_hora = coalesce(
    ymd_hms(fecha_publicacion, tz = ZONA, quiet = TRUE),
    ymd(fecha_publicacion, tz = ZONA, quiet = TRUE)
  ))

noticias %>%
  mutate(fecha_hora = format(fecha_hora, "%Y-%m-%d %H:%M:%S")) %>%
  write_excel_csv(archivo_noticias, na = "")

# Título: contadores de la corrida y verificación de calidad por medio
n_nuevas      <- sum(tareas$prioridad == 1L)
n_completadas <- sum(tareas$prioridad == 2L)
n_errores     <- if (is.null(nuevas)) 0L else sum(!nuevas$estado %in% "ok")
n_restantes   <- sum(is.na(noticias$estado))

resumen <- noticias %>%
  group_by(medio) %>%
  summarise(
    total = n(),
    con_fecha = sum(!is.na(fecha_hora)),
    con_texto = sum(nchar(texto) > 200, na.rm = TRUE),
    con_citas = sum(!is.na(dominios_citados) & nzchar(dominios_citados)),
    errores = sum(str_detect(estado, "^(error|bloqueado)") %in% TRUE)
  )
print(resumen)
message("Pendientes de completar tras esta corrida: ", n_restantes)
