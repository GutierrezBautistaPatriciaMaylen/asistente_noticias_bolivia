# Título: carga de paquetes y de los resultados de la minería
library(tidyverse)
library(lubridate)
library(jsonlite)
library(Matrix)
source("R/config.R")

modelo <- readRDS("datos/modelo.rds")
res_hist <- readRDS("datos/historias.rds")
red <- readRDS("datos/red_fuentes.rds")
tend <- readRDS("datos/tendencias.rds")
docs <- res_hist$docs
historias <- res_hist$historias

dir.create("docs/data", showWarnings = FALSE, recursive = TRUE)
file.create("docs/.nojekyll")

fecha_iso <- function(x) {
  ifelse(is.na(x), NA_character_, paste0(format(x, "%Y-%m-%dT%H:%M:%S", tz = ZONA), "-04:00"))
}
guardar_json <- function(obj, ruta) {
  write_json(obj, ruta, auto_unbox = TRUE, na = "null", digits = 5)
}

# Título: noticias con coordenadas del mapa, historia y PageRank
noticias_web <- docs %>%
  transmute(id, m = medio, f = fecha_iso(fecha_hora), t = titulo, u = enlace,
            r = str_trunc(texto, 300), h = historia,
            x = round(x, 4), y = round(y, 4), p = round(pagerank, 6))
guardar_json(noticias_web, "docs/data/noticias.json")

# Título: índice TF-IDF disperso para el buscador del navegador
Xt <- as(t(modelo$X), "CsparseMatrix")
vectores <- map(seq_len(ncol(Xt)), function(k) {
  n <- Xt@p[k + 1] - Xt@p[k]
  idx <- seq.int(Xt@p[k] + 1, length.out = n)
  list(i = I(Xt@i[idx]), w = I(round(Xt@x[idx], 4)))
})
indice <- list(vocab = I(modelo$vocab), idf = I(round(modelo$idf, 4)),
               mapa = as.list(modelo$mapa_stem), etiquetas = as.list(modelo$etiquetas),
               docs = vectores)
guardar_json(indice, "docs/data/indice.json")

# Título: historias con medios, resumen y noticias que las componen
historias_web <- map(seq_len(nrow(historias)), function(k) {
  h <- historias[k, ]
  list(id = h$historia, titulo = h$titulo, resumen = h$resumen,
       n = h$n_notas, nm = h$n_medios,
       medios = I(str_split(h$medios, ", ")[[1]]),
       fi = fecha_iso(h$fecha_ini), ff = fecha_iso(h$fecha_fin),
       palabras = I(str_split(h$palabras, ", ")[[1]]),
       rep = h$representante,
       notas = I(docs$id[which(docs$historia == h$historia)]))
})
guardar_json(historias_web, "docs/data/historias.json")

# Título: red de fuentes (los medios y las 60 fuentes con más autoridad)
if (nrow(red$nodos) == 0) {
  red_web <- list(ok = FALSE, pct_con_citas = red$pct_con_citas)
} else {
  top_fuentes <- red$nodos %>%
    filter(tipo == "fuente") %>%
    slice_max(authority, n = 60, with_ties = FALSE) %>%
    pull(id)
  nodos_web <- red$nodos %>%
    filter(tipo == "medio" | id %in% top_fuentes) %>%
    transmute(id, tipo, cat = categoria, n, pr = round(pagerank, 5),
              hub = round(hub, 4), auth = round(authority, 4))
  aristas_web <- red$aristas %>%
    filter(medio %in% nodos_web$id, fuente %in% nodos_web$id) %>%
    transmute(from = medio, to = fuente, n)
  red_web <- list(ok = TRUE, pct_con_citas = red$pct_con_citas,
                  total_fuentes = sum(red$nodos$tipo == "fuente"),
                  nodos = nodos_web, aristas = aristas_web)
}
guardar_json(red_web, "docs/data/red_fuentes.json")

# Título: tendencias de Google Trends y cobertura de los medios
tend_web <- list(
  ok = !is.null(tend$trends),
  fuente = tend$fuente_trends,
  generado = tend$generado,
  temas = I(tend$temas),
  trends = if (is.null(tend$trends)) list() else tend$trends,
  cobertura = if (is.null(tend$cobertura)) list() else tend$cobertura,
  correlaciones = tend$correlaciones
)
guardar_json(tend_web, "docs/data/tendencias.json")

# Título: metadatos generales (conteos, medios y condiciones de uso)
por_medio <- docs %>% count(medio, name = "n")
por_dia <- docs %>%
  mutate(dia = format(as.Date(fecha_hora, tz = ZONA), "%Y-%m-%d")) %>%
  count(dia, medio, name = "n")

meta <- list(
  generado = format(Sys.time(), "%d/%m/%Y %H:%M", tz = ZONA),
  n_docs = nrow(docs),
  n_historias = nrow(historias),
  gof = modelo$gof,
  por_medio = por_medio,
  por_dia = por_dia,
  medios = medios,
  temas = I(tend$temas)
)
guardar_json(meta, "docs/data/meta.json")

archivos <- list.files("docs/data", full.names = TRUE)
message("Archivos generados en docs/data:")
print(data.frame(archivo = basename(archivos), MB = round(file.size(archivos) / 1e6, 2)))
