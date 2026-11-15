# Título: carga de paquetes, modelo e historias
library(tidyverse)
library(lubridate)
library(Matrix)
library(SnowballC)
library(gtrendsR)
source("R/config.R")
source("R/utils_texto.R")

modelo <- readRDS("datos/modelo.rds")
res_hist <- readRDS("datos/historias.rds")
docs <- res_hist$docs
historias <- res_hist$historias
X <- modelo$X
vocab <- modelo$vocab
etiquetas <- modelo$etiquetas

# Título: elección de los temas desde las historias más cubiertas (o define los tuyos)
temas_manual <- NULL    # ejemplo: c("dólar", "incendio", "magisterio", "combustible", "maletas")
n_temas <- 5
excluir <- c("bolivia", "boliviano", "bolivianos", "gobierno", "pais", "presidente", "caso")

if (is.null(temas_manual)) {
  df_ratio <- Matrix::colSums(X > 0) / nrow(X)
  temas <- character(0)
  for (h in historias$historia) {        # las historias ya vienen ordenadas por cobertura
    idx <- which(docs$historia == h)
    cen <- Matrix::colMeans(X[idx, , drop = FALSE])
    cand <- names(sort(cen, decreasing = TRUE))
    cand <- cand[df_ratio[cand] < 0.25]            # descarta palabras demasiado comunes
    cand <- unname(etiquetas[cand])
    cand <- cand[!cand %in% c(excluir, temas)]
    if (length(cand) > 0) temas <- c(temas, cand[1])
    if (length(temas) >= n_temas) break
  }
} else {
  temas <- temas_manual
}
message("Temas: ", paste(temas, collapse = ", "))

# Título: consulta a Google Trends (Bolivia, último mes) con respaldo local
archivo_snapshot <- "datos/trends_snapshot.rds"
fuente_trends <- "no disponible"

trends <- tryCatch({
  Sys.sleep(2)
  res <- gtrends(keyword = temas, geo = "BO", time = "today 1-m")
  io <- res$interest_over_time
  if (is.null(io) || nrow(io) == 0) stop("Google Trends no devolvió datos")
  io %>%
    transmute(tema = keyword,
              fecha = as.Date(date),
              interes = suppressWarnings(as.numeric(ifelse(hits == "<1", "0.5", hits))))
}, error = function(e) {
  message("Google Trends falló: ", conditionMessage(e))
  NULL
})

if (!is.null(trends)) {
  fuente_trends <- "en vivo"
  saveRDS(list(trends = trends, temas = temas, generado = Sys.time()), archivo_snapshot)
} else if (file.exists(archivo_snapshot)) {
  snap <- readRDS(archivo_snapshot)
  if (setequal(snap$temas, temas)) {
    trends <- snap$trends
    fuente_trends <- paste0("respaldo del ", format(snap$generado, "%d/%m/%Y"))
    message("Se usa el respaldo de Google Trends: ", fuente_trends)
  }
}
trends_ok <- !is.null(trends)

# Título: cobertura de los medios como porcentaje de las noticias de cada día
docs <- docs %>% mutate(dia = as.Date(fecha_hora, tz = ZONA))
total_dia <- docs %>% count(dia, name = "n_total")

cobertura_n <- map_dfr(temas, function(t) {
  raiz <- wordStem(limpiar_texto(t), language = "spanish")
  j <- match(raiz, vocab)
  if (is.na(j)) return(tibble())
  tibble(tema = t, dia = docs$dia, presente = X[, j] > 0) %>%
    filter(presente) %>%
    count(tema, dia, name = "n")
})

dias <- seq(min(docs$dia), max(docs$dia), by = "day")
cobertura <- expand_grid(tema = temas, fecha = dias) %>%
  left_join(rename(cobertura_n, fecha = dia), by = c("tema", "fecha")) %>%
  left_join(rename(total_dia, fecha = dia), by = "fecha") %>%
  mutate(n = replace_na(n, 0L),
         n_total = replace_na(n_total, 0L),
         pct = if_else(n_total > 0, round(100 * n / n_total, 1), NA_real_))

# Título: correlación con Trends solo en días con cobertura suficiente (15 o más noticias)
min_notas_dia <- 15
dias_validos <- sum(total_dia$n_total >= min_notas_dia)
message("Días con cobertura suficiente para comparar: ", dias_validos,
        " (se necesitan 5 o más para calcular la correlación)")

correlaciones <- tibble(tema = temas, dias = 0L, correlacion = NA_real_)
if (trends_ok) {
  comb <- cobertura %>%
    filter(n_total >= min_notas_dia) %>%
    left_join(trends, by = c("tema", "fecha")) %>%
    filter(!is.na(interes), !is.na(pct))
  if (nrow(comb) > 0) {
    correlaciones <- comb %>%
      group_by(tema) %>%
      summarise(dias = n(),
                correlacion = if (n() >= 5 && sd(pct) > 0 && sd(interes) > 0) round(cor(pct, interes), 2) else NA_real_,
                .groups = "drop") %>%
      right_join(tibble(tema = temas), by = "tema")
  }
}
print(correlaciones)

saveRDS(list(temas = temas, trends = trends, cobertura = cobertura,
             correlaciones = correlaciones, fuente_trends = fuente_trends,
             dias_validos = dias_validos, min_notas_dia = min_notas_dia,
             generado = format(Sys.time(), "%d/%m/%Y %H:%M")),
        "datos/tendencias.rds")