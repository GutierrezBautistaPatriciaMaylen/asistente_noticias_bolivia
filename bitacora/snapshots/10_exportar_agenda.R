# Título: exportación de la red de agenda a JSON para el dashboard
library(tidyverse)
library(jsonlite)

red <- readRDS("datos/red_fuentes.rds")
ag <- red$agenda

if (is.null(ag) || nrow(ag$nodos) == 0) {
  agenda_web <- list(ok = FALSE)
} else {
  agenda_web <- list(
    ok = TRUE,
    n_historias = ag$n_historias,
    nodos = ag$nodos %>%
      transmute(id = medio, historias, primicias, pct_primicias,
                pr = round(pagerank, 4), hub = round(hub, 4), auth = round(authority, 4)),
    aristas = ag$aristas %>%
      transmute(from = seguidor, to = lider, n, rezago = rezago_mediano_h)
  )
}
write_json(agenda_web, "docs/data/agenda.json", auto_unbox = TRUE, na = "null", digits = 5)
message("Archivo generado: docs/data/agenda.json")