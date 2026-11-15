# Título: carga de paquetes y lectura de noticias e historias
library(tidyverse)
library(igraph)
source("R/config.R")

noticias <- read_csv("datos/noticias.csv",
                     col_types = cols(.default = col_character()),
                     show_col_types = FALSE)
res_hist <- readRDS("datos/historias.rds")
docs <- res_hist$docs

# Título: fuentes citadas dentro de las noticias
n_ok <- sum(noticias$estado %in% "ok")

citas <- noticias %>%
  filter(estado %in% "ok", !is.na(dominios_citados), nzchar(dominios_citados)) %>%
  select(medio, enlace, dominios_citados) %>%
  separate_rows(dominios_citados, sep = ";") %>%
  rename(fuente = dominios_citados) %>%
  filter(nzchar(fuente), !str_detect(fuente, "onelink\\.me$"))

pct_con_citas <- round(100 * n_distinct(citas$enlace) / max(n_ok, 1), 1)
message("Noticias con al menos una fuente citada: ", pct_con_citas, "%")

aristas <- citas %>% count(medio, fuente, name = "n")
n_aristas <- nrow(aristas)

dominios_medios <- str_remove(medios$url, "^https?://(www\\.)?")
patron_medios <- paste0("(^|\\.)", str_replace_all(dominios_medios, "\\.", "\\\\."), "$", collapse = "|")

categorizar_fuente <- function(dominio, tipo) {
  case_when(
    tipo == "medio" ~ "Medio",
    str_detect(dominio, patron_medios) ~ "Medio boliviano",
    str_detect(dominio, "(^|\\.)(x|twitter|facebook|instagram|youtube|tiktok|whatsapp|t)\\.(com|me)$|youtu\\.be$|wa\\.me$") ~ "Redes sociales",
    str_detect(dominio, "gob\\.bo$|gov$|gob\\.[a-z]{2}$|\\.gov\\.") ~ "Gobierno",
    TRUE ~ "Prensa y otros"
  )
}

# Título: grafo medio -> fuente citada con PageRank y HITS
nodos <- tibble()
if (n_aristas < 3) {
  message("Hay muy pocas citas externas; la red de fuentes quedará vacía.")
} else {
  g <- graph_from_data_frame(aristas %>% transmute(from = medio, to = fuente, n),
                             directed = TRUE)
  pr <- page_rank(g, directed = TRUE, weights = E(g)$n)$vector
  hs <- tryCatch(
    hits_scores(g, weights = E(g)$n),
    error = function(e) list(hub = hub_score(g, weights = E(g)$n)$vector,
                             authority = authority_score(g, weights = E(g)$n)$vector)
  )
  
  n_por_fuente <- aristas %>% group_by(fuente) %>% summarise(n = sum(n))
  n_por_medio <- citas %>% group_by(medio) %>% summarise(n = n_distinct(enlace))
  
  nodos <- tibble(id = V(g)$name,
                  pagerank = unname(pr[V(g)$name]),
                  hub = unname(hs$hub[V(g)$name]),
                  authority = unname(hs$authority[V(g)$name])) %>%
    mutate(tipo = if_else(id %in% medios$medio, "medio", "fuente"),
           categoria = categorizar_fuente(id, tipo)) %>%
    left_join(bind_rows(rename(n_por_medio, id = medio), rename(n_por_fuente, id = fuente)),
              by = "id")
  message("Red de fuentes: ", nrow(nodos), " nodos y ", n_aristas, " aristas")
}

# Título: pares de medios dentro de cada historia (quién publicó primero y quién lo siguió)
hist_docs <- docs %>% filter(!is.na(historia))

primeras <- hist_docs %>%
  group_by(historia, medio) %>%
  summarise(t = min(fecha_hora), .groups = "drop")

multi <- primeras %>%
  group_by(historia) %>%
  filter(n() >= 2) %>%
  ungroup()
n_hist_multi <- n_distinct(multi$historia)

pares <- suppressWarnings(
  inner_join(multi, multi, by = "historia", suffix = c("_lider", "_seguidor"))
) %>%
  filter(medio_lider != medio_seguidor, t_lider < t_seguidor) %>%
  mutate(rezago_h = as.numeric(difftime(t_seguidor, t_lider, units = "hours")))

aristas_agenda <- pares %>%
  group_by(seguidor = medio_seguidor, lider = medio_lider) %>%
  summarise(n = n_distinct(historia),
            rezago_mediano_h = round(median(rezago_h), 1),
            .groups = "drop")

participa <- multi %>% count(medio, name = "historias")
primicias <- multi %>%
  group_by(historia) %>%
  slice_min(t, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  count(medio, name = "primicias")

nodos_agenda <- tibble(medio = medios$medio) %>%
  left_join(participa, by = "medio") %>%
  left_join(primicias, by = "medio") %>%
  mutate(historias = replace_na(historias, 0L),
         primicias = replace_na(primicias, 0L),
         pct_primicias = if_else(historias > 0, round(100 * primicias / historias, 1), NA_real_),
         pagerank = NA_real_, hub = NA_real_, authority = NA_real_)

# Título: PageRank y HITS sobre la red de agenda (seguidor -> líder)
lider_agenda <- "sin datos"
if (nrow(aristas_agenda) > 0) {
  g2 <- graph_from_data_frame(aristas_agenda %>% transmute(from = seguidor, to = lider, n),
                              directed = TRUE,
                              vertices = tibble(name = medios$medio))
  pr2 <- page_rank(g2, directed = TRUE, weights = E(g2)$n)$vector
  hs2 <- tryCatch(
    hits_scores(g2, weights = E(g2)$n),
    error = function(e) list(hub = hub_score(g2, weights = E(g2)$n)$vector,
                             authority = authority_score(g2, weights = E(g2)$n)$vector)
  )
  nodos_agenda <- nodos_agenda %>%
    mutate(pagerank = unname(pr2[medio]),
           hub = unname(hs2$hub[medio]),
           authority = unname(hs2$authority[medio]))
  lider_agenda <- nodos_agenda$medio[which.max(nodos_agenda$authority)]
}

message("Historias con 2 o más medios: ", n_hist_multi, " | medio que marca la agenda: ", lider_agenda)
nodos_agenda %>% arrange(desc(authority)) %>% print(width = 150)

saveRDS(list(nodos = nodos, aristas = aristas, pct_con_citas = pct_con_citas, n_ok = n_ok,
             agenda = list(nodos = nodos_agenda, aristas = aristas_agenda,
                           n_historias = n_hist_multi)),
        "datos/red_fuentes.rds")