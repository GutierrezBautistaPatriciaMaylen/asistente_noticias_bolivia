# Título: carga de paquetes y del modelo de índice
library(tidyverse)
library(Matrix)
library(igraph)
source("R/config.R")
source("R/utils_texto.R")

modelo <- readRDS("datos/modelo.rds")
docs <- modelo$docs
vocab <- modelo$vocab
X <- modelo$X
etiquetas <- modelo$etiquetas

# Título: parámetros del grafo de similitud
umbral_similitud <- 0.30   # similitud mínima del coseno para conectar dos noticias (bájalo si salen pocas historias)
ventana_dias <- 7          # solo se conectan noticias publicadas con pocos días de diferencia

# Título: construcción del grafo de similitud entre noticias
S <- as.matrix(tcrossprod(X))
diag(S) <- 0
t_num <- as.numeric(docs$fecha_hora)
dif_dias <- abs(outer(t_num, t_num, "-")) / 86400
S[S < umbral_similitud | dif_dias > ventana_dias] <- 0

g <- graph_from_adjacency_matrix(S, mode = "undirected", weighted = TRUE, diag = FALSE)
if (ecount(g) == 0) stop("No hay conexiones entre noticias: baja umbral_similitud (por ejemplo a 0.25)")
message("Grafo de similitud: ", vcount(g), " noticias y ", ecount(g), " conexiones")

# Título: PageRank y detección de comunidades (historias)
docs$pagerank <- page_rank(g, weights = E(g)$weight)$vector
# Título: detección de comunidades con mayor resolución (historias más específicas)
com <- cluster_louvain(g, weights = E(g)$weight, resolution = 1.5)
docs$historia_cruda <- as.integer(membership(com))

# Título: función de resumen extractivo (oraciones cortas del inicio de la noticia)
resumen_extractivo <- function(texto, centroide, n = 2) {
  oraciones <- str_split(texto, "(?<=[.!?])\\s+")[[1]]
  oraciones <- oraciones[nchar(oraciones) >= 40 & nchar(oraciones) <= 300 & !str_detect(oraciones, "///")]
  oraciones <- head(oraciones, 6)
  if (length(oraciones) <= n) return(str_trunc(paste(oraciones, collapse = " "), 500))
  puntaje <- map_dbl(oraciones, function(o) {
    t <- tokenizar(o)
    t <- t[t %in% names(centroide)]
    if (length(t) == 0) return(0)
    sum(centroide[t]) / sqrt(length(t))
  })
  top <- sort(order(puntaje, decreasing = TRUE)[seq_len(n)])
  str_trunc(paste(oraciones[top], collapse = " "), 500)
}

# Título: tabla de historias (comunidades con al menos 2 noticias)
tam <- table(docs$historia_cruda)
validas <- as.integer(names(tam)[tam >= 2])

historias <- map_dfr(validas, function(h) {
  idx <- which(docs$historia_cruda == h)
  sub <- docs[idx, ]
  i_rep <- idx[which.max(docs$pagerank[idx])]
  cen <- Matrix::colMeans(X[idx, , drop = FALSE])
  top <- names(sort(cen, decreasing = TRUE))[1:5]
  tibble(
    historia_cruda = h,
    n_notas = length(idx),
    n_medios = n_distinct(sub$medio),
    medios = paste(sort(unique(sub$medio)), collapse = ", "),
    fecha_ini = min(sub$fecha_hora),
    fecha_fin = max(sub$fecha_hora),
    representante = docs$id[i_rep],
    titulo = docs$titulo[i_rep],
    palabras = paste(unname(etiquetas[top]), collapse = ", "),
    resumen = resumen_extractivo(docs$texto[i_rep], cen)
  )
})

if (nrow(historias) == 0) stop("No se formaron historias: baja umbral_similitud (por ejemplo a 0.25)")

historias <- historias %>%
  arrange(desc(n_medios), desc(n_notas), desc(fecha_fin)) %>%
  mutate(historia = row_number())

docs <- docs %>%
  left_join(select(historias, historia_cruda, historia), by = "historia_cruda")

saveRDS(list(docs = docs, historias = historias), "datos/historias.rds")

# Título: resumen de las historias más cubiertas
message("Historias detectadas: ", nrow(historias),
        " | cubiertas por 3 o más medios: ", sum(historias$n_medios >= 3))
historias %>%
  select(historia, n_medios, n_notas, titulo) %>%
  head(10) %>%
  print(width = 150)

