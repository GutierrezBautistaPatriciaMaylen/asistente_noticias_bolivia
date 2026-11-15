# Título: carga de paquetes y lectura de la base de noticias
library(tidyverse)
library(lubridate)
library(Matrix)
library(SnowballC)
source("R/config.R")
source("R/utils_texto.R")

noticias <- read_csv("datos/noticias.csv",
                     col_types = cols(.default = col_character()),
                     show_col_types = FALSE)

docs <- noticias %>%
  mutate(fecha_hora = ymd_hms(fecha_hora, tz = ZONA, quiet = TRUE)) %>%
  filter(!is.na(fecha_hora), !is.na(texto), nchar(texto) > 200) %>%
  distinct(enlace, .keep_all = TRUE) %>%
  arrange(fecha_hora) %>%
  mutate(id = row_number() - 1L,                      # id desde 0 (para JavaScript)
         texto_busqueda = paste(titulo, titulo, texto))   # el título pesa el doble

# Título: tokenización con raíces y construcción del vocabulario
lista_formas <- map(docs$texto_busqueda, tokenizar, stem = FALSE)
lista_tokens <- map(lista_formas, ~ wordStem(.x, language = "spanish"))

tf_long <- tibble(doc = rep(seq_along(lista_tokens), lengths(lista_tokens)),
                  termino = unlist(lista_tokens)) %>%
  count(doc, termino, name = "n")

vocab <- sort(unique(tf_long$termino))

tf_long <- tf_long %>%
  group_by(doc) %>%
  mutate(tf = n / sum(n)) %>%
  ungroup() %>%
  mutate(j = match(termino, vocab))

N <- nrow(docs)
df_vec <- tabulate(tf_long$j, nbins = length(vocab))
idf <- log((1 + N) / (1 + df_vec)) + 1

# Título: matriz TF-IDF dispersa con filas normalizadas (similitud del coseno)
X <- sparseMatrix(i = tf_long$doc, j = tf_long$j,
                  x = tf_long$tf * idf[tf_long$j],
                  dims = c(N, length(vocab)))
norma <- sqrt(Matrix::rowSums(X^2)); norma[norma == 0] <- 1
X <- Diagonal(x = 1 / norma) %*% X
colnames(X) <- vocab

# Título: etiquetas legibles de cada raíz y mapa de formas a raíces (para el navegador)
formas <- tibble(forma = unlist(lista_formas)) %>%
  count(forma, name = "n") %>%
  mutate(raiz = wordStem(forma, language = "spanish"))

etiq <- formas %>%
  group_by(raiz) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()
etiquetas <- setNames(etiq$forma, etiq$raiz)

dif <- formas$forma != formas$raiz
mapa_stem <- setNames(formas$raiz[dif], formas$forma[dif])

# Título: mapa 2D de las noticias con PCA (calculado desde la matriz de distancias)
S <- as.matrix(tcrossprod(X))
D <- sqrt(pmax(2 * (1 - S), 0))
cm <- cmdscale(D, k = 2, eig = TRUE)
docs$x <- cm$points[, 1]
docs$y <- cm$points[, 2]
gof <- round(100 * cm$GOF[1], 1)
message("Los 2 ejes del mapa explican ", gof, "% de la variabilidad")

saveRDS(list(docs = docs, vocab = vocab, idf = idf, X = X, etiquetas = etiquetas,
             mapa_stem = mapa_stem, gof = gof),
        "datos/modelo.rds")

# Título: función de búsqueda por similitud del coseno (para pruebas en R)
buscar_noticias <- function(consulta, k = 5, filtro_medio = NULL) {
  tq <- tokenizar(consulta)
  tq <- tq[tq %in% vocab]
  if (length(tq) == 0) {
    message("Ninguna palabra de la consulta aparece en las noticias.")
    return(invisible(NULL))
  }
  tab <- table(tq)
  j <- match(names(tab), vocab)
  q <- numeric(length(vocab))
  q[j] <- (as.numeric(tab) / sum(tab)) * idf[j]
  q <- q / sqrt(sum(q^2))
  
  puntajes <- as.vector(X %*% q)
  res <- docs %>% mutate(puntaje = round(puntajes, 3)) %>% filter(puntaje > 0)
  if (!is.null(filtro_medio)) res <- res %>% filter(medio %in% filtro_medio)
  res %>%
    arrange(desc(puntaje)) %>%
    slice_head(n = k) %>%
    select(puntaje, medio, fecha_hora, titulo)
}

# Título: pruebas del buscador (nota que "incendios" e "incendio" ya dan lo mismo)
message("Noticias indexadas: ", N, " | raíces: ", length(vocab))
print(buscar_noticias("incendios chiquitania"), width = 150)
print(buscar_noticias("caso maletas"), width = 150)