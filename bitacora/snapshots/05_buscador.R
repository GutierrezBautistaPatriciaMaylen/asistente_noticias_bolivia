# Título: carga de paquetes y lectura de la base de noticias
library(tidyverse)
library(lubridate)

noticias <- read_csv("datos/noticias.csv",
                     col_types = cols(.default = col_character()),
                     show_col_types = FALSE)

# solo noticias con fecha y texto suficiente (descarta secciones y enlaces que no son noticias)
docs <- noticias %>%
  mutate(fecha_hora = ymd_hms(fecha_hora, tz = "America/La_Paz", quiet = TRUE)) %>%
  filter(!is.na(fecha_hora), !is.na(texto), nchar(texto) > 200) %>%
  distinct(enlace, .keep_all = TRUE) %>%
  mutate(texto_busqueda = paste(titulo, titulo, texto))   # el título pesa el doble

# Título: lista de palabras vacías y funciones de limpieza de texto
stopwords_es <- c("a","al","algo","algunas","algunos","ante","antes","como","con","contra",
                  "cual","cuando","de","del","desde","donde","durante","el","ella","ellas","ellos","en",
                  "entre","era","eran","es","esa","esas","ese","eso","esos","esta","estaba","estado",
                  "estan","estar","este","esto","estos","fue","fueron","ha","habia","han","hasta","hay",
                  "la","las","le","les","lo","los","mas","me","mi","mientras","muy","nos","no","nuestra",
                  "nuestro","otra","otro","otras","otros","para","pero","por","porque","que","se","ser",
                  "sera","si","sin","sobre","son","su","sus","tambien","tras","tu","un","una","uno","unos",
                  "ya","asi","sea","ni","cada","tiene","tienen","dijo","segun","ademas","hace","hacer",
                  "pues","solo","aun","todo","todos","toda","todas","ese","esas","aquel","entonces","donde",
                  "quien","quienes","cuyo","puede","pueden","debe","deben","fue","ser","han","sido","tras")

limpiar_texto <- function(x) {
  x <- str_to_lower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")   # quita tildes y ñ
  x <- str_replace_all(x, "[^a-z ]", " ")
  str_squish(x)
}

tokenizar <- function(x) {
  t <- str_split(limpiar_texto(x), " ")[[1]]
  t[nchar(t) >= 3 & !t %in% stopwords_es]
}

# Título: construcción de la matriz TF-IDF de las noticias
lista_tokens <- map(docs$texto_busqueda, tokenizar)

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

M <- matrix(0, nrow = N, ncol = length(vocab))
M[cbind(tf_long$doc, tf_long$j)] <- tf_long$tf * idf[tf_long$j]
norma <- sqrt(rowSums(M^2)); norma[norma == 0] <- 1
M <- M / norma   # cada noticia queda con longitud 1 (para similitud del coseno)

saveRDS(list(docs = docs, vocab = vocab, idf = idf, M = M), "datos/modelo_tfidf.rds")

# Título: función de búsqueda por similitud del coseno
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
  
  puntajes <- as.vector(M %*% q)
  res <- docs %>% mutate(puntaje = round(puntajes, 3)) %>% filter(puntaje > 0)
  if (!is.null(filtro_medio)) res <- res %>% filter(medio %in% filtro_medio)
  
  res %>%
    arrange(desc(puntaje)) %>%
    slice_head(n = k) %>%
    select(puntaje, medio, fecha_hora, titulo, enlace)
}

# Título: pruebas del buscador con tres consultas
message("Noticias indexadas: ", N, " | términos: ", length(vocab))
print(buscar_noticias("caso maletas"), width = 150)
print(buscar_noticias("incendio chiquitania"), width = 150)
print(buscar_noticias("magisterio diálogo gobierno"), width = 150)
