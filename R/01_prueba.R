# Título: prueba rápida de lectura de una página web
library(rvest)

url <- "https://es.wikipedia.org/wiki/Bolivia"
pagina <- read_html(url)

titulo <- pagina %>% html_elements("h1") %>% html_text()
print(titulo)