# Título: prueba de lectura de titulares de El Deber
library(rvest)

pagina <- read_html("https://eldeber.com.bo")
titulares <- pagina %>% html_elements("h2, h3") %>% html_text(trim = TRUE)
head(titulares, 10)

library(rvest)
pagina <- read_html("https://www.opinion.com.bo")
titulares <- pagina %>% html_elements("h2, h3") %>% html_text(trim = TRUE)
head(titulares, 10)


pagina <- read_html("https://www.lostiempos.com")
titulares <- pagina %>% html_elements("h2, h3") %>% html_text(trim = TRUE)
head(titulares, 10)


pagina <- read_html("https://www.brujuladigital.net")
titulares <- pagina %>% html_elements("h2, h3") %>% html_text(trim = TRUE)
head(titulares, 10)


pagina <- read_html("https://www.erbol.com.bo")
titulares <- pagina %>% html_elements("h2, h3") %>% html_text(trim = TRUE)
head(titulares, 10)
