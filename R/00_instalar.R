# Título: instalación de paquetes del proyecto
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rvest, tidyverse, stringr, lubridate, jsonlite, httr, igraph, tidygraph, ggraph)