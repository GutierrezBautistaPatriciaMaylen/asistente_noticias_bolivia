# Título: ejecución completa del proyecto con bitácora automática
source("R/bitacora.R")

# Si defines actualizar_datos <- FALSE antes de ejecutar, no se descarga nada nuevo
if (!exists("actualizar_datos")) actualizar_datos <- TRUE

# 1. detectar cambios en los scripts (todos los archivos modificados en esta corrida comparten este motivo)
registrar_cambios(
  motivo = "Correcciones tras la primera corrida completa: se ampliaron las palabras vacías con verbos de reporte, días y meses (utils_texto.R); los temas de Google Trends ahora salen de las historias más cubiertas y la cobertura se mide como porcentaje diario en días con 15 o más noticias (08); la red de fuentes citadas tenía solo 4.8% de noticias con citas, por lo que se agregó una red de agenda medio seguidor -> medio líder con PageRank y HITS (07) y su exportación (10)",
  efecto = "Los temas de tendencias dejan de ser palabras genéricas, la comparación con Trends deja de mezclarse con el volumen de recolección y se responde quién marca la agenda"
)

if (actualizar_datos) {
  # 2. extraer portadas de los medios
  source("R/03_extraer_enlaces.R")
  registrar_corrida(
    seccion = "Extracción de portadas", archivo = "03_extraer_enlaces.R",
    motivo = "Corrida de extracción de portadas",
    efecto = paste0(bitacora$medio, ": ", bitacora$noticias_encontradas,
                    " (", bitacora$estado, ")", collapse = "; ")
  )
  
  # 3. extraer noticias nuevas y completar las pendientes
  source("R/04_extraer_noticias.R")
  registrar_corrida(
    seccion = "Extracción de noticias", archivo = "04_extraer_noticias.R",
    motivo = paste0("Corrida acumulativa: ", n_nuevas, " noticias nuevas, ", n_completadas,
                    " completadas con fuentes citadas, ", n_errores,
                    " con error; pendientes de completar: ", n_restantes),
    efecto = paste0(resumen$medio, ": ", resumen$total, " en total, ", resumen$con_fecha,
                    " con fecha, ", resumen$con_texto, " con texto, ", resumen$con_citas,
                    " con fuentes citadas", collapse = "; ")
  )
}

# 4. índice TF-IDF y mapa
source("R/05_indice.R")
registrar_corrida(
  seccion = "Índice y mapa", archivo = "05_indice.R",
  motivo = "Construcción de la matriz TF-IDF con raíces de palabras y del mapa PCA",
  efecto = paste0(N, " noticias indexadas, ", length(vocab), " raíces; el mapa 2D explica ", gof, "% de la variabilidad")
)

# 5. historias (grafo de similitud)
source("R/06_historias.R")
registrar_corrida(
  seccion = "Historias", archivo = "06_historias.R",
  motivo = "Grafo de similitud, PageRank y comunidades de Louvain",
  efecto = paste0(nrow(historias), " historias detectadas, ", sum(historias$n_medios >= 3), " cubiertas por 3 o más medios")
)

# 6. redes: fuentes citadas y agenda (PageRank y HITS)
source("R/07_red_fuentes.R")
registrar_corrida(
  seccion = "Redes", archivo = "07_red_fuentes.R",
  motivo = "Red de fuentes citadas y red de agenda entre medios con PageRank y HITS",
  efecto = paste0(pct_con_citas, "% de las noticias citan fuentes externas; ", n_hist_multi,
                  " historias con 2 o más medios; el medio que más marca la agenda es ", lider_agenda)
)

# 7. tendencias de Google Trends Bolivia
source("R/08_tendencias.R")
registrar_corrida(
  seccion = "Tendencias", archivo = "08_tendencias.R",
  motivo = "Google Trends Bolivia frente a la cobertura de los medios",
  efecto = paste0("Temas: ", paste(temas, collapse = ", "), " | Google Trends: ", fuente_trends,
                  " | días comparables: ", dias_validos)
)

# 8. exportar los datos para el dashboard
source("R/09_exportar_json.R", encoding = "UTF-8")
source("R/10_exportar_agenda.R", encoding = "UTF-8")
registrar_corrida(
  seccion = "Exportación", archivo = "09_exportar_json.R",
  motivo = "Generación de los archivos JSON de docs/data",
  efecto = paste0(length(list.files("docs/data")), " archivos generados")
)

# 9. exportar la bitácora (al final, para incluir esta corrida)
exportar_bitacora_json("docs/data/bitacora.json")
