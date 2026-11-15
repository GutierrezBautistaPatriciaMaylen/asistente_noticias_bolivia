# Título: palabras vacías en español (sin tildes) y funciones de limpieza y tokenización
library(stringr)
library(SnowballC)

stopwords_es <- unique(c(
  "a","al","algo","algunas","algunos","ante","antes","como","con","contra","cual","cuales",
  "cuando","de","del","desde","donde","durante","el","ella","ellas","ellos","en","entre",
  "era","eran","es","esa","esas","ese","eso","esos","esta","estaba","estado","estan","estar",
  "estas","este","esto","estos","fue","fueron","ha","habia","han","hasta","hay","la","las",
  "le","les","lo","los","mas","me","mi","mientras","muy","nos","no","nuestra","nuestro",
  "otra","otras","otro","otros","para","pero","por","porque","que","quien","quienes","se",
  "ser","sera","si","sin","sobre","son","su","sus","tambien","tras","tu","un","una","uno",
  "unos","ya","asi","sea","sido","ni","cada","tiene","tienen","dijo","segun","ademas","hace",
  "hacer","pues","solo","aun","todo","todos","toda","todas","entonces","puede","pueden",
  "debe","deben","ayer","hoy","ano","anos","luego","ahora","aquel","cuyo","hacia","dos",
  "tres","parte","varios","vez","via"
))

# verbos de reporte, días, meses y palabras genéricas de las noticias
stopwords_noticias <- c(
  "informo","informaron","informa","senalo","senalaron","senala","indico","indicaron","indica",
  "afirmo","afirmaron","explico","explicaron","anuncio","anunciaron","aseguro","aseguraron",
  "manifesto","expreso","declaro","agrego","preciso","pidio","dijeron","dicen",
  "nueva","nuevo","nuevas","nuevos","queda","quedan","primer","primera","mil","millones",
  "lunes","martes","miercoles","jueves","viernes","sabado","domingo",
  "enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre",
  "octubre","noviembre","diciembre"
)
stopwords_es <- unique(c(stopwords_es, stopwords_noticias))

limpiar_texto <- function(x) {
  x <- str_to_lower(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")   # quita tildes y ñ
  x <- str_replace_all(x, "[^a-z ]", " ")
  str_squish(x)
}

# stem = TRUE devuelve la raíz de cada palabra (incendio e incendios quedan iguales)
tokenizar <- function(x, stem = TRUE) {
  t <- str_split(limpiar_texto(x), " ")[[1]]
  t <- t[nchar(t) >= 3 & !t %in% stopwords_es]
  if (stem) t <- wordStem(t, language = "spanish")
  t
}