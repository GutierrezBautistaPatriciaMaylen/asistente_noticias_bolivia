# Título: carga de paquetes, bitácora y modelo TF-IDF
library(tidyverse)
library(lubridate)
library(jsonlite)
source("R/bitacora.R")

modelo <- readRDS("datos/modelo_tfidf.rds")
docs  <- modelo$docs
vocab <- modelo$vocab
idf   <- modelo$idf
M     <- modelo$M
dir.create("docs", showWarnings = FALSE)

# Título: preparación de las noticias y sus vectores TF-IDF para la página web
vectores <- map(seq_len(nrow(docs)), function(k) {
  j <- which(M[k, ] > 0)
  list(i = I(j - 1L), w = I(round(M[k, j], 4)))
})

docs_web <- map(seq_len(nrow(docs)), function(k) {
  c(list(m = docs$medio[k],
         f = format(docs$fecha_hora[k], "%Y-%m-%dT%H:%M:%S-04:00"),
         t = docs$titulo[k],
         u = docs$enlace[k],
         r = str_trunc(str_squish(docs$texto[k]), 300)),
    vectores[[k]])
})

# Título: temas del momento, medios fuente y bitácora para la página
reciente <- which(docs$fecha_hora >= max(docs$fecha_hora) - days(2))
df_rec   <- colSums(M[reciente, , drop = FALSE] > 0)
orden    <- order(df_rec * idf, decreasing = TRUE)
temas    <- head(vocab[orden[df_rec[orden] >= 3]], 8)

medios_web <- tribble(
  ~medio, ~url, ~pausa, ~robots,
  "El Deber", "https://eldeber.com.bo", 3,
  "Permite el acceso; se evitan /buscar/, /service/ y otras rutas prohibidas",
  "Opinión", "https://www.opinion.com.bo", 3,
  "Se evitan /api, /comments, /ads y /admin",
  "Los Tiempos", "https://www.lostiempos.com", 10,
  "Pide 10 segundos entre peticiones; se respeta; se evitan /search/, /admin/ y /user/",
  "Brújula Digital", "https://www.brujuladigital.net", 3,
  "Permite búsqueda e indexación; pide no usar el contenido para entrenar IA, aquí solo se usa para un índice de búsqueda con enlace a la fuente",
  "Erbol", "https://www.erbol.com.bo", 10,
  "Pide 10 segundos entre peticiones; se respeta; se evitan /search/, /admin/ y /user/"
)

bit <- leer_bitacora()
bit_web <- if (is.null(bit)) list() else as.data.frame(bit[nrow(bit):1, ])

# Título: construcción del JSON con todos los datos de la página
datos <- list(
  meta = list(generado = format(Sys.time(), "%d/%m/%Y %H:%M"), n_docs = nrow(docs)),
  temas = I(temas),
  vocab = I(vocab),
  idf = I(round(idf, 4)),
  docs = docs_web,
  medios = medios_web,
  bitacora = bit_web
)
json <- as.character(toJSON(datos, auto_unbox = TRUE, na = "null", digits = 4))
json <- gsub("</", "<\\/", json, fixed = TRUE)   # evita cerrar la etiqueta script por error

# Título: plantilla HTML de la página (CSS y JavaScript incluidos)
plantilla <- r"---(<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Asistente de noticias de Bolivia</title>
<style>
:root{--bg:#f4f6fb;--card:#fff;--tx:#1b1f2a;--mut:#667085;--ac:#2a5bd7;--bd:#e3e7ef}
@media (prefers-color-scheme:dark){:root{--bg:#0f1219;--card:#181d28;--tx:#e8eaf0;--mut:#9aa4b8;--ac:#7aa2ff;--bd:#2a3142}}
*{box-sizing:border-box}
body{margin:0;font-family:system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;background:var(--bg);color:var(--tx);line-height:1.45}
.wrap{max-width:980px;margin:0 auto;padding:0 18px}
header{padding:26px 0 10px}
h1{margin:0 0 4px;font-size:1.7rem}
h2{font-size:1.05rem;margin:20px 0 8px}
.sub{margin:0 0 12px;color:var(--mut)}
.note,small{color:var(--mut)}
.pills{display:flex;flex-wrap:wrap;gap:8px}
.pill{background:var(--card);border:1px solid var(--bd);border-radius:999px;padding:3px 12px;font-size:.8rem;color:var(--mut)}
nav{display:flex;gap:6px;flex-wrap:wrap;margin:14px 0}
nav button{border:1px solid var(--bd);background:var(--card);color:var(--tx);padding:8px 16px;border-radius:10px;cursor:pointer;font-size:.92rem}
nav button.on{background:var(--ac);border-color:var(--ac);color:#fff}
.panel{background:var(--card);border:1px solid var(--bd);border-radius:14px;padding:18px;margin-bottom:30px}
#msgs{min-height:240px;max-height:62vh;overflow-y:auto;display:flex;flex-direction:column;gap:12px;padding:10px 0}
.msg{max-width:94%;padding:10px 14px;border-radius:14px}
.user{align-self:flex-end;background:var(--ac);color:#fff}
.bot{align-self:flex-start;background:var(--bg);border:1px solid var(--bd)}
.bot p{margin:4px 0}
.card{display:block;text-decoration:none;color:inherit;border:1px solid var(--bd);background:var(--card);border-radius:12px;padding:10px 12px;margin:8px 0}
.card:hover{border-color:var(--ac)}
.meta{display:flex;gap:10px;flex-wrap:wrap;align-items:center;font-size:.78rem;color:var(--mut);margin-bottom:4px}
.badge{display:inline-block;color:#fff;border-radius:6px;padding:1px 8px;font-size:.74rem;font-weight:600}
.ttl{font-weight:600}
.ext{font-size:.86rem;color:var(--mut);margin-top:3px}
.ask{display:flex;gap:8px;margin-top:10px}
.ask input{flex:1;padding:11px 14px;border-radius:10px;border:1px solid var(--bd);background:var(--bg);color:var(--tx);font-size:1rem}
.ask button{background:var(--ac);color:#fff;border:0;border-radius:10px;padding:0 22px;cursor:pointer;font-size:1rem}
.chip{border:1px solid var(--bd);background:var(--bg);color:var(--tx);border-radius:999px;padding:4px 12px;margin:3px 4px 3px 0;cursor:pointer;font-size:.84rem}
.bar{display:flex;align-items:center;gap:10px;margin:6px 0;font-size:.88rem}
.bar .lb{width:130px;flex:none}
.bar .tr{flex:1;background:var(--bg);border-radius:6px;height:16px;overflow:hidden}
.bar .vl{width:34px;text-align:right;color:var(--mut)}
.tw{overflow-x:auto}
table{width:100%;border-collapse:collapse;font-size:.84rem}
th,td{text-align:left;padding:8px;border-bottom:1px solid var(--bd);vertical-align:top}
.tag{padding:1px 8px;border-radius:6px;font-size:.74rem;background:var(--bg);border:1px solid var(--bd)}
pre{white-space:pre-wrap;word-break:break-word;max-width:420px;font-size:.76rem;margin:4px 0 0}
footer{padding:0 0 30px;font-size:.8rem;color:var(--mut)}
[hidden]{display:none!important}
</style>
</head>
<body>
<div class="wrap">
<header>
<h1>📰 Asistente de noticias de Bolivia</h1>
<p class="sub">Pregunta por un tema y descubre qué publican los medios bolivianos y cómo lo cubre cada uno.</p>
<div class="pills" id="pills"></div>
</header>
<nav>
<button data-t="chat" class="on">💬 Asistente</button>
<button data-t="panorama">📊 Panorama</button>
<button data-t="bitacora">🗂️ Bitácora</button>
<button data-t="origen">🔎 Origen de los datos</button>
</nav>

<section class="panel" id="chat">
<div id="filtros" class="note"></div>
<div id="msgs"></div>
<div class="ask"><input id="q" placeholder="Ej.: incendio en la Chiquitania, caso maletas, magisterio..." autocomplete="off"><button id="go">Buscar</button></div>
<div id="sug" style="margin-top:10px"></div>
</section>

<section class="panel" id="panorama" hidden>
<h2>Noticias por medio</h2><div id="bmedio"></div>
<h2>Noticias por día de publicación</h2><div id="bdia"></div>
<h2>Temas del momento</h2><div id="temas"></div>
</section>

<section class="panel" id="bitacora" hidden>
<p class="note">Registro automático de los pasos, los cambios en el código (con el fragmento modificado) y las corridas del proyecto.</p>
<div id="ultima"></div>
<label>Filtrar por tipo:
<select id="ftipo"><option value="">todos</option><option>linea_base</option><option>cambio</option><option>corrida</option><option>paso</option></select></label>
<div class="tw"><table><thead><tr><th>Fecha</th><th>Tipo</th><th>Sección</th><th>Archivo</th><th>Motivo</th><th>Fragmento</th><th>Efecto</th></tr></thead><tbody id="bitbody"></tbody></table></div>
</section>

<section class="panel" id="origen" hidden>
<h2>Problema que resuelve</h2>
<p>Nadie lee cinco periódicos al día. Este asistente reúne en un solo lugar las noticias de varios medios bolivianos, permite buscar por tema y muestra qué medio publicó qué y cuándo, siempre con enlace a la fuente original.</p>
<h2>Fuentes</h2>
<div class="tw"><table><thead><tr><th>Medio</th><th>Sitio</th><th>Pausa entre peticiones</th><th>Condiciones (robots.txt)</th></tr></thead><tbody id="orbody"></tbody></table></div>
<h2>Método y uso responsable</h2>
<ul>
<li>Datos obtenidos por web scraping con R (paquete rvest) de las portadas y páginas de noticias de los medios listados.</li>
<li>Se leyó el robots.txt de cada sitio; se respetan sus restricciones y las pausas entre peticiones.</li>
<li>El programa se identifica como proyecto académico de Minería de Datos.</li>
<li>Solo se muestra el titular, la fecha y un extracto breve; el contenido completo está en el medio, con enlace.</li>
<li>La búsqueda usa TF-IDF y similitud del coseno sobre el texto de las noticias.</li>
<li>Los datos reflejan lo publicado al momento de la extracción; no es un archivo histórico completo.</li>
</ul>
</section>
<footer>Proyecto académico de Minería de Datos · UMSA · hecho con R y TF-IDF</footer>
</div>

<script id="datos" type="application/json">__DATOS__</script>
<script>
const D=JSON.parse(document.getElementById('datos').textContent);
const $=s=>document.querySelector(s);
const esc=s=>String(s==null?'':s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const COL={'El Deber':'#d9480f','Opinión':'#1c7ed6','Los Tiempos':'#2b8a3e','Brújula Digital':'#7048e8','Erbol':'#c2255c'};
const colorDe=m=>COL[m]||'#667085';
const fmt=f=>f?new Date(f).toLocaleString('es-BO',{dateStyle:'medium',timeStyle:'short',timeZone:'America/La_Paz'}):'';
const urlOk=u=>/^https?:\/\//.test(u)?u:'#';
const vocabIdx=new Map(D.vocab.map((t,i)=>[t,i]));
const medios=[...new Set(D.docs.map(d=>d.m))].sort();
const activos=new Set(medios);

function tokenizar(s){
  return s.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g,'').replace(/[^a-z ]/g,' ').split(/\s+/).filter(t=>t.length>=3);
}

function buscar(consulta){
  const toks=tokenizar(consulta).filter(t=>vocabIdx.has(t));
  if(!toks.length) return null;
  const cnt=new Map();
  toks.forEach(t=>{const j=vocabIdx.get(t);cnt.set(j,(cnt.get(j)||0)+1);});
  const q=new Map();let n2=0;
  cnt.forEach((c,j)=>{const v=(c/toks.length)*D.idf[j];q.set(j,v);n2+=v*v;});
  const nq=Math.sqrt(n2);
  const res=[];
  D.docs.forEach(d=>{
    if(!activos.has(d.m)) return;
    let s=0;
    for(let k=0;k<d.i.length;k++){const v=q.get(d.i[k]);if(v!==undefined) s+=v*d.w[k];}
    s/=nq;
    if(s>0) res.push({d:d,s:s});
  });
  res.sort((a,b)=>b.s-a.s);
  return res;
}

function addMsg(rol,html){
  const m=$('#msgs');
  const e=document.createElement('div');
  e.className='msg '+rol;e.innerHTML=html;
  m.appendChild(e);m.scrollTop=m.scrollHeight;
}

function tarjeta(r){
  return `<a class="card" href="${esc(urlOk(r.d.u))}" target="_blank" rel="noopener">
  <div class="meta"><span class="badge" style="background:${colorDe(r.d.m)}">${esc(r.d.m)}</span><span>${esc(fmt(r.d.f))}</span><span>similitud ${r.s.toFixed(2)}</span></div>
  <div class="ttl">${esc(r.d.t)}</div><div class="ext">${esc(r.d.r)}</div></a>`;
}

function preguntar(texto){
  texto=texto.trim();
  if(!texto) return;
  addMsg('user',esc(texto));
  if(activos.size===0){addMsg('bot','Activa al menos un medio para poder buscar.');return;}
  const res=buscar(texto);
  if(!res||!res.length){
    addMsg('bot','No encontré noticias sobre eso en la base actual. Prueba con otras palabras o elige un tema sugerido.');
    return;
  }
  const rel=res.filter(r=>r.s>=0.1);
  const base=rel.length?rel:res;
  const porMedio={};
  base.forEach(r=>{porMedio[r.d.m]=(porMedio[r.d.m]||0)+1;});
  const cob=Object.entries(porMedio).sort((a,b)=>b[1]-a[1]).map(([m,c])=>`<span class="badge" style="background:${colorDe(m)}">${esc(m)}: ${c}</span>`).join(' ');
  addMsg('bot',`<p>Encontré <b>${base.length}</b> noticia(s) relacionada(s) con «${esc(texto)}». Cobertura por medio:</p><p>${cob}</p>${res.slice(0,5).map(tarjeta).join('')}`);
}

function enviar(){const i=$('#q');const t=i.value;i.value='';preguntar(t);}

function bars(ent,color){
  const mx=Math.max(1,...ent.map(e=>e[1]));
  return ent.map(([k,v])=>`<div class="bar"><span class="lb">${esc(k)}</span><span class="tr"><span style="display:block;height:100%;border-radius:6px;width:${(v/mx*100).toFixed(1)}%;background:${color?color(k):'var(--ac)'}"></span></span><span class="vl">${v}</span></div>`).join('');
}

function renderBit(){
  const t=$('#ftipo').value;
  const rows=D.bitacora.filter(b=>!t||b.tipo===t);
  $('#bitbody').innerHTML=rows.map(b=>`<tr><td>${esc(b.fecha)}<br><small>${esc(b.hora)}</small></td><td><span class="tag">${esc(b.tipo)}</span></td><td>${esc(b.seccion)}</td><td>${esc(b.archivo)}<br><small>${esc(b.version)}</small></td><td>${esc(b.motivo)}</td><td>${b.fragmento?`<details><summary>ver</summary><pre>${esc(b.fragmento)}</pre></details>`:''}</td><td>${esc(b.efecto)}</td></tr>`).join('');
}

function tab(id){
  document.querySelectorAll('.panel').forEach(p=>{p.hidden=(p.id!==id);});
  document.querySelectorAll('nav button').forEach(b=>b.classList.toggle('on',b.dataset.t===id));
}

/* inicio */
$('#pills').innerHTML=[D.meta.n_docs+' noticias',medios.length+' medios','Actualizado: '+esc(D.meta.generado)].map(t=>`<span class="pill">${t}</span>`).join('');
$('#filtros').innerHTML='Medios a consultar: '+medios.map(m=>`<label style="margin-right:12px;white-space:nowrap"><input type="checkbox" value="${esc(m)}" checked> ${esc(m)}</label>`).join('');
$('#filtros').addEventListener('change',e=>{if(e.target.value){if(e.target.checked) activos.add(e.target.value); else activos.delete(e.target.value);}});

const chips=D.temas.map(t=>`<button class="chip" data-q="${esc(t)}">${esc(t)}</button>`).join('');
$('#sug').innerHTML=chips?'<span class="note">Temas del momento: </span>'+chips:'';
$('#temas').innerHTML=chips||'<span class="note">Aún no hay suficientes noticias recientes.</span>';

const porMedio={};D.docs.forEach(d=>{porMedio[d.m]=(porMedio[d.m]||0)+1;});
$('#bmedio').innerHTML=bars(Object.entries(porMedio).sort((a,b)=>b[1]-a[1]),colorDe);
const porDia={};D.docs.forEach(d=>{const k=(d.f||'').slice(0,10);if(k) porDia[k]=(porDia[k]||0)+1;});
$('#bdia').innerHTML=bars(Object.entries(porDia).sort((a,b)=>a[0]<b[0]?-1:1).slice(-10));

const ult=D.bitacora.find(b=>b.tipo==='corrida');
$('#ultima').innerHTML=ult?`<p><b>Última corrida:</b> ${esc(ult.fecha)} ${esc(ult.hora)} · ${esc(ult.seccion)}<br><small>${esc(ult.efecto)}</small></p>`:'';
renderBit();
$('#ftipo').addEventListener('change',renderBit);

$('#orbody').innerHTML=D.medios.map(m=>`<tr><td>${esc(m.medio)}</td><td><a href="${esc(urlOk(m.url))}" target="_blank" rel="noopener">${esc(m.url)}</a></td><td>${esc(m.pausa)} s</td><td>${esc(m.robots)}</td></tr>`).join('');

document.querySelectorAll('nav button').forEach(b=>b.addEventListener('click',()=>tab(b.dataset.t)));
document.addEventListener('click',e=>{const c=e.target.closest('[data-q]');if(c){tab('chat');preguntar(c.dataset.q);}});
$('#go').addEventListener('click',enviar);
$('#q').addEventListener('keydown',e=>{if(e.key==='Enter') enviar();});
addMsg('bot','Hola 👋 Busco en las noticias de '+medios.length+' medios bolivianos. Escribe un tema o elige uno de los sugeridos.');
</script>
</body>
</html>)---"

# Título: generación del archivo docs/index.html
partes <- strsplit(plantilla, "__DATOS__", fixed = TRUE)[[1]]
html <- paste0(partes[1], json, partes[2])
writeLines(enc2utf8(html), "docs/index.html", useBytes = TRUE)
message("Página generada: docs/index.html (",
        round(file.size("docs/index.html") / 1e6, 2), " MB)")