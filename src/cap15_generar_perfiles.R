# src/cap15_generar_perfiles.R -- genera perfiles_escucha sinteticos (Clase 2).
# Analogo R del generador del cap. 15: 50 000 perfiles de escucha verosimiles,
# con rasgos musicales tomados de las distribuciones reales por genero de
# musica.parquet (regla de coherencia). El azar es el de R (no portable), asi
# que las cifras exactas difieren de otra implementacion, pero la estructura
# --cuasi-identificadores, huella casi unica, senal para premium-- es la misma.
# Semilla 42. Salida: data/processed/perfiles_escucha.parquet.

suppressPackageStartupMessages({library(dplyr); library(arrow); library(digest)})
buscar <- function(c){ r<-c[file.exists(c)][1]; if(is.na(r)) NULL else r }
MUS <- buscar(c("data/processed/musica.parquet","../data/processed/musica.parquet"))

PAISES <- c("España"=0.06,"México"=0.09,"Argentina"=0.05,"Colombia"=0.05,
  "Chile"=0.03,"Estados Unidos"=0.10,"Brasil"=0.08,"Reino Unido"=0.06,
  "Francia"=0.05,"Alemania"=0.06,"Italia"=0.04,"Japón"=0.05,
  "Corea del Sur"=0.03,"India"=0.05,"Nigeria"=0.03,"Suecia"=0.02,
  "Canadá"=0.03,"Australia"=0.02,"Países Bajos"=0.03,"Portugal"=0.02)
GUSTOS <- list(
  "España"=c("spanish","reggaeton","pop","salsa"),
  "México"=c("latino","reggaeton","salsa","latin"),
  "Argentina"=c("tango","latin","rock","reggaeton"),
  "Colombia"=c("reggaeton","salsa","latino","dancehall"),
  "Chile"=c("latin","reggaeton","indie","pop"),
  "Estados Unidos"=c("hip-hop","pop","country","r-n-b"),
  "Brasil"=c("brazil","samba","mpb","sertanejo"),
  "Reino Unido"=c("british","rock","indie","punk"),
  "Francia"=c("french","house","electronic","disco"),
  "Alemania"=c("german","techno","minimal-techno","trance"),
  "Italia"=c("opera","pop","classical","disco"),
  "Japón"=c("j-pop","j-rock","anime","j-idol"),
  "Corea del Sur"=c("k-pop","pop","r-n-b","dance"),
  "India"=c("indian","world-music","pop","romance"),
  "Nigeria"=c("afrobeat","dancehall","reggae","world-music"),
  "Suecia"=c("swedish","synth-pop","edm","house"),
  "Canadá"=c("pop","rock","hip-hop","country"),
  "Australia"=c("rock","alt-rock","indie","pop"),
  "Países Bajos"=c("trance","hardstyle","house","techno"),
  "Portugal"=c("pop","world-music","rock","romance"))

perfil_musical <- function() {
  m <- read_parquet(MUS, col_select=c("artists","energy","valence","track_genre"))
  stats <- m |> group_by(track_genre) |>
    summarise(e_m=mean(energy), e_s=sd(energy), v_m=mean(valence), v_s=sd(valence),
              .groups="drop")
  primero <- trimws(sub(";.*","", m$artists))
  vivero <- split(primero, m$track_genre)
  list(stats=stats, vivero=vivero)
}

generar <- function(n=50000, seed=42) {
  set.seed(seed)
  pm <- perfil_musical(); stats <- pm$stats; vivero <- pm$vivero
  generos <- stats$track_genre
  paises <- names(PAISES)
  pais <- sample(paises, n, replace=TRUE, prob=PAISES/sum(PAISES))
  base_cp <- setNames(10000 + 500*(seq_along(paises)-1), paises)
  cp <- base_cp[pais] + sample(0:5, n, replace=TRUE)
  edad <- pmin(pmax(as.integer(rgamma(n, shape=7, scale=6)), 0), 99)
  dias <- edad*365 + sample(0:364, n, replace=TRUE)
  fecha_nac <- as.POSIXct("2026-01-01", tz="UTC") - dias*86400
  sexo <- sample(c("M","F"), n, replace=TRUE)
  # genero_top: realce de los generos firma del pais
  genero_top <- character(n)
  for (p in paises) {
    w <- rep(1, length(generos)); w[generos %in% GUSTOS[[p]]] <- w[generos %in% GUSTOS[[p]]] + 12
    m <- pais==p
    genero_top[m] <- sample(generos, sum(m), replace=TRUE, prob=w/sum(w))
  }
  e_m<-setNames(stats$e_m,generos); e_s<-setNames(stats$e_s,generos)
  v_m<-setNames(stats$v_m,generos); v_s<-setNames(stats$v_s,generos)
  energia <- pmin(pmax(rnorm(n, e_m[genero_top], 0.5*e_s[genero_top]), 0), 1)
  valence <- pmin(pmax(rnorm(n, v_m[genero_top], 0.5*v_s[genero_top]), 0), 1)
  minutos <- pmin(pmax(rgamma(n, shape=2, scale=45), 0), 600)
  n_art <- pmin(pmax(as.integer(rgamma(n, shape=4, scale=10)), 3), 300)
  # sigue_a: cola pesada tipo Zipf (aprox Pareto discreta), tope 5000
  sigue_a <- pmin(as.integer(runif(n)^(-1/1.2)), 5000L)
  # huella de escucha: hash de los ~<=50 artistas top del usuario (casi unica)
  huella <- vapply(seq_len(n), function(i){
    pool <- vivero[[genero_top[i]]]
    k <- min(max(n_art[i], 5), 50)
    firma <- paste(sort(unique(sample(pool, k, replace=TRUE))), collapse="|")
    substr(digest(firma, algo="sha256", serialize=FALSE), 1, 16)
  }, character(1))
  z <- -2.75 + 0.95*(minutos-90)/60 + 0.55*(energia-0.5) +
       0.45*(edad-35)/20 + 0.30*(n_art-40)/40 + 0.15*(sexo=="F")
  premium <- as.integer(runif(n) < 1/(1+exp(-z)))
  tibble(id=1:n, fecha_nac=fecha_nac, edad=edad, sexo=sexo, pais=pais, cp=as.integer(cp),
         genero_top=genero_top, energia_media=round(energia,3), valence_media=round(valence,3),
         minutos_dia=as.integer(minutos), n_artistas=n_art, sigue_a=sigue_a,
         huella_top50=huella, premium=premium)
}

if (sys.nframe()==0L) {
  if (is.null(MUS)) { cat("(sin musica.parquet)\n"); quit() }
  d <- generar()
  out <- if (dir.exists("data/processed")) "data/processed/perfiles_escucha.parquet"
         else "../data/processed/perfiles_escucha.parquet"
  write_parquet(d, out)
  cat("perfiles_escucha:", nrow(d), "x", ncol(d), "->", out, "\n")
  cat("edad media:", round(mean(d$edad),1), "| premium:", round(mean(d$premium),3),
      "| huellas unicas:", length(unique(d$huella_top50)), "/", nrow(d), "\n")
}
