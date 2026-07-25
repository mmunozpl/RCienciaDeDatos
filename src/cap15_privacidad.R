# src/cap15_privacidad.R -- privacidad y confidencialidad.
# Acompana al cap. 15: reproduce TODAS las cifras sobre los perfiles de escucha
# sinteticos (50 000 filas, regenerados en R por cap15_generar_perfiles.R):
# reidentificacion y k-anonimato (sdcMicro), l-diversidad, entropia de la huella,
# privacidad diferencial (mecanismo de Laplace, ley 1/eps, respuesta aleatorizada),
# inferencia de pertenencia (dos modelos, AUC del ataque, reusa yardstick), datos
# sinteticos (synthpop, utilidad). Cada cifra se ejecuto antes de imprimirse;
# semillas fijas (el azar de R no es portable).

suppressPackageStartupMessages({library(dplyr); library(arrow)})
buscar <- function(c){ r<-c[file.exists(c)][1]; if(is.na(r)) NULL else r }
PERF <- buscar(c("data/processed/perfiles_escucha.parquet",
                 "../data/processed/perfiles_escucha.parquet"))
SEMILLA <- 2026

# --- 1. reidentificacion: cuasi-identificador fino vs generalizado ----------
tam_grupo <- function(df, cuasi) {
  df |> group_by(across(all_of(cuasi))) |> mutate(k=n()) |> pull(k)
}
demo_reident <- function(perf) {
  # ANTES: cuasi-identificador FINO (cp + fecha_nac + sexo)
  k <- tam_grupo(perf, c("cp","fecha_nac","sexo"))
  cat("cuasi FINO (cp+fecha_nac+sexo): en grupos k<5:",
      round(100*mean(k<5),1), "% | unicos (k=1):", round(100*mean(k==1),1), "%\n")
  # GENERALIZAR: edad a decenios; cp y fecha_nac -> pais
  g <- perf |> mutate(edad10 = (edad %/% 10)*10)
  k2 <- tam_grupo(g, c("pais","edad10","sexo"))
  cat("GENERALIZADO (pais+decenio+sexo): en grupos k<5:",
      round(100*mean(k2<5),1), "%\n")
  # la huella de escucha individua como una huella de navegador
  cat("huellas top50 unicas:", length(unique(perf$huella_top50)), "/", nrow(perf), "\n")
}

# --- 2. k-anonimato con sdcMicro --------------------------------------------
demo_sdc <- function(perf) {
  if (!requireNamespace("sdcMicro", quietly=TRUE)) { cat("(sdcMicro no)\n"); return() }
  library(sdcMicro)
  set.seed(SEMILLA)
  sub <- perf |> slice_sample(n=5000) |>
    mutate(edad10 = (edad %/% 10)*10) |>
    select(pais, edad10, sexo, premium)
  sdc <- createSdcObj(sub, keyVars=c("pais","edad10","sexo"), numVars=NULL)
  # riesgo: registros con k < 3 y k < 5
  fk <- sdc@risk$individual[,"fk"]
  cat("sdcMicro sobre 5000: registros con k<3:", sum(fk<3),
      "| k<5:", sum(fk<5), "\n")
  # l-diversidad del atributo sensible (premium) por grupo de cuasi-id:
  # fraccion de registros en grupos k>=5 que ademas son HOMOGENEOS en premium
  gr <- sub |> group_by(pais, edad10, sexo) |>
    summarise(k=n(), l=n_distinct(premium), .groups="drop")
  gk5 <- gr |> filter(k>=5)
  cat("l-diversidad (premium) en grupos 5-anonimos: homogeneos (l=1):",
      round(100*weighted.mean(gk5$l<=1, gk5$k),1), "% de registros\n")
}

# --- 3. entropia de la huella (bits que individua un cuasi-identificador) ----
demo_entropia <- function(perf) {
  bits <- function(x) { p<-prop.table(table(x)); -sum(p*log2(p)) }
  cat("entropia (bits) por variable:\n")
  for (v in c("pais","sexo","genero_top")) cat(sprintf("  %-11s %.2f bits\n", v, bits(perf[[v]])))
  cat(sprintf("  %-11s %.2f bits\n", "edad", bits(perf$edad)))
  # la combinacion cp+fecha+sexo casi identifica (log2 N = 15.6 bits para 50k)
  comb <- paste(perf$cp, perf$fecha_nac, perf$sexo)
  cat(sprintf("  %-11s %.2f bits (max posible log2(N)=%.2f)\n",
              "cp+fnac+sexo", bits(comb), log2(nrow(perf))))
}

# --- 4. privacidad diferencial: mecanismo de Laplace, ley 1/eps -------------
rlaplace <- function(n, escala) { u<-runif(n)-0.5; -escala*sign(u)*log(1-2*abs(u)) }
demo_laplace <- function(perf) {
  real <- perf |> group_by(pais) |> summarise(pr=sum(premium), .groups="drop")
  set.seed(SEMILLA)
  cat("consulta 'premium por pais' (sensibilidad 1):\n")
  for (eps in c(0.1, 0.5, 1.0, 5.0)) {
    errs <- replicate(200, mean(abs(rlaplace(nrow(real), 1/eps))))
    cat(sprintf("  epsilon=%.1f: error medio ~ %.2f  (1/eps=%.2f)\n",
                eps, mean(errs), 1/eps))
  }
  # un pais concreto: valor real y dispersion de la respuesta con ruido
  p1 <- real$pais[which.max(real$pr)]; v <- max(real$pr)
  set.seed(SEMILLA)
  r_e1 <- v + rlaplace(5000, 1/1.0); r_e01 <- v + rlaplace(5000, 1/0.1)
  cat(sprintf("pais mayor (%s) real=%d | sd(respuesta) eps=1: %.1f, eps=0.1: %.1f\n",
              p1, v, sd(r_e1), sd(r_e01)))
}

# --- 5. privacidad diferencial LOCAL: respuesta aleatorizada -----------------
demo_respuesta_aleatorizada <- function(perf) {
  set.seed(SEMILLA)
  real <- mean(perf$premium)                          # proporcion real de premium
  # cada usuario: con prob 0.5 responde la verdad; si no, tira una moneda
  v <- perf$premium
  moneda1 <- runif(nrow(perf)) < 0.5
  moneda2 <- runif(nrow(perf)) < 0.5
  resp <- ifelse(moneda1, v, as.integer(moneda2))
  # estimador insesgado: p = 2*media_observada - 0.5
  est <- 2*mean(resp) - 0.5
  cat(sprintf("respuesta aleatorizada: premium real=%.3f, estimado=%.3f (ruido local)\n",
              real, est))
}

# --- 6. inferencia de pertenencia: memorizar es filtrar ---------------------
demo_mia <- function(perf) {
  if (!requireNamespace("xgboost", quietly=TRUE) ||
      !requireNamespace("yardstick", quietly=TRUE)) { cat("(faltan paquetes)\n"); return() }
  library(yardstick)
  set.seed(42)
  d <- perf |> mutate(sexoF = as.integer(sexo=="F"))
  feats <- c("edad","cp","energia_media","minutos_dia","sexoF")
  # pocos datos y alta capacidad => memoriza; muchos test
  idx <- sample(nrow(d))
  tr <- d[idx[1:2000], ]; te <- d[idx[2001:7000], ]
  Xtr <- as.matrix(tr[feats]); Xte <- as.matrix(te[feats])
  dtr <- xgboost::xgb.DMatrix(Xtr, label=tr$premium)
  ataque <- function(par, nombre) {
    set.seed(42)
    bst <- xgboost::xgb.train(par, dtr, nrounds=par$nrounds, verbose=0)
    ptr <- predict(bst, Xtr); pte <- predict(bst, Xte)
    acc <- function(p,y) mean((p>0.5)==y)
    brecha <- acc(ptr,tr$premium) - acc(pte,te$premium)
    # ataque: confianza del modelo en la clase REAL de cada registro
    conf_tr <- ifelse(tr$premium==1, ptr, 1-ptr)
    conf_te <- ifelse(te$premium==1, pte, 1-pte)
    ev <- tibble(miembro=factor(c(rep("si",length(conf_tr)), rep("no",length(conf_te))),
                                levels=c("no","si")),
                 conf=c(conf_tr, conf_te))
    auc <- roc_auc(ev, miembro, conf, event_level="second")$.estimate
    cat(sprintf("  %-13s brecha=%+.3f  AUC ataque=%.3f\n", nombre, brecha, auc))
  }
  ataque(list(max_depth=0, eta=0.3, min_child_weight=1, nrounds=600,
              objective="binary:logistic", nthread=4), "sobreajustado")
  ataque(list(max_depth=3, eta=0.1, min_child_weight=100, lambda=1, nrounds=100,
              objective="binary:logistic", nthread=4), "regularizado")
}

# --- 7. datos sinteticos: utilidad y "no son privados por defecto" ----------
demo_sinteticos <- function(perf) {
  if (!requireNamespace("synthpop", quietly=TRUE)) { cat("(synthpop no)\n"); return() }
  library(synthpop)
  set.seed(SEMILLA)
  orig <- perf |> slice_sample(n=3000) |>
    select(edad, sexo, pais, energia_media, minutos_dia, premium) |>
    mutate(across(c(sexo,pais), as.factor))
  syn <- syn(orig, seed=SEMILLA, print.flag=FALSE)
  s <- syn$syn
  cat("utilidad (comparacion de marginales):\n")
  cat(sprintf("  edad media    real %.1f  vs sintetico %.1f\n", mean(orig$edad), mean(s$edad)))
  cat(sprintf("  premium prop  real %.3f vs sintetico %.3f\n", mean(orig$premium), mean(s$premium)))
  cat(sprintf("  cor(edad,min) real %.3f vs sintetico %.3f\n",
              cor(orig$edad, orig$minutos_dia), cor(s$edad, s$minutos_dia)))
  # no privados por defecto: filas sinteticas que replican EXACTAMENTE un registro
  # real (misma fila completa) -> son copias, no invenciones
  clave <- function(d) do.call(paste, c(d[c("edad","sexo","pais","energia_media","minutos_dia","premium")], sep="|"))
  reps <- sum(clave(s) %in% clave(orig))
  cat("filas sinteticas que replican EXACTAMENTE un registro real:",
      reps, "de", nrow(s), "\n")
}

if (sys.nframe()==0L) {
  if (is.null(PERF)) { cat("(sin perfiles_escucha.parquet: correr cap15_generar_perfiles.R)\n"); quit() }
  perf <- read_parquet(PERF); cat("perfiles:", nrow(perf), "x", ncol(perf), "\n")
  cat("\n== 1. reidentificacion ==\n"); demo_reident(perf)
  cat("\n== 2. k-anonimato (sdcMicro) ==\n"); demo_sdc(perf)
  cat("\n== 3. entropia de la huella ==\n"); demo_entropia(perf)
  cat("\n== 4. privacidad diferencial (Laplace) ==\n"); demo_laplace(perf)
  cat("\n== 5. respuesta aleatorizada (DP local) ==\n"); demo_respuesta_aleatorizada(perf)
  cat("\n== 6. inferencia de pertenencia ==\n"); demo_mia(perf)
  cat("\n== 7. datos sinteticos ==\n"); demo_sinteticos(perf)
  cat("\nFIN cap15 OK\n")
}
