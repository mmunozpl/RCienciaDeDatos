# src/cap16_ingenieria.R -- reproducibilidad, ingenieria, despliegue y etica.
# Acompana al cap. 16 (cierre del libro): reproduce las cifras verificables:
# pruebas por ejemplo (testthat) y por propiedad (hedgehog), validacion de
# datos (pointblank), auditoria de equidad del clasificador de pago por sexo
# (paridad demografica, TPR, FPR), y deteccion de deriva de datos (KS y PSI)
# entre dos generos. Cada cifra se ejecuto antes de imprimirse; semillas fijas.

suppressPackageStartupMessages({library(dplyr); library(arrow)})
buscar <- function(c){ r<-c[file.exists(c)][1]; if(is.na(r)) NULL else r }
MUS  <- buscar(c("data/processed/musica.parquet","../data/processed/musica.parquet"))
PERF <- buscar(c("data/processed/perfiles_escucha.parquet","../data/processed/perfiles_escucha.parquet"))

# --- una funcion del dominio que probaremos de dos formas -------------------
# normaliza a [0,1] por min-max; invariantes: rango en [0,1], preserva el orden.
normaliza <- function(x) {
  if (length(x) == 0) return(numeric(0))     # borde: vector vacio
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0, length(x)))     # columna constante -> 0
  (x - rng[1]) / diff(rng)
}

# --- 1. pruebas POR EJEMPLO con testthat ------------------------------------
demo_testthat <- function() {
  library(testthat)
  res <- test_that("normaliza: casos concretos", {
    expect_equal(normaliza(c(0, 5, 10)), c(0, 0.5, 1))     # caso tipico
    expect_equal(normaliza(c(7, 7, 7)), c(0, 0, 0))         # constante
    expect_true(all(normaliza(runif(100)) >= 0))            # cota inferior
    expect_length(normaliza(1:20), 20)                      # preserva longitud
  })
  cat("testthat: 4 expectativas por ejemplo ->", if (res) "PASA" else "FALLA", "\n")
}

# --- 2. pruebas POR PROPIEDAD con hedgehog ----------------------------------
demo_hedgehog <- function() {
  if (!requireNamespace("hedgehog", quietly = TRUE)) { cat("(hedgehog no)\n"); return() }
  library(testthat); library(hedgehog)
  # generador de vectores de longitud 2..50 con dobles en [-1000, 1000]
  gen_vec <- gen.and_then(gen.element(0:50),
                          function(n) gen.c(of = n, gen.unif(-1000, 1000)))
  ok <- test_that("normaliza: propiedades sobre 100 casos aleatorios", {
    # propiedad 1: el resultado siempre cae en [0, 1]
    forall(gen_vec, function(x) expect_true(all(normaliza(x) >= 0 & normaliza(x) <= 1)))
    # propiedad 2: normalizar es monotono (ordenar la entrada da salida ordenada)
    forall(gen_vec, function(x) expect_false(is.unsorted(normaliza(sort(x)))))
  })
  cat("hedgehog: 2 propiedades ('[0,1]' y 'monotonia') sobre 100 casos c/u ->",
      if (ok) "PASA" else "FALLA", "\n")
}

# --- 3. validacion de datos con pointblank ----------------------------------
demo_pointblank <- function(mus) {
  if (!requireNamespace("pointblank", quietly = TRUE)) { cat("(pointblank no)\n"); return() }
  library(pointblank)
  agente <- create_agent(tbl = mus, label = "contrato musica") |>
    col_vals_between(energy, 0, 1) |>
    col_vals_between(danceability, 0, 1) |>
    col_vals_gte(tempo, 0) |>
    col_vals_not_null(track_id) |>
    col_vals_between(popularity, 0, 100) |>
    interrogate()
  x <- agente$validation_set
  cat("pointblank: pasos del contrato:", nrow(x),
      "| todos pasan:", all(x$all_passed),
      "| filas evaluadas:", x$n[1], "\n")
  for (i in seq_len(nrow(x)))
    cat(sprintf("  %-22s pasa=%s (%d fallos)\n",
                paste0(x$assertion_type[i], "(", x$column[i], ")"),
                x$all_passed[i], x$n_failed[i]))
}

# --- 4. auditoria de equidad del clasificador de pago por sexo --------------
demo_equidad <- function(perf) {
  library(rsample); library(yardstick)
  d <- perf |> mutate(sexoF = as.integer(sexo == "F"))
  set.seed(2026)
  div <- initial_split(d, prop = 0.7, strata = premium)
  tr <- training(div); te <- testing(div)
  aj <- glm(premium ~ edad + minutos_dia + energia_media + n_artistas + sexoF,
            data = tr, family = binomial())
  prob <- predict(aj, te, type = "response")
  tau <- quantile(prob, 1 - mean(te$premium))     # umbral = tasa base global
  pred <- as.integer(prob >= tau)
  cat("umbral tau:", round(tau, 3), "| seleccion global:", round(mean(pred), 3), "\n")
  cat("grupo  seleccion  TPR    FPR    (premium real)\n")
  for (g in c("M","F")) {
    m <- te$sexo == g
    sel <- mean(pred[m])
    tpr <- mean(pred[m][te$premium[m]==1]); fpr <- mean(pred[m][te$premium[m]==0])
    cat(sprintf("  %s     %.3f      %.3f  %.3f  (%.3f)\n", g, sel, tpr, fpr, mean(te$premium[m])))
  }
}

# --- 5. deriva de datos: KS y PSI entre dos generos -------------------------
psi <- function(ref, act, n_bins = 10) {
  cortes <- quantile(ref, seq(0, 1, length.out = n_bins + 1))
  cortes[1] <- -Inf; cortes[length(cortes)] <- Inf
  p_ref <- pmax(as.numeric(table(cut(ref, cortes))) / length(ref), 1e-6)
  p_act <- pmax(as.numeric(table(cut(act, cortes))) / length(act), 1e-6)
  sum((p_act - p_ref) * log(p_act / p_ref))
}
demo_deriva <- function(mus) {
  ref <- mus$acousticness[mus$track_genre == "classical"]
  nue <- mus$acousticness[mus$track_genre == "jazz"]
  ks <- suppressWarnings(ks.test(ref, nue))
  cat(sprintf("acousticness  media classical %.2f  jazz %.2f\n", mean(ref), mean(nue)))
  cat(sprintf("KS  = %.3f  (p = %.0e)\n", ks$statistic, ks$p.value))
  cat(sprintf("PSI = %.2f  (umbral de deriva severa 0.25)\n", psi(ref, nue)))
}

if (sys.nframe() == 0L) {
  cat("== 1. pruebas por ejemplo (testthat) ==\n"); demo_testthat()
  cat("\n== 2. pruebas por propiedad (hedgehog) ==\n"); demo_hedgehog()
  if (!is.null(MUS)) {
    mus <- read_parquet(MUS)
    cat("\n== 3. validacion de datos (pointblank) ==\n"); demo_pointblank(mus)
    cat("\n== 5. deriva de datos (KS y PSI) ==\n"); demo_deriva(mus)
  }
  if (!is.null(PERF)) {
    perf <- read_parquet(PERF)
    cat("\n== 4. auditoria de equidad ==\n"); demo_equidad(perf)
  }
  cat("\nFIN cap16 OK\n")
}
