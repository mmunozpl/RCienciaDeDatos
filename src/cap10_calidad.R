# src/cap10_calidad.R -- el ciclo de trabajo: limpieza, calidad y transformacion.
# Acompana al cap. 10: reproduce las cifras (contrato con pointblank, validate,
# assertr; ausentes con naniar; imputacion con simputation; atipicos; recipes y la
# fuga de datos; el integrador del fichero sucio). Cada cifra se ejecuto antes de
# imprimirse. Semillas fijas (42 para el fichero sucio; declarado como sintetico).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(arrow)
})
GENEROS <- c("classical", "hip-hop", "jazz", "pop", "reggaeton", "rock")

buscar <- function(c) { r <- c[file.exists(c)][1]; if (is.na(r)) NULL else r }
MUS <- buscar(c("data/processed/musica.parquet", "../data/processed/musica.parquet"))

leer_rebanada <- function() {
  read_parquet(MUS) |>
    filter(track_genre %in% GENEROS) |>
    distinct(track_id, track_genre, .keep_all = TRUE)
}

# --- 1. tidy: desordenado -> ordenado ---------------------------------------
demo_tidy <- function() {
  ancho <- tibble(pista = c("A","B"), e2023 = c(100,200), e2024 = c(150,220))
  tidy <- ancho |> pivot_longer(starts_with("e"), names_to = "anio",
                                names_prefix = "e", values_to = "escuchas")
  cat("ancho:", ncol(ancho), "columnas | tidy:", ncol(tidy), "columnas\n")
  print(tidy)
}

# --- 2. contrato con pointblank sobre el fichero sucio ----------------------
sembrar_sucio <- function(mus) {
  set.seed(42)
  sucio <- mus |> select(track_id, track_name, track_genre, popularity, tempo, energy)
  i <- sample(nrow(sucio), 40)
  sucio$track_genre[i] <- toupper(sucio$track_genre[i])   # 40 erratas de caja
  sucio$tempo[sample(nrow(sucio), 18)] <- 0               # 18 tempos imposibles
  sucio$popularity[sample(nrow(sucio), 12)] <- 999        # 12 fuera de rango
  bind_rows(sucio, sucio[sample(nrow(sucio), 25), ])      # 25 duplicados exactos
}

demo_contrato <- function(sucio) {
  if (!requireNamespace("pointblank", quietly = TRUE)) {
    cat("(pointblank no instalado)\n"); return(invisible()) }
  library(pointblank)
  ag <- create_agent(sucio, actions = action_levels(warn_at = 1, stop_at = 0.05)) |>
    col_vals_between(popularity, 0, 100) |>
    col_vals_gt(tempo, 0) |>
    col_vals_in_set(track_genre, GENEROS) |>
    rows_distinct(vars(track_id, track_genre)) |>
    interrogate()
  vs <- ag$validation_set
  cat("contrato sobre el fichero sucio (", nrow(sucio), "filas):\n")
  for (k in seq_len(nrow(vs)))
    cat(sprintf("  %-16s %d fallos\n", vs$assertion_type[k], vs$n_failed[k]))
}

demo_validate <- function(mus) {
  if (!requireNamespace("validate", quietly = TRUE)) {
    cat("(validate no instalado)\n"); return(invisible()) }
  library(validate)
  mus <- mus |> mutate(tempo = if_else(tempo == 0, NA_real_, tempo))  # imposible -> NA
  reglas <- validator(
    pop_rango  = in_range(popularity, min = 0, max = 100),
    tempo_pos  = tempo > 0,
    genero_dom = track_genre %in% GENEROS,
    energy_01  = in_range(energy, min = 0, max = 1))
  print(summary(confront(mus, reglas))[, c("name","items","passes","fails","nNA")])
}

# --- 3. ausentes con naniar --------------------------------------------------
demo_ausentes <- function(mus) {
  if (!requireNamespace("naniar", quietly = TRUE)) {
    cat("(naniar no instalado)\n"); return(invisible()) }
  library(naniar)
  set.seed(42)
  d <- mus |> mutate(tempo = if_else(tempo == 0, NA_real_, tempo)) |>
    select(track_genre, popularity, energy, tempo, danceability)
  p <- ifelse(d$track_genre == "classical", 0.25, 0.03)
  d$energy[runif(nrow(d)) < p] <- NA                 # MAR: mas en clasica
  d$danceability[sample(nrow(d), 60)] <- NA          # MCAR
  print(miss_var_summary(d))
  cat("n_miss total:", n_miss(d), "| pct:", round(pct_miss(d), 2), "%\n")
  cat("energy NA por genero (%):\n")
  print(d |> group_by(track_genre) |>
        summarise(pct = round(100*mean(is.na(energy)), 1)))
  # la ausencia lleva informacion (bind_shadow)
  sh <- bind_shadow(d) |> group_by(energy_NA) |>
    summarise(pop = round(mean(popularity), 1), n = n())
  print(sh)
  cat("p-valor MCAR (Little):", format.pval(mcar_test(
    select(d, popularity, energy, danceability))$p.value, digits = 3), "\n")
  # sesgo del borrado listwise (elimina sobre todo clasica)
  comp <- d[complete.cases(d), ]
  cat("popularidad media: toda", round(mean(d$popularity), 2),
      "| tras borrar NA", round(mean(comp$popularity), 2), "\n")
  cat("% clasica: antes", round(100*mean(d$track_genre=="classical"), 1),
      "| despues", round(100*mean(comp$track_genre=="classical"), 1), "\n")
  d
}

# --- 4. imputacion y encogimiento de varianza -------------------------------
demo_imputar <- function(mus) {
  if (!requireNamespace("simputation", quietly = TRUE)) {
    cat("(simputation no instalado)\n"); return(invisible()) }
  library(simputation)
  set.seed(7)
  d <- mus |> select(track_genre, energy, danceability, loudness, tempo)
  d$energy[sample(nrow(d), 800)] <- NA
  s0 <- sd(d$energy, na.rm = TRUE)
  cat("sd energy original:", round(s0, 4), "\n")
  cat("  mediana global :", round(sd(impute_median(d, energy ~ 1)$energy), 4),
      sprintf("(%.1f%%)\n", 100*(1 - sd(impute_median(d, energy~1)$energy)/s0)))
  cat("  mediana x genero:", round(sd(impute_median(d, energy ~ track_genre)$energy), 4), "\n")
  cat("  modelo lineal   :", round(sd(impute_lm(d, energy ~ danceability + loudness + tempo)$energy, na.rm=TRUE), 4), "\n")
}

# --- 5. atipicos: imposible vs raro -----------------------------------------
demo_atipicos <- function(mus) {
  t <- mus$tempo[!is.na(mus$tempo) & mus$tempo > 0]
  z <- abs(t - median(t)) / mad(t)
  cat("tempo mediana:", round(median(t),1), "| MAD:", round(mad(t),1), "\n")
  cat("atipicos |z_robusto| > 3.5:", sum(z > 3.5), "de", length(t), "\n")
  qs <- quantile(t, c(.25, .75)); iqr <- diff(qs)
  cat("regla IQR (fuera de 1.5*IQR):",
      sum(t < qs[1]-1.5*iqr | t > qs[2]+1.5*iqr), "\n")
  cat("tempos imposibles (== 0) en el catalogo completo:",
      sum(read_parquet(MUS)$tempo == 0), "\n")
}

# --- 6. recipes: fit/transform y la fuga de datos ---------------------------
demo_recipes <- function(mus) {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    cat("(recipes no instalado)\n"); return(invisible()) }
  library(recipes)
  set.seed(2026)
  idx <- sample(nrow(mus), 0.7*nrow(mus))
  tr <- mus[idx, ]; te <- mus[-idx, ]
  rec <- recipe(popularity ~ energy + danceability + loudness + tempo, data = tr) |>
    step_impute_median(all_numeric_predictors()) |>
    step_normalize(all_numeric_predictors())
  p <- prep(rec, training = tr)
  cat("media energy TRAIN tras normalizar (=0):",
      round(mean(bake(p, new_data = NULL)$energy), 4), "\n")
  cat("media energy TEST con params de train (!=0):",
      round(mean(bake(p, new_data = te)$energy), 4), "\n")
  # la fuga: split desplazado
  trc <- mus |> filter(track_genre %in% c("classical","jazz"))
  tec <- mus |> filter(track_genre == "reggaeton")
  x <- 0.56
  cat("un punto test energy=", x, "-> z(train)=",
      round((x - mean(trc$energy))/sd(trc$energy), 2),
      "vs z(fuga todo)=", round((x - mean(mus$energy))/sd(mus$energy), 2), "\n")
}

# --- 7. integrador: sembrar -> validar -> reparar -> revalidar --------------
demo_integrador <- function(mus) {
  if (!requireNamespace("pointblank", quietly = TRUE)) return(invisible())
  library(pointblank)
  sucio <- sembrar_sucio(mus)
  cat("sucio:", nrow(sucio), "filas\n")
  limpio <- sucio |>
    mutate(track_genre = tolower(track_genre)) |>
    mutate(tempo = if_else(tempo <= 0, NA_real_, tempo)) |>
    mutate(popularity = if_else(popularity > 100, NA_real_, popularity)) |>
    distinct(track_id, track_genre, .keep_all = TRUE)
  cat("limpio:", nrow(limpio), "filas | NA tempo:", sum(is.na(limpio$tempo)),
      "| NA pop:", sum(is.na(limpio$popularity)),
      "| generos:", n_distinct(limpio$track_genre), "\n")
  ag <- create_agent(limpio, actions = action_levels(warn_at = 1)) |>
    col_vals_in_set(track_genre, GENEROS) |>
    rows_distinct(vars(track_id, track_genre)) |> interrogate()
  cat("re-validacion, fallos por regla:",
      paste(ag$validation_set$n_failed, collapse = ", "), "-> dato de confianza\n")
}

# --- ejecucion ---------------------------------------------------------------
if (sys.nframe() == 0L) {
  if (is.null(MUS)) { cat("(sin musica.parquet: nada que ejecutar)\n"); quit() }
  mus <- leer_rebanada()
  cat("rebanada de 6 generos:", nrow(mus), "pistas x", ncol(mus), "columnas\n")
  cat("\n== 1. tidy ==\n");         demo_tidy()
  cat("\n== 2. contrato pointblank (fichero sucio) ==\n"); demo_contrato(sembrar_sucio(mus))
  cat("\n== 2b. validate ==\n");    demo_validate(mus)
  cat("\n== 3. ausentes (naniar) ==\n");  demo_ausentes(mus)
  cat("\n== 4. imputacion (simputation) ==\n"); demo_imputar(mus)
  cat("\n== 5. atipicos ==\n");     demo_atipicos(mus)
  cat("\n== 6. recipes y fuga de datos ==\n");  demo_recipes(mus)
  cat("\n== 7. integrador (sembrar->validar->reparar->revalidar) ==\n"); demo_integrador(mus)
  cat("\nFIN cap10 OK\n")
}
