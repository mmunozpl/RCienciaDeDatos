# src/cap14_tidymodels.R -- modelado tabular reproducible con tidymodels.
# Acompana al cap. 14: reproduce las cifras del flujo de trabajo completo:
# especificacion parsnip, receta recipes (anti-fuga), workflow, validacion
# cruzada con rsample, ajuste de hiperparametros con tune, arboles/bosque/
# boosting via parsnip, clasificacion, e interpretabilidad (vip/pdp/shap).
# Cada cifra se ejecuto antes de imprimirse; semillas fijas (azar no portable).

suppressPackageStartupMessages({
  library(dplyr); library(arrow)
  library(rsample); library(recipes); library(parsnip)
  library(workflows); library(tune); library(yardstick); library(dials)
})
buscar <- function(c) { r <- c[file.exists(c)][1]; if (is.na(r)) NULL else r }
MUS <- buscar(c("data/processed/musica.parquet", "../data/processed/musica.parquet"))
GEN <- c("pop","rock","classical","hip-hop","jazz","reggaeton")

leer_reb <- function() {
  read_parquet(MUS) |> filter(track_genre %in% GEN) |>
    distinct(track_id, track_genre, .keep_all = TRUE) |>
    group_by(track_genre) |> slice_head(n = 1000) |> ungroup() |>
    mutate(track_genre = factor(track_genre, levels = GEN))
}
NUM <- c("danceability","energy","speechiness","acousticness",
         "instrumentalness","liveness","valence","tempo")

# --- 1. la especificacion parsnip: modelo, motor, modo --------------------
demo_parsnip <- function(part) {
  esp <- linear_reg()                        # modelo (algoritmo)
  cat("spec:", class(esp)[1], "| engine:", esp$engine, "\n")
  aj <- esp |> fit(loudness ~ danceability + energy + acousticness + valence,
                   data = part$tr)
  pred <- predict(aj, part$te) |> bind_cols(part$te["loudness"])
  cat("R2 (regresion lineal, 4 rasgos):",
      round(rsq(pred, loudness, .pred)$.estimate, 3), "\n")
  # cambiar de modelo = cambiar el spec, no el resto
  arbol <- decision_tree(mode = "regression") |> set_engine("rpart")
  aj2 <- arbol |> fit(loudness ~ danceability + energy + acousticness + valence,
                      data = part$tr)
  pred2 <- predict(aj2, part$te) |> bind_cols(part$te["loudness"])
  cat("R2 (arbol de decision, mismos rasgos):",
      round(rsq(pred2, loudness, .pred)$.estimate, 3), "\n")
}

# --- 2. receta + workflow (preprocesado que se aprende sin fuga) ----------
demo_workflow <- function(part) {
  rec <- recipe(loudness ~ ., data = part$tr |> select(loudness, all_of(NUM))) |>
    step_normalize(all_numeric_predictors())        # aprende centro/escala del TRAIN
  wf <- workflow() |> add_recipe(rec) |> add_model(linear_reg())
  aj <- fit(wf, part$tr)
  pred <- predict(aj, part$te) |> bind_cols(part$te["loudness"])
  cat("workflow (receta+modelo) R2:", round(rsq(pred, loudness, .pred)$.estimate, 3),
      "| MAE:", round(mae(pred, loudness, .pred)$.estimate, 3), "\n")
  invisible(wf)
}

# --- 3. validacion cruzada con rsample (fit_resamples) --------------------
demo_cv <- function(part) {
  set.seed(2026); pliegues <- vfold_cv(part$tr, v = 5)
  rec <- recipe(loudness ~ ., data = part$tr |> select(loudness, all_of(NUM))) |>
    step_normalize(all_numeric_predictors())
  wf <- workflow() |> add_recipe(rec) |> add_model(linear_reg())
  res <- fit_resamples(wf, pliegues, metrics = metric_set(rmse, rsq, mae))
  print(collect_metrics(res) |> select(.metric, mean, std_err))
}

# --- 4. arboles: bosque y boosting via parsnip ----------------------------
demo_arboles <- function(part) {
  rec <- recipe(loudness ~ ., data = part$tr |> select(loudness, all_of(NUM)))
  eval1 <- function(spec, nombre) {
    wf <- workflow() |> add_recipe(rec) |> add_model(spec)
    set.seed(2026); aj <- fit(wf, part$tr)
    pred <- predict(aj, part$te) |> bind_cols(part$te["loudness"])
    cat(sprintf("  %-22s R2 %.3f | MAE %.3f\n", nombre,
                rsq(pred, loudness, .pred)$.estimate,
                mae(pred, loudness, .pred)$.estimate))
  }
  eval1(linear_reg(), "regresion lineal")
  eval1(rand_forest(trees = 500, mode = "regression") |> set_engine("ranger"),
        "bosque aleatorio")
  eval1(boost_tree(trees = 500, mode = "regression") |> set_engine("xgboost"),
        "gradient boosting")
}

# --- 5. ajuste de hiperparametros con tune (malla) ------------------------
demo_tune <- function(part) {
  rec <- recipe(loudness ~ ., data = part$tr |> select(loudness, all_of(NUM)))
  esp <- boost_tree(trees = 500, tree_depth = tune(), learn_rate = tune(),
                    mode = "regression") |> set_engine("xgboost")
  wf <- workflow() |> add_recipe(rec) |> add_model(esp)
  malla <- grid_regular(tree_depth(range = c(2L, 8L)),
                        learn_rate(range = c(-2, -0.5)), levels = 4)
  cat("malla:", nrow(malla), "combinaciones\n")
  set.seed(2026); pliegues <- vfold_cv(part$tr, v = 5)
  res <- tune_grid(wf, pliegues, grid = malla, metrics = metric_set(rmse, rsq))
  mejor <- select_best(res, metric = "rmse")
  cat("mejor: tree_depth", mejor$tree_depth, "learn_rate", round(mejor$learn_rate, 4), "\n")
  wf_fin <- finalize_workflow(wf, mejor)
  # last_fit: entrena en TODO el train y evalua UNA vez en test
  set.seed(2026); lf <- last_fit(wf_fin, part$div, metrics = metric_set(rmse, rsq, mae))
  print(collect_metrics(lf) |> select(.metric, .estimate))
  invisible(list(wf = wf_fin, div = part$div))
}

# --- 6. clasificacion: workflow para predecir el genero -------------------
demo_clasificar <- function(part) {
  rec <- recipe(track_genre ~ ., data = part$tr |> select(track_genre, all_of(NUM)))
  esp <- boost_tree(trees = 400, mode = "classification") |> set_engine("xgboost")
  wf <- workflow() |> add_recipe(rec) |> add_model(esp)
  set.seed(2026); aj <- fit(wf, part$tr)
  pr <- predict(aj, part$te) |> bind_cols(part$te["track_genre"])
  prp <- predict(aj, part$te, type = "prob") |> bind_cols(part$te["track_genre"])
  cat("exactitud (boosting, 6 generos):",
      round(accuracy(pr, track_genre, .pred_class)$.estimate, 3), "\n")
  cat("F1 macro:", round(f_meas(pr, track_genre, .pred_class)$.estimate, 3), "\n")
  cat("ROC-AUC (una-contra-resto):",
      round(roc_auc(prp, track_genre,
                    paste0(".pred_", GEN))$.estimate, 3), "\n")
}

# --- 7. interpretabilidad: importancia, PDP, SHAP -------------------------
demo_interpret <- function(part) {
  # ajustar un boosting sobre los 8 rasgos (regresion del volumen)
  set.seed(2026)
  rec <- recipe(loudness ~ ., data = part$tr |> select(loudness, all_of(NUM)))
  wf <- workflow() |> add_recipe(rec) |>
    add_model(boost_tree(trees = 400, mode = "regression") |> set_engine("xgboost"))
  aj <- fit(wf, part$tr)
  # (a) importancia por permutacion con DALEX
  if (requireNamespace("DALEX", quietly = TRUE)) {
    ex <- DALEX::explain(aj, data = part$te[NUM], y = part$te$loudness, verbose = FALSE,
                         predict_function = function(m, d)
                           predict(m, d)$.pred)
    set.seed(1); imp <- DALEX::model_parts(ex, B = 5)
    ag <- aggregate(dropout_loss ~ variable, imp[imp$permutation == 0, ], mean)
    ag <- ag[!ag$variable %in% c("_full_model_","_baseline_"), ]
    ag <- ag[order(-ag$dropout_loss), ]
    cat("importancia por permutacion (RMSE al barajar), top 4:\n")
    for (i in 1:4) cat(sprintf("  %-16s %.3f\n", ag$variable[i], ag$dropout_loss[i]))
  }
  # (b) SHAP con xgboost nativo (predcontrib) + resumen
  if (requireNamespace("shapviz", quietly = TRUE)) {
    fitted <- extract_fit_engine(aj)
    Xte <- as.matrix(part$te[NUM])
    sv <- shapviz::shapviz(fitted, X_pred = Xte, X = part$te[NUM])
    ms <- sort(colMeans(abs(shapviz::get_shap_values(sv))), decreasing = TRUE)
    cat("SHAP (importancia media |phi|), top 4:\n")
    for (i in 1:4) cat(sprintf("  %-16s %.3f\n", names(ms)[i], ms[i]))
  }
}

# --- 8. la fuga fabrica senal de la nada (p >> n, ruido puro) --------------
demo_fuga <- function() {
  set.seed(1)
  n <- 150; p <- 3000                    # pocos datos, muchos rasgos de RUIDO
  X <- matrix(rnorm(n*p), n, p); colnames(X) <- paste0("z", 1:p)
  y <- rnorm(n)                          # objetivo SIN relacion con nada
  tr <- 1:120; te <- 121:150
  r2 <- function(m, d) max(0, cor(predict(m, d[te,]), y[te])^2)
  # FUGA: elegir los 20 rasgos mas correlacionados con y usando TODO (tr+te)
  top_f <- order(abs(cor(X, y)), decreasing = TRUE)[1:20]
  df <- data.frame(y = y, X[, top_f])
  cat("R2 con FUGA (seleccion sobre todo): ", round(r2(lm(y~., df[tr,]), df), 2), "<- espejismo\n")
  # HONESTO: elegir usando solo el train
  top_o <- order(abs(cor(X[tr,], y[tr])), decreasing = TRUE)[1:20]
  df2 <- data.frame(y = y, X[, top_o])
  cat("R2 HONESTO (seleccion solo train):  ", round(r2(lm(y~., df2[tr,]), df2), 2), "<- la verdad (ruido)\n")
}

if (sys.nframe() == 0L) {
  if (is.null(MUS)) { cat("(sin musica.parquet)\n"); quit() }
  sub <- leer_reb(); cat("rebanada:", nrow(sub), "pistas\n")
  set.seed(2026); div <- initial_split(sub, prop = 0.8, strata = track_genre)
  part <- list(tr = training(div), te = testing(div), div = div)
  cat("\n== 1. la especificacion parsnip ==\n"); demo_parsnip(part)
  cat("\n== 2. receta + workflow ==\n"); demo_workflow(part)
  cat("\n== 3. validacion cruzada ==\n"); demo_cv(part)
  cat("\n== 4. arboles: bosque y boosting ==\n"); demo_arboles(part)
  cat("\n== 5. ajuste de hiperparametros ==\n"); demo_tune(part)
  cat("\n== 6. clasificacion ==\n"); demo_clasificar(part)
  cat("\n== 7. interpretabilidad ==\n"); demo_interpret(part)
  cat("\n== 8. la fuga fabrica senal ==\n"); demo_fuga()
  cat("\nFIN cap14 OK\n")
}
