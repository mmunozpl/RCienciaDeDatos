# src/cap13_aprendizaje.R -- fundamentos del aprendizaje automatico.
# Acompana al cap. 13: reproduce TODAS las cifras (particion, regresion por
# ecuacion normal y lm, metricas con yardstick, clasificacion logistica,
# sobreajuste y regularizacion glmnet, descenso de gradiente, red desde cero
# con retropropagacion en R base, y la leccion tabular xgboost/ranger vs red).
# Cada cifra se ejecuto antes de imprimirse; semillas fijas (el azar de R no es
# portable). rsample, yardstick, parsnip, glmnet, xgboost, ranger instalados.

suppressPackageStartupMessages({
  library(dplyr); library(arrow); library(rsample); library(yardstick)
})
buscar <- function(c) { r <- c[file.exists(c)][1]; if (is.na(r)) NULL else r }
MUS <- buscar(c("data/processed/musica.parquet", "../data/processed/musica.parquet"))
GEN <- c("pop","rock","classical","hip-hop","jazz","reggaeton")

# rebanada equilibrada: 1000 por genero (en el catalogo hay exactamente 1000)
leer_reb <- function() {
  read_parquet(MUS) |> filter(track_genre %in% GEN) |>
    distinct(track_id, track_genre, .keep_all = TRUE) |>
    group_by(track_genre) |> slice_head(n = 1000) |> ungroup() |>
    mutate(track_genre = factor(track_genre, levels = GEN))
}

# --- 1. la particion train/test (rsample, estratificada) --------------------
demo_particion <- function(sub) {
  set.seed(2026)
  div <- initial_split(sub, prop = 0.8, strata = track_genre)
  tr <- training(div); te <- testing(div)
  cat("train:", nrow(tr), "| test:", nrow(te), "\n")
  cat("proporcion por genero conservada (train):\n")
  print(round(prop.table(table(tr$track_genre)), 3))
  invisible(list(tr = tr, te = te))
}

# --- 2. regresion lineal: ecuacion normal a mano ----------------------------
FEAT_REG <- c("energy","acousticness","danceability","valence")
ajustar_lineal <- function(X, y) {
  Xb <- cbind(1, X)                       # columna de unos para b
  solve(crossprod(Xb), crossprod(Xb, y))  # (X'X) beta = X'y
}
predecir_lineal <- function(X, beta) cbind(1, X) %*% beta

demo_regresion <- function(part) {
  Xtr <- as.matrix(part$tr[FEAT_REG]); ytr <- part$tr$loudness
  Xte <- as.matrix(part$te[FEAT_REG]); yte <- part$te$loudness
  beta <- ajustar_lineal(Xtr, ytr)
  cat("ecuacion normal -> b =", round(beta[1], 2),
      "| coef =", paste(round(beta[-1], 2), collapse = " "), "\n")
  # con lm(): misma solucion
  aj <- lm(loudness ~ energy + acousticness + danceability + valence, data = part$tr)
  cat("lm coincide con ecuacion normal:",
      isTRUE(all.equal(unname(coef(aj)), as.numeric(beta), tolerance = 1e-8)), "\n")
  # metricas con yardstick
  ptr <- as.numeric(predecir_lineal(Xtr, beta))
  pte <- as.numeric(predecir_lineal(Xte, beta))
  dtr <- tibble(truth = ytr, est = ptr); dte <- tibble(truth = yte, est = pte)
  cat("MAE train:", round(mae(dtr, truth, est)$.estimate, 2),
      "| MAE test:", round(mae(dte, truth, est)$.estimate, 2),
      "| R2 test:", round(rsq(dte, truth, est)$.estimate, 3),
      "| RMSE test:", round(rmse(dte, truth, est)$.estimate, 2), "\n")
  # interpretabilidad: coeficientes estandarizados (efecto por 1 sd)
  sdx <- apply(Xtr, 2, sd)
  cat("coef estandarizados (dB por 1 sd):",
      paste(round(beta[-1] * sdx, 2), collapse = " "), "\n")
  invisible(list(beta = beta, Xtr = Xtr, ytr = ytr, Xte = Xte, yte = yte))
}

# --- 3. clasificacion: regresion logistica binaria pop vs rock --------------
demo_clasificacion <- function(sub) {
  dos <- sub |> filter(track_genre %in% c("pop","rock")) |>
    mutate(clase = factor(track_genre, levels = c("rock","pop")))
  set.seed(2026)
  div <- initial_split(dos, prop = 0.8, strata = clase)
  tr <- training(div); te <- testing(div)
  aj <- glm(clase ~ danceability + energy + loudness + speechiness +
              acousticness + valence + tempo,
            data = tr, family = binomial())
  prob <- predict(aj, te, type = "response")            # P(pop)
  pred <- factor(ifelse(prob > 0.5, "pop", "rock"), levels = c("rock","pop"))
  ev <- tibble(truth = te$clase, pred = pred, prob = prob)
  cat("exactitud pop-vs-rock:", round(accuracy(ev, truth, pred)$.estimate, 3), "\n")
  cat("precision(pop):", round(precision(ev, truth, pred, event_level="second")$.estimate, 3),
      "| sensibilidad(pop):", round(recall(ev, truth, pred, event_level="second")$.estimate, 3),
      "| F1:", round(f_meas(ev, truth, pred, event_level="second")$.estimate, 3), "\n")
  cat("ROC-AUC:", round(roc_auc(ev, truth, prob, event_level="second")$.estimate, 3), "\n")
  cat("matriz de confusion:\n"); print(conf_mat(ev, truth, pred)$table)
  # tasa base: clase mayoritaria
  cat("tasa base (clase mayoritaria en test):",
      round(max(prop.table(table(te$clase))), 3), "\n")
  invisible(ev)
}

# --- 4. clasificacion multiclase: 6 generos (logistica multinomial) ---------
demo_multiclase <- function(part) {
  if (!requireNamespace("nnet", quietly = TRUE)) { cat("(nnet no instalado)\n"); return() }
  fml <- as.formula(paste("track_genre ~",
    paste(c("danceability","energy","loudness","speechiness","acousticness",
            "instrumentalness","liveness","valence","tempo"), collapse = " + ")))
  aj <- nnet::multinom(fml, data = part$tr, trace = FALSE, maxit = 300)
  pred <- predict(aj, part$te)
  ev <- tibble(truth = part$te$track_genre, pred = pred)
  cat("exactitud 6 generos (multinomial):",
      round(accuracy(ev, truth, pred)$.estimate, 3),
      "| tasa base:", round(1/6, 3), "\n")
  cat("F1 macro:", round(f_meas(ev, truth, pred, estimator="macro")$.estimate, 3), "\n")
  invisible(ev)
}

# --- 5. sobreajuste: grado del polinomio, brecha train/test -----------------
# con POCOS datos de entrenamiento el polinomio de alto grado memoriza el ruido:
# el error de train baja siempre, el de test dibuja una U (sesgo-varianza).
demo_sobreajuste <- function(part) {
  set.seed(2026)
  peq <- part$tr[sample(nrow(part$tr), 40), ]     # solo 40 pistas para aprender
  xtr <- peq$energy; ytr <- peq$loudness
  xte <- part$te$energy; yte <- part$te$loudness
  cat("(entrenando con 40 pistas)\n")
  cat("grado | MAE train | MAE test\n")
  res <- data.frame()
  for (g in c(1, 3, 5, 9, 15)) {
    aj <- lm(ytr ~ poly(xtr, g, raw = FALSE))
    ptr <- predict(aj)
    pte <- as.numeric(predict(aj, data.frame(xtr = xte)))
    m_tr <- mean(abs(ytr - ptr)); m_te <- mean(abs(yte - pte))
    cat(sprintf("  %2d  |  %.3f   |  %.3f\n", g, m_tr, m_te))
    res <- rbind(res, data.frame(grado = g, tr = m_tr, te = m_te))
  }
  invisible(res)
}

# --- 6. regularizacion: ridge y lasso con glmnet ----------------------------
demo_regularizacion <- function(part) {
  if (!requireNamespace("glmnet", quietly = TRUE)) { cat("(glmnet no)\n"); return() }
  library(glmnet)
  feats <- c("danceability","energy","loudness","speechiness","acousticness",
             "instrumentalness","liveness","valence","tempo")
  Xtr <- scale(as.matrix(part$tr[feats])); ytr <- part$tr$loudness
  # reusar centro/escala del train en el test (sin fuga)
  Xte <- scale(as.matrix(part$te[feats]),
               center = attr(Xtr,"scaled:center"), scale = attr(Xtr,"scaled:scale"))
  yte <- part$te$loudness
  # loudness esta entre los predictores? no: es el objetivo; lo quitamos
  keep <- setdiff(feats, "loudness")
  Xtr <- Xtr[, keep]; Xte <- Xte[, keep]
  set.seed(2026)
  cvr <- cv.glmnet(Xtr, ytr, alpha = 0)   # ridge
  cvl <- cv.glmnet(Xtr, ytr, alpha = 1)   # lasso
  mae_te <- function(cv) mean(abs(yte - as.numeric(predict(cv, Xte, s = "lambda.min"))))
  cat("ridge lambda.min:", round(cvr$lambda.min, 4), "| MAE test:", round(mae_te(cvr), 3), "\n")
  cat("lasso lambda.min:", round(cvl$lambda.min, 4), "| MAE test:", round(mae_te(cvl), 3), "\n")
  nz <- sum(as.numeric(coef(cvl, s = "lambda.min"))[-1] != 0)
  cat("lasso: coeficientes no nulos:", nz, "de", length(keep), "\n")
  invisible(NULL)
}

# --- 7. descenso de gradiente (converge a la ecuacion normal) ---------------
demo_gradiente <- function(part) {
  X <- scale(as.matrix(part$tr[FEAT_REG])); y <- part$tr$loudness
  ct <- attr(X,"scaled:center"); sc <- attr(X,"scaled:scale")
  n <- nrow(X); Xb <- cbind(1, X)
  beta <- rep(0, ncol(Xb)); eta <- 0.1
  perdida <- numeric()
  for (it in 1:2000) {
    err <- as.numeric(Xb %*% beta) - y
    grad <- crossprod(Xb, err) / n            # gradiente del ECM
    beta <- beta - eta * grad
    if (it %in% c(1,10,100,1000,2000))
      cat(sprintf("  iter %4d  ECM=%.4f\n", it, mean(err^2)))
  }
  # comparar con la solucion cerrada (sobre X estandarizado)
  cerrada <- solve(crossprod(Xb), crossprod(Xb, y))
  cat("descenso ~ ecuacion normal:",
      isTRUE(all.equal(as.numeric(beta), as.numeric(cerrada), tolerance = 1e-3)), "\n")
  invisible(NULL)
}

# --- 8. red neuronal desde cero (1 capa oculta, retropropagacion) -----------
demo_red_cero <- function(sub) {
  dos <- sub |> filter(track_genre %in% c("pop","rock"))
  set.seed(2026); div <- initial_split(dos, prop = 0.8, strata = track_genre)
  tr <- training(div); te <- testing(div)
  feats <- c("danceability","energy","loudness","speechiness","acousticness","valence","tempo")
  Xtr <- scale(as.matrix(tr[feats]))
  Xte <- scale(as.matrix(te[feats]),
               center = attr(Xtr,"scaled:center"), scale = attr(Xtr,"scaled:scale"))
  ytr <- as.numeric(tr$track_genre == "pop"); yte <- as.numeric(te$track_genre == "pop")
  relu <- function(z) pmax(z, 0)
  sigm <- function(z) 1 / (1 + exp(-z))
  set.seed(1); H <- 8; d <- ncol(Xtr)
  W1 <- matrix(rnorm(d*H, sd = 0.3), d, H); b1 <- rep(0, H)
  W2 <- matrix(rnorm(H,   sd = 0.3), H, 1); b2 <- 0
  eta <- 0.05; n <- nrow(Xtr)
  for (ep in 1:400) {
    Z1 <- sweep(Xtr %*% W1, 2, b1, "+"); A1 <- relu(Z1)
    Z2 <- as.numeric(A1 %*% W2) + b2;    A2 <- sigm(Z2)
    # retropropagacion (entropia cruzada + sigmoide -> dZ2 = A2 - y)
    dZ2 <- (A2 - ytr) / n
    dW2 <- crossprod(A1, dZ2); db2 <- sum(dZ2)
    dA1 <- outer(dZ2, as.numeric(W2)); dZ1 <- dA1 * (Z1 > 0)
    dW1 <- crossprod(Xtr, dZ1); db1 <- colSums(dZ1)
    W2 <- W2 - eta*dW2; b2 <- b2 - eta*db2
    W1 <- W1 - eta*dW1; b1 <- b1 - eta*db1
    if (ep %in% c(1,50,200,400)) {
      L <- -mean(ytr*log(A2+1e-9) + (1-ytr)*log(1-A2+1e-9))
      cat(sprintf("  epoca %3d  perdida=%.4f\n", ep, L))
    }
  }
  pred_te <- as.numeric(sigm(sweep(relu(sweep(Xte %*% W1,2,b1,"+")) %*% W2,2,b2,"+"))) > 0.5
  cat("exactitud test (red desde cero):", round(mean(pred_te == yte), 3), "\n")
  invisible(NULL)
}

# --- 9b. la misma red, con torch (autograd + Adam) --------------------------
# Requiere torch con backend (install_torch()). Se salta si no esta.
demo_torch <- function(sub) {
  if (!requireNamespace("torch", quietly = TRUE)) { cat("(torch no instalado)\n"); return() }
  ok <- tryCatch({ torch::torch_tensor(1); TRUE }, error = function(e) FALSE)
  if (!ok) { cat("(backend de torch no instalado: install_torch())\n"); return() }
  library(torch); torch_manual_seed(2026)
  dos <- sub |> filter(track_genre %in% c("pop","rock"))
  set.seed(2026); div <- initial_split(dos, prop = 0.8, strata = track_genre)
  tr <- training(div); te <- testing(div)
  feats <- c("danceability","energy","loudness","speechiness","acousticness","valence","tempo")
  Xtr <- scale(as.matrix(tr[feats]))
  Xte <- scale(as.matrix(te[feats]),
               center = attr(Xtr,"scaled:center"), scale = attr(Xtr,"scaled:scale"))
  ytr <- as.numeric(tr$track_genre == "pop"); yte <- as.numeric(te$track_genre == "pop")
  xtr <- torch_tensor(Xtr, dtype = torch_float())
  ytr_t <- torch_tensor(ytr, dtype = torch_float())$unsqueeze(2)
  red <- nn_sequential(nn_linear(length(feats), 16), nn_relu(), nn_linear(16, 1))
  opt <- optim_adam(red$parameters, lr = 0.01); lossf <- nn_bce_with_logits_loss()
  for (ep in 1:200) {
    opt$zero_grad(); L <- lossf(red(xtr), ytr_t); L$backward(); opt$step()
    if (ep %in% c(1,50,100,200)) cat(sprintf("  epoca %3d  perdida=%.4f\n", ep, as.numeric(L)))
  }
  prob <- as.numeric(torch_sigmoid(red(torch_tensor(Xte, dtype = torch_float()))))
  cat("exactitud test (torch MLP):", round(mean((prob > 0.5) == yte), 3), "\n")
  invisible(NULL)
}

# --- 9. la leccion tabular: boosting/bosque vs logistica vs red -------------
demo_tabular <- function(part) {
  feats <- c("danceability","energy","loudness","speechiness","acousticness",
             "instrumentalness","liveness","valence","tempo")
  tr <- part$tr; te <- part$te
  ev_acc <- function(pred) mean(pred == te$track_genre)
  out <- c()
  # (a) linea base logistica multinomial
  if (requireNamespace("nnet", quietly = TRUE)) {
    aj <- nnet::multinom(as.formula(paste("track_genre ~", paste(feats, collapse="+"))),
                         data = tr, trace = FALSE, maxit = 300)
    out["logistica"] <- ev_acc(predict(aj, te))
  }
  # (b) bosque aleatorio (ranger)
  if (requireNamespace("ranger", quietly = TRUE)) {
    set.seed(2026)
    rf <- ranger::ranger(track_genre ~ ., data = tr[c("track_genre", feats)],
                         num.trees = 400)
    out["bosque"] <- ev_acc(predict(rf, te[feats])$predictions)
  }
  # (c) gradient boosting (xgboost)
  if (requireNamespace("xgboost", quietly = TRUE)) {
    set.seed(2026)
    ytr <- as.integer(tr$track_genre) - 1L
    dtr <- xgboost::xgb.DMatrix(as.matrix(tr[feats]), label = ytr)
    bst <- xgboost::xgb.train(list(objective="multi:softmax", num_class=6,
                                   eta=0.3, max_depth=6, nthread=4),
                              dtr, nrounds = 120, verbose = 0)
    pr <- factor(GEN[as.integer(predict(bst, as.matrix(te[feats]))) + 1L], levels = GEN)
    out["boosting"] <- ev_acc(pr)
  }
  cat("exactitud 6 generos por modelo:\n")
  for (nm in names(out)) cat(sprintf("  %-10s %.3f\n", nm, out[nm]))
  invisible(out)
}

if (sys.nframe() == 0L) {
  if (is.null(MUS)) { cat("(sin musica.parquet)\n"); quit() }
  sub <- leer_reb(); cat("rebanada:", nrow(sub), "pistas,", nlevels(sub$track_genre), "generos\n")
  cat("\n== 1. particion ==\n"); part <- demo_particion(sub)
  cat("\n== 2. regresion lineal ==\n"); reg <- demo_regresion(part)
  cat("\n== 3. clasificacion binaria ==\n"); demo_clasificacion(sub)
  cat("\n== 4. multiclase 6 generos ==\n"); demo_multiclase(part)
  cat("\n== 5. sobreajuste ==\n"); demo_sobreajuste(part)
  cat("\n== 6. regularizacion ==\n"); demo_regularizacion(part)
  cat("\n== 7. descenso de gradiente ==\n"); demo_gradiente(part)
  cat("\n== 8. red desde cero ==\n"); demo_red_cero(sub)
  cat("\n== 8b. red con torch ==\n"); demo_torch(sub)
  cat("\n== 9. leccion tabular ==\n"); demo_tabular(part)
  cat("\nFIN cap13 OK\n")
}
