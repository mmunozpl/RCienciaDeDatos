# src/cap07_vectorizacion.R -- vectorizacion, matrices y algebra en R.
# Acompana al cap. 7: reproduce cada experimento (los tiempos son cocientes;
# los absolutos dependen de la maquina y de la BLAS). Semillas fijas.

suppressPackageStartupMessages({
  library(Matrix); library(lobstr)
})
crono <- function(expr) unname(system.time(expr)["elapsed"])

demo_vectorizacion <- function() {
  set.seed(2026); v <- runif(1e6)
  tb <- crono({ s <- 0; for (i in seq_along(v)) s <- s + v[i] })
  tv <- crono(sum(v))
  cat("bucle vs sum:", round(tb / max(tv, 1e-4)), "x\n")
  # reciclaje
  cat("reciclaje:", c(1,2,3,4,5,6) + c(10,20), "\n")
  # mascaras
  set.seed(1); e <- runif(20)
  cat("> 0.5:", sum(e > 0.5), "| media altas:", round(mean(e[e > 0.5]), 3), "\n")
  cat("pmax/pmin:", pmin(pmax(c(-3,5,8), 0), 5), "\n")
  # repertorio
  cat("cumsum:", cumsum(c(3,1,4)), "| diff:", diff(c(3,1,4,1)), "\n")
  # precision: sum vs bucle
  x <- rep(0.1, 10)
  cat("sum(rep(0.1,10)) == 1:", sum(x) == 1,
      "| bucle:", { s <- 0; for (y in x) s <- s + y; s == 1 }, "\n")
}

demo_matrices <- function() {
  m <- matrix(1:12, 3)
  cat("column-major:", as.vector(m)[1:6], "\n")
  cat("m[,2]:", m[, 2], "| drop=FALSE dim:", dim(m[, 2, drop = FALSE]), "\n")
  a <- matrix(1:4, 2); b <- diag(2)
  cat("* elemento a elemento vs %*%:", identical(a * b, a %*% b), "(distintos salvo diag)\n")
  # sweep vs trampa
  X <- matrix(1:6, nrow = 2, byrow = TRUE)
  mc <- c(10, 20, 30)
  cat("X - v (trampa) fila1:", (X - mc)[1, ], "| sweep fila1:", sweep(X, 2, mc)[1, ], "\n")
  # scale
  Z <- scale(matrix(c(1,2,3,4,5,6), 3))
  cat("scale centrada:", round(colMeans(Z), 10), "\n")
  # apply vs rowSums
  set.seed(2026); M <- matrix(runif(3e6), ncol = 3)
  cat("apply(,1,sum) vs rowSums:",
      round(crono(apply(M, 1, sum)) / max(crono(rowSums(M)), 1e-4)), "x\n")
}

demo_arrays <- function() {
  A <- array(1:24, dim = c(2, 3, 4))
  cat("array dims:", dim(A), "| A[2,3,4]:", A[2, 3, 4], "\n")
  cat("apply margen c(1,2):\n"); print(apply(A, c(1, 2), mean))
  cat("aperm dims:", dim(aperm(A, c(2, 1, 3))), "\n")
}

demo_algebra <- function() {
  set.seed(2026)
  A <- matrix(rnorm(200*50), 200, 50)
  cat("crossprod == t(A)%*%A:", isTRUE(all.equal(crossprod(A), t(A) %*% A)), "\n")
  # sistema
  set.seed(2026); S <- matrix(rnorm(9), 3); S <- S %*% t(S) + diag(3); b <- c(1,2,3)
  cat("solve(A,b) residuo:", max(abs(S %*% solve(S, b) - b)), "\n")
  # minimos cuadrados
  set.seed(2026); n <- 100
  X <- cbind(1, rnorm(n), rnorm(n)); y <- X %*% c(2,-1,0.5) + rnorm(n, 0, 0.1)
  beta <- solve(crossprod(X), crossprod(X, y))
  cat("beta:", round(beta, 3), "| vs lm.fit:", round(lm.fit(X, y)$coefficients, 3), "\n")
  # descomposiciones
  Sd <- crossprod(matrix(rnorm(20), 5, 4))
  cat("eigen == svd (sim):", isTRUE(all.equal(eigen(Sd, symmetric=TRUE)$values, svd(Sd)$d)), "\n")
  cat("det via chol:", round(prod(diag(chol(Sd)))^2, 4), "| det:", round(det(Sd), 4), "\n")
  # transformacion
  th <- pi/4; Rot <- matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2)
  cat("rotar (1,0):", round(Rot %*% c(1,0), 4), "| det rotacion:", round(det(Rot), 4), "\n")
  # numero de condicion
  X2 <- cbind(1, 1:50); X2 <- cbind(X2, X2[,2] + rnorm(50, 0, 1e-7))
  cat("kappa casi colineal:", format(kappa(X2), scientific = TRUE, digits = 3), "\n")
}

demo_azar <- function() {
  set.seed(2026); a <- runif(3); set.seed(2026); b <- runif(3)
  cat("set.seed reproducible:", identical(a, b), "\n")
  cat("RNG:", RNGkind()[1], "\n")
  # Monte Carlo pi
  set.seed(2026); n <- 1e6; x <- runif(n); y <- runif(n)
  cat("pi ~", round(4 * mean(x^2 + y^2 <= 1), 4), "\n")
  # bootstrap vectorizado
  set.seed(2026); datos <- rnorm(1000, 50, 10); B <- 2000
  idx <- matrix(sample(length(datos), B*length(datos), TRUE), nrow = B)
  mb <- rowMeans(matrix(datos[idx], nrow = B))
  cat("IC bootstrap 95%:", round(quantile(mb, c(.025,.975)), 3), "\n")
  # TCL
  set.seed(2026); M <- matrix(rexp(10000*30, 1), nrow = 10000)
  medias <- rowMeans(M)
  asim <- function(z) mean((z-mean(z))^3)/sd(z)^3
  cat("asimetria media muestral (n=30):", round(asim(medias), 2), "(exp original ~1.9)\n")
  # distribuciones
  cat("qnorm(0.975):", round(qnorm(0.975), 3), "| pnorm(1.96):", round(pnorm(1.96), 3), "\n")
}

demo_dispersas <- function() {
  set.seed(2026); n <- 5000
  i <- sample(n, 25000, TRUE); j <- sample(n, 25000, TRUE); x <- runif(25000)
  S <- sparseMatrix(i = i, j = j, x = x, dims = c(n, n))
  D <- as.matrix(S)
  cat("dispersa vs densa memoria:",
      round(as.numeric(obj_size(D)) / as.numeric(obj_size(S))), "x\n")
  v <- runif(n)
  cat("producto disperso vs denso:",
      round(crono(for (k in 1:10) D %*% v) / max(crono(for (k in 1:10) S %*% v), 1e-4)), "x\n")
  cat("Diagonal(1e6):", format(obj_size(Diagonal(1e6))), "\n")
}

demo_integrador <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) { cat("(sin arrow)\n"); return(invisible()) }
  suppressPackageStartupMessages({library(arrow); library(dplyr)})
  candidatos <- c("data/processed/musica.parquet", "../data/processed/musica.parquet")
  real <- candidatos[file.exists(candidatos)][1]
  if (is.na(real)) { cat("(sin parquet)\n"); return(invisible()) }
  rasgos <- c("danceability","energy","loudness","speechiness","acousticness",
              "instrumentalness","liveness","valence","tempo")
  mus <- read_parquet(real, col_select = all_of(rasgos))
  set.seed(2026); muestra <- slice_sample(mus, n = 5000)
  Z <- scale(as.matrix(muestra[, rasgos]))
  R <- crossprod(Z) / (nrow(Z) - 1)
  ev <- eigen(R, symmetric = TRUE)
  cat("par mas correlacionado r:", round(max(R[upper.tri(R)]), 3), "\n")
  cat("2 PC var explicada:", round(100*sum(ev$values[1:2])/sum(ev$values), 1), "%\n")
  # coseno
  U <- Z / sqrt(rowSums(Z^2)); Sc <- tcrossprod(U)
  cat("vecino mas cercano de la pista 1: r =",
      round(sort(Sc[1, ], decreasing = TRUE)[2], 3), "\n")
  # reconstruccion
  for (k in c(2, 6)) {
    Zrec <- Z %*% ev$vectors[, 1:k] %*% t(ev$vectors[, 1:k])
    cat("reconstruccion k=", k, "RMSE:", round(sqrt(mean((Z - Zrec)^2)), 3), "\n")
  }
  # mahalanobis
  d2 <- mahalanobis(Z, colMeans(Z), cov(Z))
  cat("atipicos (chi2 9gl 0.999):", sum(d2 > qchisq(0.999, 9)), "de 5000\n")
}

if (sys.nframe() == 0L) {
  demos <- list(vectorizacion = demo_vectorizacion, matrices = demo_matrices,
                arrays = demo_arrays, algebra = demo_algebra, azar = demo_azar,
                dispersas = demo_dispersas, integrador = demo_integrador)
  for (nombre in names(demos)) {
    cat("\n==", nombre, "==\n")
    demos[[nombre]]()
  }
}
