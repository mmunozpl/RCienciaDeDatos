# src/cap04_estructuras.R -- estructuras integradas y su coste.
# Acompana al cap. 4: reproduce cada experimento del capitulo (tiempos como
# cocientes; los absolutos dependen de la maquina). Semillas fijas.

suppressPackageStartupMessages({
  library(lobstr)
  library(purrr)
  library(rlang)
})
crono <- function(expr) unname(system.time(expr)["elapsed"])

demo_tamanos <- function() {
  cat("cabecera:", format(obj_size(numeric(0))),
      "| int 1e6:", format(obj_size(integer(1e6))),
      "| dbl 1e6:", format(obj_size(numeric(1e6))),
      "| lgl 1e6:", format(obj_size(logical(1e6))), "\n")
  S <- sample(1e7, 1e5)
  presente <- raw(1e7); presente[S] <- as.raw(1)
  cat("bitmap raw 1e7:", format(obj_size(presente)),
      "(logical:", format(obj_size(logical(1e7))), ")\n")
}

demo_crecer <- function() {
  crecer <- function(n) { v <- c(); for (i in 1:n) v <- c(v, i); v }
  prea   <- function(n) { v <- numeric(n); for (i in 1:n) v[i] <- i; v }
  t2k <- crono(crecer(2000)); t8k <- crono(crecer(8000)); t32 <- crono(crecer(32000))
  p32 <- crono(prea(32000))
  cat("crecer 2k/8k/32k:", t2k, t8k, t32,
      "| cocientes:", round(t8k/max(t2k,1e-4),1), round(t32/max(t8k,1e-4),1), "\n")
  cat("crecer(32k)/prea(32k):", round(t32/max(p32,1e-4)), "\n")
  crecer_l <- function(n) { l <- list(); for (i in 1:n) l[[length(l)+1]] <- i; l }
  prea_l   <- function(n) { l <- vector("list", n); for (i in 1:n) l[[i]] <- i; l }
  cat("listas 32k crecer/prea:",
      round(crono(crecer_l(32000))/max(crono(prea_l(32000)),1e-4)), "\n")
}

demo_copia_escala <- function() {
  x <- numeric(1e8)
  t1 <- crono(x[1] <- 1)
  y <- x
  t2 <- crono(y[2] <- 2)
  cat("modificar 1 celda de 1e8: in situ", t1, "s | con 2a referencia", t2, "s\n")
  rm(x, y); invisible(gc())
}

demo_pertenencia <- function() {
  set.seed(2026)
  S <- sample(1e7, 1e5); q <- sample(S, 1e4)
  t_b <- crono({ r1 <- logical(length(q)); for (i in seq_along(q)) r1[i] <- q[i] %in% S })
  t_v <- crono(r2 <- q %in% S)
  cat("bucle vs lote:", t_b, "/", t_v, "=", round(t_b/max(t_v,1e-4)), "x\n")
  So <- sort(S)
  t_f <- crono(pos <- findInterval(q, So))
  cat("findInterval 10k:", t_f, "s | hallados:", all(So[pmax(pos,1)] == q), "\n")
  idx <- new.env(hash = TRUE)
  for (s in as.character(S)) assign(s, TRUE, envir = idx)
  t_e <- crono(vapply(as.character(q), exists, logical(1), envir = idx, inherits = FALSE))
  cat("10k consultas al entorno:", t_e, "s\n")
  h <- utils::hashtab(); utils::sethash(h, "pop", 3); utils::sethash(h, "rock", 2)
  total <- 0; utils::maphash(h, function(k, v) total <<- total + v)
  cat("maphash suma:", total, "| numhash:", utils::numhash(h), "\n")
}

demo_diccionario <- function() {
  nm <- sprintf("k%05d", 1:10000)
  lst <- setNames(as.list(seq_along(nm)), nm)
  env <- list2env(lst, hash = TRUE)
  claves <- sample(nm, 2000)
  t_l <- crono(for (k in claves) lst[[k]])
  t_e <- crono(for (k in claves) get(k, envir = env, inherits = FALSE))
  cat("2000 lookups lista/entorno:", t_l, "/", t_e, "=",
      round(t_l/max(t_e,1e-4)), "x\n")
  mapa <- c(POP = "pop", RCK = "rock", JZZ = "jazz")
  cat("traduccion:", paste(unname(mapa[c("RCK","POP","XXX")]), collapse = " "), "\n")
}

demo_contar <- function() {
  print(table(c("pop","rock","pop","jazz","pop","rock")))
  r <- rle(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE))
  cat("racha TRUE mas larga:", max(r$lengths[r$values]), "\n")
  gen <- c("pop","rock","pop","jazz","pop")
  idx_gen <- split(seq_along(gen), gen)
  cat("indice invertido pop:", paste(idx_gen[["pop"]], collapse = " "), "\n")
  x <- c(0.72, 0.85, 0.20, 0.41); f <- c("pop","rock","pop","jazz")
  tr <- lapply(split(x, f), \(v) v / max(v))
  cat("unsplit conserva orden:", paste(round(unsplit(tr, f), 2), collapse = " "), "\n")
  set.seed(2026)
  g1e6 <- sample(c("pop","rock","jazz","classical","metal"), 1e6, TRUE)
  t_t <- crono(table(g1e6))
  cat("table 1e6:", t_t, "s\n")
}

demo_acumulados <- function() {
  set.seed(2026)
  dur <- round(runif(1e6, 120, 420))
  acum <- c(0, cumsum(dur))
  suma_rango <- function(i, j) acum[j + 1] - acum[i]
  stopifnot(suma_rango(1, 5) == sum(dur[1:5]))
  t_d <- crono(for (k in 1:2000) sum(dur[1000:500000]))
  t_p <- crono(for (k in 1:2000) suma_rango(1000, 500000))
  cat("2000 sumas de rango directo/prefix:", round(t_d/max(t_p,1e-4)), "x\n")
  x <- c(10, 12, 9, 14, 11)
  cat("media movil embed:", paste(round(rowMeans(embed(x, 3)), 2), collapse = " "), "\n")
}

demo_orden <- function() {
  set.seed(2026)
  x <- runif(1e6); k <- 5
  t_full <- crono(tf <- sort(x, decreasing = TRUE)[1:k])
  t_part <- crono({ u <- sort(x, partial = length(x)-k+1)[length(x)-k+1]
                    tp <- sort(x[x >= u], decreasing = TRUE)[1:k] })
  cat("top-5 completo/parcial:", t_full, "/", t_part,
      "| iguales:", identical(round(tf,12), round(tp,12)), "\n")
  cat("sort(1:1e8):", crono(invisible(sort(1:1e8))), "s (ALTREP sabe que esta ordenado)\n")
  cat("orden claves texto:", paste(sort(sprintf("t%03d", c(2,10,1))), collapse=" "), "\n")
}

demo_df <- function() {
  set.seed(2026)
  n <- 1e5
  df <- data.frame(id = sprintf("t%06d", 1:n),
                   genero = sample(c("pop","rock","jazz"), n, TRUE),
                   energia = runif(n))
  t_f <- crono({ s <- 0; for (i in 1:2000) s <- s + df[i, "energia"] })
  t_c <- crono(s2 <- sum(df$energia[1:2000]))
  cat("2000 celdas fila-a-fila vs columna:", round(t_f/max(t_c,1e-4)), "x\n")
  trozo <- df[1:10, ]
  t_rb <- crono({ acc <- trozo[0, ]; for (k in 1:300) acc <- rbind(acc, trozo) })
  t_lb <- crono({ l <- vector("list", 300); for (k in 1:300) l[[k]] <- trozo
                  acc2 <- do.call(rbind, l) })
  cat("300 rbind acumulando vs lista+rbind:", round(t_rb/max(t_lb,1e-4)), "x\n")
  df$etiquetas <- I(replicate(n, character(0), simplify = FALSE))
  cat("columna-lista creada:", is.list(df$etiquetas), "\n")
}

demo_texto <- function() {
  set.seed(2026)
  textos <- paste0("pista-", sample(1e6), "-", sample(c("pop","rock","jazz"), 1e6, TRUE))
  t_r <- crono(a <- grepl("rock", textos))
  t_f <- crono(b <- grepl("rock", textos, fixed = TRUE))
  cat("grepl regex/fixed:", round(t_r/max(t_f,1e-4), 1), "x | identicos:",
      identical(a, b), "\n")
  gen <- sample(c("pop","rock","jazz","classical","metal"), 1e6, TRUE)
  cat("character 1e6:", format(obj_size(gen)),
      "| factor:", format(obj_size(factor(gen))), "\n")
}

demo_huellas <- function() {
  d1 <- data.frame(a = 1:3); d2 <- data.frame(a = 1:3)
  cat("hash iguales:", hash(d1) == hash(d2), "\n")
  cache <- new.env(hash = TRUE)
  lenta <- function(d) { Sys.sleep(0.2); nrow(d) }
  con_cache <- function(d) {
    k <- hash(d)
    if (!exists(k, envir = cache, inherits = FALSE)) assign(k, lenta(d), envir = cache)
    get(k, envir = cache, inherits = FALSE)
  }
  t1 <- crono(con_cache(d1)); t2 <- crono(con_cache(d2))
  cat("cache por huella: 1a", round(t1,2), "s | 2a", round(t2,3), "s\n")
}

demo_integrador <- function() {
  set.seed(2026)
  n <- 50000
  ids <- sprintf("t%06d", sample(1:60000, n, replace = TRUE))
  gen <- sample(c("pop","rock","jazz","classical","metal"), n, TRUE,
                prob = c(.30,.25,.20,.15,.10))
  ener <- round(runif(n), 3)
  primera <- !duplicated(ids)
  ids2 <- ids[primera]; gen2 <- gen[primera]; ener2 <- ener[primera]
  cat("unicas:", length(ids2), "| duplicados:", sum(!primera), "\n")
  print(table(gen2))
  idx <- new.env(hash = TRUE)
  for (i in seq_along(ids2)) assign(ids2[i], i, envir = idx)
  fila <- get(ids2[100], envir = idx, inherits = FALSE)
  cat("indice:", ids2[100], "-> fila", fila, gen2[fila], ener2[fila], "\n")
  dur2 <- round(runif(length(ids2), 120, 420), 1)
  k <- 5
  u <- sort(dur2, partial = length(dur2)-k+1)[length(dur2)-k+1]
  cat("top-5 duracion:", paste(sort(dur2[dur2 >= u], TRUE)[1:k], collapse = " "), "\n")
  cat("<=180s:", findInterval(180, sort(dur2)), "de", length(dur2), "\n")
  medias <- vapply(split(ener2, gen2), mean, numeric(1))
  cat("genero mas energico:", names(which.max(medias)),
      round(max(medias), 3), "\n")
  cat("mediana a mano:", dur2[order(dur2)][(length(dur2)+1) %/% 2],
      "| median():", median(dur2), "(n par: interpola)\n")
  cat("sello del catalogo:", substr(hash(list(ids2, gen2, ener2)), 1, 12), "...\n")
}

if (sys.nframe() == 0L) {
  demos <- list(tamanos = demo_tamanos, crecer = demo_crecer,
                copia_escala = demo_copia_escala, pertenencia = demo_pertenencia,
                diccionario = demo_diccionario, contar = demo_contar,
                acumulados = demo_acumulados, orden = demo_orden,
                df = demo_df, texto = demo_texto, huellas = demo_huellas,
                integrador = demo_integrador)
  for (nombre in names(demos)) {
    cat("\n==", nombre, "==\n")
    demos[[nombre]]()
  }
}
