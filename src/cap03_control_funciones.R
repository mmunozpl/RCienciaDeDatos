# src/cap03_control_funciones.R -- control de flujo, funciones y condiciones.
# Acompana al cap. 3: if/switch, bucles, vectorizacion, funciones, estilo
# funcional (purrr y base), contratos, condiciones y depuracion. Cada demo
# imprime lo que el capitulo cita.

suppressPackageStartupMessages({
  library(purrr)
  library(rlang)
  library(checkmate)
  library(memoise)
})

demo_if_switch <- function() {
  x <- 7
  etiqueta <- if (x > 5) "grande" else "pequeno"
  cat("if como expresion:", etiqueta, "\n")
  describe <- function(genero) {
    switch(genero,
      pop   = "melodias pegadizas",
      rock  = ,
      metal = "guitarras",
      "otro estilo")
  }
  cat("switch:", describe("pop"), "|", describe("rock"), "|", describe("jazz"), "\n")
}

demo_bucles <- function() {
  medias <- c(pop = 0.73, rock = 0.85, jazz = 0.41)
  acum <- numeric(length(medias))
  for (i in seq_along(medias)) acum[i] <- medias[[i]] * 100
  cat("for preasignado:", paste(acum, collapse = " "), "\n")
  n <- 1
  while (n < 100) n <- n * 2
  cat("while:", n, "\n")
  i <- 0
  repeat { i <- i + 1; if (i %% 2 == 0) next; if (i > 7) break }
  cat("repeat/next/break:", i, "\n")
  suaviza <- function(x, alfa = 0.3) {
    s <- numeric(length(x)); s[1] <- x[1]
    for (k in 2:length(x)) s[k] <- alfa * x[k] + (1 - alfa) * s[k - 1]
    s
  }
  cat("suaviza:", paste(round(suaviza(c(10, 12, 9, 14)), 2), collapse = " "), "\n")
  raiz <- function(a, tol = 1e-10) {
    x <- a / 2
    repeat {
      mejora <- (x + a / x) / 2
      if (abs(mejora - x) < tol) return(mejora)
      x <- mejora
    }
  }
  cat("newton raiz(2):", round(raiz(2), 6),
      "| identico a sqrt en 10 dec:", round(raiz(2), 10) == round(sqrt(2), 10), "\n")
}

demo_funciones <- function() {
  raiz_segura <- function(x) { if (x < 0) return(NA_real_); sqrt(x) }
  cat("salida temprana:", raiz_segura(-1), raiz_segura(9), "\n")
  potencia <- function(base, exponente = 2) base^exponente
  cat("parcial de argumentos (expo=5):", potencia(2, expo = 5), "\n")
  resumen <- function(x, ...) mean(x, ...)
  cat("dots bien:", resumen(c(1, 2, NA, 4), na.rm = TRUE),
      "| typo tragado:", resumen(c(1, 2, NA, 4), na.rn = TRUE), "\n")
  ajusta_recta <- function(x, y) {
    m <- lm(y ~ x)
    list(pendiente = unname(coef(m)[2]), r2 = summary(m)$r.squared, n = length(x))
  }
  set.seed(2026)
  x <- runif(50); y <- 2 * x + rnorm(50, 0, 0.1)
  r <- ajusta_recta(x, y)
  cat(sprintf("lista con nombres: pendiente %.2f | r2 %.3f | n %d\n",
              r$pendiente, r$r2, r$n))
}

demo_funcional <- function() {
  cat("Filter:", paste(Filter(\(x) x > 0.5, c(0.73, 0.41, 0.85)), collapse = " "), "\n")
  cat("Reduce acumulado:", paste(Reduce(`+`, 1:5, accumulate = TRUE), collapse = " "), "\n")
  cat("Reduce(intersect):",
      paste(Reduce(intersect, list(c("pop","rock","jazz"),
                                   c("rock","jazz","ska"),
                                   c("jazz","rock"))), collapse = " "), "\n")
  notas <- list(a = 1:3, b = 4:6)
  cat("map_dbl:", paste(map_dbl(notas, mean), collapse = " "), "\n")
  cat("pmap_dbl:", paste(pmap_dbl(list(1:2, 3:4, 5:6), \(a, b, c) a + b + c),
                         collapse = " "), "\n")
  cat("accumulate (= suaviza):",
      paste(round(accumulate(c(10, 12, 9, 14), \(acum, x) 0.3 * x + 0.7 * acum), 2),
            collapse = " "), "\n")
  df <- data.frame(genero = c("pop", "rock", "pop"),
                   energia = c(0.72, 0.85, 0.20),
                   duracion = c(201, 355, 168))
  cat("map sobre columnas:",
      paste(round(map_dbl(keep(df, is.numeric), mean), 2), collapse = " "), "\n")
  pistas <- data.frame(genero = c("pop","rock","pop","jazz","rock"),
                       energia = c(0.72, 0.85, 0.20, 0.41, 0.88))
  tabla <- imap(split(pistas, pistas$genero),
                \(d, g) data.frame(genero = g, media = mean(d$energia))) |> list_rbind()
  print(tabla)
}

demo_adverbios <- function() {
  log_pos <- possibly(log, otherwise = NA_real_)
  cat("possibly con 'a':", log_pos("a"), "\n")
  r <- safely(log)("a")
  cat("safely: result nulo:", is.null(r$result), "| hay error:", !is.null(r$error), "\n")
  fib <- function(n) if (n < 2) n else fib(n - 1) + fib(n - 2)
  v1 <- fib(20)
  fib <- memoise(fib)
  cat("fib(20):", v1, "== memoizada:", fib(20) == v1, "\n")
  set.seed(11)
  inestable <- function() { if (runif(1) < 0.5) stop("fallo transitorio"); "exito" }
  con_reintento <- function(f, intentos = 5) {
    function(...) {
      for (i in seq_len(intentos)) {
        r <- tryCatch(f(...), error = function(e) NULL)
        if (!is.null(r)) { cat("  intento", i, ": exito\n"); return(invisible(r)) }
        cat("  intento", i, ": fallo\n")
      }
      stop("agotados ", intentos, " intentos")
    }
  }
  con_reintento(inestable)()
}

demo_contratos <- function() {
  normalizar <- function(x) {
    stopifnot(is.numeric(x), length(x) > 0, !anyNA(x))
    out <- (x - min(x)) / (max(x) - min(x))
    stopifnot("salida fuera de [0,1]" = all(out >= 0 & out <= 1))
    out
  }
  cat("normalizar:", paste(normalizar(c(2, 4, 6)), collapse = " "), "\n")
  cat("contrato:", tryCatch(normalizar("a"), error = conditionMessage), "\n")
  proporcion_altas <- function(x, umbral = 0.5) {
    assert_numeric(x, lower = 0, upper = 1, any.missing = FALSE)
    mean(x > umbral)
  }
  cat("checkmate ok:", round(proporcion_altas(c(0.2, 0.7, 0.9)), 3), "\n")
  cat("checkmate mal:", tryCatch(proporcion_altas(c(0.2, 1.7)),
                                 error = conditionMessage), "\n")
}

demo_condiciones <- function() {
  seguro <- tryCatch(log("a"),
                     error = function(e) NA_real_,
                     finally = cat("  (finally corrio)\n"))
  cat("tryCatch:", seguro, "\n")
  wch <- withCallingHandlers({ warning("primera"); "el codigo CONTINUO" },
                             warning = function(w) invokeRestart("muffleWarning"))
  cat("withCallingHandlers:", wch, "\n")
  con_registro <- function(expr) {
    avisos <- character(0)
    resultado <- withCallingHandlers(expr,
      warning = function(w) { avisos <<- c(avisos, conditionMessage(w))
                              invokeRestart("muffleWarning") })
    list(resultado = resultado, avisos = avisos)
  }
  r <- con_registro(as.numeric(c("3.14", "x")))
  cat("recolector: avisos capturados:", length(r$avisos), "\n")
  lee_columna <- function(col) {
    abort(paste0("la columna '", col, "' no existe"),
          class = "error_columna", columna = col)
  }
  rec <- tryCatch(lee_columna("energia"),
                  error_columna = function(e) paste("recuperado:", e$columna))
  cat("abort por clase:", rec, "\n")
}

demo_ficheros <- function() {
  dir <- tempfile(); dir.create(dir)
  writeLines("energia\n0.7\n0.9", file.path(dir, "lote1.csv"))
  writeLines("energia\n0.4", file.path(dir, "lote2.csv"))
  writeLines("esto no es un csv;;;", file.path(dir, "roto.csv"))
  lee_lote <- function(f) {
    d <- read.csv(f)
    if (!"energia" %in% names(d) || !is.numeric(d$energia))
      abort(paste0(basename(f), ": sin columna numerica energia"),
            class = "error_contrato")
    d
  }
  fs <- list.files(dir, pattern = "[.]csv$", full.names = TRUE)
  res <- set_names(map(fs, safely(lee_lote)), basename(fs))
  ok <- map_lgl(res, \(r) is.null(r$error))
  cat("procesados:", sum(ok), "| rechazados:", sum(!ok), "\n")
  cat("medias:", paste(round(map_dbl(res[ok], \(r) mean(r$result$energia)), 2),
                       collapse = " "), "\n")
  cat("motivo:", map_chr(res[!ok], \(r) conditionMessage(r$error)), "\n")
}

demo_integrador <- function() {
  lotes <- list(ok1 = c(0.72, 0.85, 0.41), vacio = numeric(0),
                malo = c(0.5, 1.7, 0.3), ok2 = c(0.20, 0.95))
  media_energia <- function(x) {
    if (length(x) == 0) abort("lote vacio", class = "error_vacio")
    if (any(x < 0 | x > 1)) abort("energia fuera de [0,1]", class = "error_rango")
    mean(x)
  }
  resultado <- imap(lotes, \(x, nombre) {
    tryCatch(
      list(lote = nombre, media = media_energia(x), estado = "ok"),
      error_vacio = \(e) list(lote = nombre, media = NA_real_, estado = "vacio"),
      error_rango = \(e) list(lote = nombre, media = NA_real_,
                              estado = "fuera de rango"))
  })
  tabla <- list_rbind(map(resultado, as.data.frame))
  print(tabla)
  cat(sprintf("media global (lotes ok): %.3f\n", mean(tabla$media, na.rm = TRUE)))
}

demo_bootstrap <- function() {
  set.seed(2026)
  energia <- runif(200, 0.2, 0.95)
  remuestrea <- function(x) mean(sample(x, length(x), replace = TRUE))
  boot <- replicate(5000, remuestrea(energia))
  cat("media observada:", round(mean(energia), 3), "\n")
  cat("IC bootstrap 95%:", paste(round(quantile(boot, c(0.025, 0.975)), 3),
                                 collapse = " - "), "\n")
  cat("error tipico:", round(sd(boot), 4), "\n")
}

if (sys.nframe() == 0L) {
  demos <- list(if_switch = demo_if_switch, bucles = demo_bucles,
                funciones = demo_funciones, funcional = demo_funcional,
                adverbios = demo_adverbios, contratos = demo_contratos,
                condiciones = demo_condiciones, ficheros = demo_ficheros,
                integrador = demo_integrador, bootstrap = demo_bootstrap)
  for (nombre in names(demos)) {
    cat("\n==", nombre, "==\n")
    demos[[nombre]]()
  }
}
