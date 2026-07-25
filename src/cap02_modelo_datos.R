# src/cap02_modelo_datos.R -- el modelo de datos de R en accion.
# Acompana al cap. 2: vectorizacion, reciclaje, indexacion, coercion, NA
# tipado, logica trivalente, texto, atributos y fechas, factores, listas,
# copy-on-modify, environments y evaluacion perezosa. Cada bloque imprime lo
# que el capitulo cita.

suppressPackageStartupMessages({
  library(lobstr)
  library(dplyr)
  library(forcats)
})

demo_tipos <- function() {
  cat("typeof(1L):", typeof(1L), "| typeof(1):", typeof(1), "\n")
  cat("c(TRUE, 1L, 2.5):", paste(c(TRUE, 1L, 2.5), collapse = " "), "\n")
  cat("c(TRUE, 1L, 2.5, 'x'):", paste(c(TRUE, 1L, 2.5, "x"), collapse = " "), "\n")
  cat("sum(c(TRUE, TRUE, FALSE, TRUE)):", sum(c(TRUE, TRUE, FALSE, TRUE)), "\n")
  cat("as.numeric('3,14'):", suppressWarnings(as.numeric("3,14")),
      "<- coma decimal: NA con aviso\n")
}

demo_reciclaje <- function() {
  cat("1:6 + c(10, 100):", paste(1:6 + c(10, 100), collapse = " "), "\n")
  cat("rep(c(10,100), times=3):", paste(rep(c(10, 100), times = 3), collapse = " "), "\n")
  cat("rep(c(10,100), each=3): ", paste(rep(c(10, 100), each = 3), collapse = " "), "\n")
  cat("seq_len(0): longitud", length(seq_len(0)), "| 1:0 =",
      paste(1:0, collapse = " "), "<- la trampa\n")
}

demo_indexar <- function() {
  x <- c(10, 20, 30, 40, 50)
  cat("x[c(1,3)]:", paste(x[c(1, 3)], collapse = " "),
      "| x[c(-1,-5)]:", paste(x[c(-1, -5)], collapse = " "),
      "| x[x > 25]:", paste(x[x > 25], collapse = " "), "\n")
  names(x) <- c("a", "b", "c", "d", "e")
  cat("x[c('b','d')]:", paste(x[c("b", "d")], collapse = " "),
      "| x[7]:", x[7], "<- fuera de rango: NA\n")
}

demo_na <- function() {
  cat("NA + 1:", NA + 1, "| NA > 3:", NA > 3, "\n")
  cat("mean(c(1, 2, NA)):", mean(c(1, 2, NA)), "\n")
  cat("mean(c(1, 2, NA), na.rm = TRUE):", mean(c(1, 2, NA), na.rm = TRUE), "\n")
  cat("cumsum(c(1,2,NA,4)):", paste(cumsum(c(1, 2, NA, 4)), collapse = " "), "\n")
  cat("which(is.na(c(3,NA,5,NA,7))):",
      paste(which(is.na(c(3, NA, 5, NA, 7))), collapse = " "), "\n")
  cat("complete.cases(c(34,NA,41,29), c('ES','FR',NA,'PT')):",
      paste(complete.cases(c(34, NA, 41, 29), c("ES", "FR", NA, "PT")),
            collapse = " "), "\n")
  cat("coalesce(c(10,NA,30), c(1,2,3)):",
      paste(coalesce(c(10, NA, 30), c(1, 2, 3)), collapse = " "), "\n")
}

demo_logicos <- function() {
  cat("FALSE & NA:", FALSE & NA, "| TRUE & NA:", TRUE & NA,
      "| TRUE | NA:", TRUE | NA, "| FALSE | NA:", FALSE | NA, "\n")
  en <- c(0.9, 0.4, NA, 0.7)
  cat("if_else(en>0.6,'alta','baja','sin dato'):",
      paste(if_else(en > 0.6, "alta", "baja", missing = "sin dato"),
            collapse = " "), "\n")
  cat("case_when:",
      paste(case_when(en >= 0.8 ~ "alta", en >= 0.5 ~ "media",
                      is.na(en) ~ "sin dato", .default = "baja"),
            collapse = " "), "\n")
  f <- as.Date(c("2026-01-15", "2026-03-02"))
  cat("ifelse pierde la clase Date:", ifelse(TRUE, f[1], f[2]),
      "| if_else la conserva:", format(if_else(TRUE, f[1], f[2])), "\n")
  g <- c("rock", "pop", "jazz", "pop", "classical")
  cat("which(generos=='pop'):", paste(which(g == "pop"), collapse = " "),
      "| match(c('jazz','pop'), g):", paste(match(c("jazz", "pop"), g),
                                            collapse = " "), "\n")
}

demo_orden <- function() {
  e <- c(0.7, 0.2, 0.9, 0.4)
  cat("sort(e):", paste(sort(e), collapse = " "),
      "| order(e):", paste(order(e), collapse = " "),
      "| rank(e):", paste(rank(e), collapse = " "), "\n")
  cat("sort(c(2,NA,1)): longitud", length(sort(c(2, NA, 1))),
      "<- el NA desaparece\n")
  g <- c("pop", "rock", "pop", "jazz", "pop")
  cat("unique(g):", paste(unique(g), collapse = " "),
      "| sum(duplicated(g)):", sum(duplicated(g)), "\n")
  cat("setdiff(c('pop','rock'), c('rock','jazz')):",
      setdiff(c("pop", "rock"), c("rock", "jazz")), "\n")
  cat("vacios: sum(numeric(0)) =", sum(numeric(0)),
      "| prod =", prod(numeric(0)),
      "| max =", suppressWarnings(max(numeric(0))), "\n")
}

demo_texto <- function() {
  cat("trimws('  Programar en R  '): '", trimws("  Programar en R  "), "'\n", sep = "")
  cat("nchar('café'):", nchar("café"),
      "| en bytes:", nchar("café", type = "bytes"), "\n")
  cat("gsub('-','_','hard-rock-duro'):", gsub("-", "_", "hard-rock-duro"), "\n")
  cat("grepl('rock', ...):",
      paste(grepl("rock", c("hard-rock", "pop", "rockabilly")), collapse = " "), "\n")
  cat("sprintf('%05.1f|%+d', 3.14, 42L):", sprintf("%05.1f|%+d", 3.14, 42L), "\n")
  cat("format(1234567.891, big.mark=' '):",
      format(1234567.891, big.mark = " ", decimal.mark = ","), "\n")
}

demo_atributos <- function() {
  v <- structure(1:4, unidades = "ms")
  cat("attrs de v*2:", paste(names(attributes(v * 2)), collapse = ", "),
      "| attrs de v[1:2]:",
      if (is.null(attributes(v[1:2]))) "NULL (perdidos)" else "conservados", "\n")
  d <- as.Date("2026-07-19")
  cat("fecha: typeof", typeof(d), "| class", class(d),
      "| unclass:", unclass(d), "| d+7:", format(d + 7), "\n")
  t <- as.POSIXct("2026-07-19 12:00:00", tz = "UTC")
  cat("POSIXct desnudo:", unclass(t)[1], "segundos desde 1970\n")
}

demo_factores <- function() {
  g <- factor(c("pop", "rock", "pop", "jazz", "pop", "rock"))
  print(table(g))
  cat("fct_infreq:", paste(levels(fct_infreq(g)), collapse = " "), "\n")
  gg <- factor(c("pop", "rock", "jazz", "blues", "ska", "pop", "pop", "rock"))
  print(table(fct_lump_n(gg, n = 2)))
  niv <- cut(c(0.72, 0.85, 0.41, 0.20, 0.95), breaks = c(0, 0.5, 0.8, 1),
             labels = c("baja", "media", "alta"))
  print(table(niv))
  h <- factor(c("10", "20", "30"))
  cat("as.integer(factor):", paste(as.integer(h), collapse = " "),
      "| via as.character:", paste(as.integer(as.character(h)), collapse = " "), "\n")
}

demo_listas <- function() {
  notas <- list(a = 1:3, b = 4:6)
  cat("vapply(notas, mean, numeric(1)):",
      paste(vapply(notas, mean, numeric(1)), collapse = " "), "\n")
  df <- data.frame(genero = c("pop", "rock"), energia = c(0.73, 0.85))
  cat("typeof(df):", typeof(df), "| class:", class(df),
      "| length (columnas):", length(df), "\n")
  cat("df$ener (emparejamiento parcial!):",
      paste(df$ener, collapse = " "),
      "| df[['ener']] es NULL:", is.null(df[["ener"]]), "\n")
}

demo_cow <- function() {
  x <- c(1, 2, 3)
  y <- x
  cat("comparten (x, y):", obj_addr(x) == obj_addr(y), "\n")
  y[1] <- 99
  cat("tras y[1] <- 99, comparten:", obj_addr(x) == obj_addr(y), "\n")
  cat("x intacto:", paste(x, collapse = " "), "\n")
  z <- runif(1e6)
  cat("obj_size(z):", format(obj_size(z)),
      "| obj_size(list(z,z,z)):", format(obj_size(list(z, z, z))),
      "<- referencias, no copias\n")
  cat("obj_size(1:1e6):", format(obj_size(1:1e6)),
      "| materializada:", format(obj_size(c(1:1e6))), "<- ALTREP\n")
}

demo_environments <- function() {
  contador <- function() {
    n <- 0
    function() { n <<- n + 1; n }
  }
  tic <- contador(); tic(); tic()
  cat("contador tras 3 llamadas:", tic(), "\n")
  otro <- contador()
  cat("otro contador (independiente):", otro(), "\n")
  z <- local({ a <- 10; b <- 20; a + b })
  cat("local({...}):", z, "| exists('a'):", exists("a"), "\n")
}

demo_promesas <- function() {
  perezoso <- function(a, b) a
  cat("perezoso(1, stop('boom')):", perezoso(1, stop("boom")), "\n")
  fs <- lapply(1:3, function(i) { force(i); function() i })
  cat("closures con force:", paste(sapply(fs, function(f) f()), collapse = " "), "\n")
  correlacion <- function(metodo = c("pearson", "spearman")) match.arg(metodo)
  cat("match.arg por defecto:", correlacion(), "\n")
}

#' Limpia una columna de energia leida como texto (ejemplo integrador).
limpiar_energia <- function(x) {
  num <- as.numeric(x)                          # "" -> NA_real_
  fuera <- !is.na(num) & (num < 0 | num > 1)    # la energia vive en [0, 1]
  if (any(fuera)) warning(sum(fuera), " valor(es) fuera de [0,1] -> NA")
  num[fuera] <- NA_real_
  num
}

demo_integrador <- function() {
  crudo_energia <- c("0.72", "0.85", "", "0.41", "1.30", "0.20")
  crudo_genero  <- c("Pop", "rock", "POP", "jazz", "rock", "pop")
  energia <- suppressWarnings(limpiar_energia(crudo_energia))
  genero  <- factor(tolower(trimws(crudo_genero)))
  pistas  <- data.frame(genero, energia)
  print(pistas)
  print(tapply(pistas$energia, pistas$genero, mean, na.rm = TRUE))
  cat("filas completas:", sum(complete.cases(pistas)),
      "| original intacto:",
      identical(crudo_energia, c("0.72", "0.85", "", "0.41", "1.30", "0.20")), "\n")
}

if (sys.nframe() == 0L) {
  demos <- list(tipos = demo_tipos, reciclaje = demo_reciclaje,
                indexar = demo_indexar, na = demo_na, logicos = demo_logicos,
                orden = demo_orden, texto = demo_texto,
                atributos = demo_atributos, factores = demo_factores,
                listas = demo_listas, cow = demo_cow,
                environments = demo_environments, promesas = demo_promesas,
                integrador = demo_integrador)
  for (nombre in names(demos)) {
    cat("\n==", nombre, "==\n")
    demos[[nombre]]()
  }
}
