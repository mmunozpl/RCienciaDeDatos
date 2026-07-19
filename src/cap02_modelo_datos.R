# src/cap02_modelo_datos.R -- el modelo de datos de R en accion.
# Acompana al cap. 2: vectorizacion, coercion, NA tipado, copy-on-modify,
# factores y evaluacion perezosa. Cada bloque imprime lo que el capitulo cita.

library(lobstr)

demo_tipos <- function() {
  cat("typeof(1L):", typeof(1L), "| typeof(1):", typeof(1), "\n")
  cat("c(TRUE, 1L, 2.5):", paste(c(TRUE, 1L, 2.5), collapse = " "), "\n")
  cat("c(TRUE, 1L, 2.5, 'x'):", paste(c(TRUE, 1L, 2.5, "x"), collapse = " "), "\n")
  cat("sum(c(TRUE, TRUE, FALSE, TRUE)):", sum(c(TRUE, TRUE, FALSE, TRUE)), "\n")
}

demo_na <- function() {
  cat("NA + 1:", NA + 1, "| NA > 3:", NA > 3, "\n")
  cat("mean(c(1, 2, NA)):", mean(c(1, 2, NA)), "\n")
  cat("mean(c(1, 2, NA), na.rm = TRUE):", mean(c(1, 2, NA), na.rm = TRUE), "\n")
}

demo_cow <- function() {
  x <- c(1, 2, 3)
  y <- x
  cat("comparten (x, y):", obj_addr(x) == obj_addr(y), "\n")
  y[1] <- 99
  cat("tras y[1] <- 99, comparten:", obj_addr(x) == obj_addr(y), "\n")
  cat("x intacto:", paste(x, collapse = " "), "\n")
}

#' Limpia una columna de energia leida como texto (ejemplo integrador).
limpiar_energia <- function(x) {
  num <- as.numeric(x)      # "" -> NA_real_
  num[num > 1] <- NA_real_  # la energia vive en [0, 1]
  num
}

if (sys.nframe() == 0L) {
  demo_tipos(); cat("\n"); demo_na(); cat("\n"); demo_cow(); cat("\n")
  crudo <- c("0.72", "0.85", "", "1.30", "0.20")
  energia <- suppressWarnings(limpiar_energia(crudo))
  cat("energia:", paste(energia, collapse = " "), "\n")
  cat("ausentes:", sum(is.na(energia)),
      "| media (na.rm):", round(mean(energia, na.rm = TRUE), 2),
      "| original intacto:", identical(crudo, c("0.72", "0.85", "", "1.30", "0.20")), "\n")
}
