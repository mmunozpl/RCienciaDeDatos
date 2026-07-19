# src/cap01_entorno.R -- identidad del entorno y comprobacion del determinismo.
# Acompana al cap. 1: lee que R y que paquetes hay, y verifica que una
# generacion con semilla fija es reproducible bit a bit.

library(dplyr)
library(readr)

#' Muestra la identidad del entorno (los pilares "codigo" y "entorno").
identidad_entorno <- function() {
  cat("R:      ", R.version.string, "\n")
  cat("plataforma:", R.version$platform, "\n")
  cat("RNG:    ", RNGkind()[1], "\n")
  for (p in c("dplyr", "readr", "ggplot2")) {
    if (requireNamespace(p, quietly = TRUE)) {
      cat(sprintf("%-8s %s\n", p, as.character(packageVersion(p))))
    }
  }
  invisible(NULL)
}

#' Genera una tabla sintetica de pistas con semilla fija (reproducible).
generar <- function(n = 500, semilla = 2026) {
  set.seed(semilla)
  genero <- sample(c("pop", "rock", "classical"), n, replace = TRUE)
  base <- case_when(
    genero == "pop" ~ 0.72,
    genero == "rock" ~ 0.85,
    TRUE ~ 0.30
  )
  energy <- pmin(pmax(base + rnorm(n, 0, 0.08), 0), 1)
  tibble(genero = genero, energy = round(energy, 3))
}

#' Resume la energia por genero (consume el artefacto, no lo regenera).
resumir <- function(pistas) {
  pistas |>
    group_by(genero) |>
    summarise(
      media = round(mean(energy), 2),
      mediana = round(median(energy), 2),
      n = n()
    )
}

#' Verifica el determinismo: dos generaciones con la misma semilla coinciden.
comprobar_determinismo <- function() {
  a <- generar()
  b <- generar()
  identical(a, b)
}

if (sys.nframe() == 0L) {
  identidad_entorno()
  cat("\ndeterminismo (dos corridas identicas):", comprobar_determinismo(), "\n\n")
  pistas <- generar()
  print(resumir(pistas))
}
