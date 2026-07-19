# binder/install.R — paquetes para ejecutar el código del libro en Binder.
# Se instalan desde el snapshot de fecha fijado en runtime.txt (reproducible).
install.packages(c(
  # núcleo tidyverse + utilidades
  "tidyverse", "fs", "glue", "cli", "withr", "janitor", "scales",
  "patchwork", "gt",
  # datos: formatos y motores
  "arrow", "duckdb", "duckplyr", "readr", "jsonlite", "DBI",
  # estadística e inferencia
  "infer", "effectsize", "broom",
  # modelado
  "tidymodels", "ranger", "xgboost", "vip",
  # objetos, contrato y calidad
  "S7", "checkmate", "pointblank",
  # temporal
  "lubridate", "tsibble", "slider",
  # pruebas y reproducibilidad
  "testthat", "renv", "targets"
))
