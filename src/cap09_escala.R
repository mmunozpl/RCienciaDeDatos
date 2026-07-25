# src/cap09_escala.R -- datos a gran escala con arrow, DuckDB y duckplyr.
# Acompana al cap. 9: reproduce las cifras (Arrow columnar, open_dataset perezoso,
# DuckDB SQL sobre Parquet, duckplyr con explain, particionado y poda, benchmark de
# los cuatro caminos, perfil del ano de los taxis). Cada cifra se ejecuto antes de
# imprimirse. El dataset de taxis (NYC yellow 2024, 41,17 M filas) es grande y
# externo; si no esta, las demos que lo usan se saltan con aviso.

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(DBI); library(duckdb)
})
crono <- function(expr) round(unname(system.time(expr)["elapsed"]), 2)

# --- localizar los datos (rutas relativas robustas) -------------------------
buscar <- function(cands) { r <- cands[file.exists(cands)][1]; if (is.na(r)) NULL else r }
MUS <- buscar(c("data/processed/musica.parquet", "../data/processed/musica.parquet"))
# el conjunto de taxis es externo: se descarga a data/nyc_taxi (ver README)
TAXI_DIR <- buscar(c("data/nyc_taxi", "../data/nyc_taxi"))
taxi_glob <- function() if (is.null(TAXI_DIR)) NULL else
  file.path(TAXI_DIR, "yellow_tripdata_2024-*.parquet")
taxi_files <- function() if (is.null(TAXI_DIR)) character(0) else
  list.files(TAXI_DIR, pattern = "yellow.*parquet$", full.names = TRUE)

# --- 1. Arrow: formato columnar y compresion --------------------------------
demo_arrow <- function() {
  tab <- read_parquet(MUS, as_data_frame = FALSE)
  cat("clase:", class(tab)[1], "| filas:", tab$num_rows,
      "| columnas:", tab$num_columns, "\n")
  col <- tab$popularity
  cat("una columna es un", class(col)[1], "de tipo", col$type$ToString(), "\n")
  # compresion: mismo dato, tres codecs
  df <- read_parquet(MUS)
  tmp <- tempfile()
  for (cod in c("uncompressed", "snappy", "zstd")) {
    f <- paste0(tmp, "_", cod, ".parquet")
    write_parquet(df, f, compression = cod)
    cat(sprintf("  %-13s %5.1f MB\n", cod, file.size(f) / 1e6))
    unlink(f)
  }
}

# --- 2. puente zero-copy: arrow Table -> DuckDB sin copiar -------------------
demo_puente <- function() {
  tab <- read_parquet(MUS, as_data_frame = FALSE)
  con <- dbConnect(duckdb())
  duckdb_register_arrow(con, "m", tab)          # sin copia
  r <- dbGetQuery(con, "SELECT count(*) n, round(avg(popularity),2) p FROM m")
  cat("DuckDB sobre la Table de arrow:", r$n, "filas, pop media", r$p, "\n")
  dbDisconnect(con, shutdown = TRUE)
}

# --- 3. arrow: open_dataset perezoso + pushdown -----------------------------
demo_dataset <- function() {
  ds <- open_dataset(taxi_files())
  cat("abrir dataset (solo metadatos):", format(nrow(ds), big.mark = " "),
      "filas x", ncol(ds), "columnas\n")
  r <- ds |> filter(trip_distance > 0, fare_amount > 0) |>
    summarise(n = n(), tarifa = round(mean(fare_amount), 2), .by = payment_type) |>
    arrange(desc(n)) |> collect()
  print(as.data.frame(r))
  t <- crono(ds |> select(trip_distance, fare_amount) |> collect())
  cat("leer 2 columnas de 19 (pushdown de proyeccion):", t, "s\n")
}

# --- 4. DuckDB: SQL analitico sobre Parquet ---------------------------------
demo_duckdb <- function() {
  con <- dbConnect(duckdb()); glob <- taxi_glob()
  n <- dbGetQuery(con, sprintf("SELECT count(*) n FROM read_parquet('%s')", glob))
  cat("count(*) instantaneo:", format(n$n, big.mark = " "), "\n")
  # propina por tipo de pago: el cero estructural del efectivo
  p <- dbGetQuery(con, sprintf("
    SELECT payment_type, count(*) n, round(avg(tip_amount),3) propina
    FROM read_parquet('%s') GROUP BY payment_type ORDER BY n DESC", glob))
  print(head(p, 3))
  cat("(efectivo=tipo 2: propina ~0 porque NO se registra)\n")
  # ventana: hora punta por tipo de pago
  w <- dbGetQuery(con, sprintf("
    SELECT payment_type, hora, viajes FROM (
      SELECT payment_type, extract(hour FROM tpep_pickup_datetime) hora,
             count(*) viajes,
             row_number() OVER (PARTITION BY payment_type ORDER BY count(*) DESC) rk
      FROM read_parquet('%s') GROUP BY payment_type, hora)
    WHERE rk = 1 ORDER BY viajes DESC LIMIT 2", glob))
  print(w)
  dbDisconnect(con, shutdown = TRUE)
}

# --- 5. duckplyr: dplyr con motor DuckDB, perezoso, explain -----------------
demo_duckplyr <- function() {
  if (!requireNamespace("duckplyr", quietly = TRUE)) {
    cat("(duckplyr no instalado)\n"); return(invisible()) }
  library(duckplyr)
  tv <- read_parquet_duckdb(taxi_glob())
  cat("clase:", paste(class(tv), collapse = " "), "\n")
  q <- tv |> filter(trip_distance > 0, fare_amount > 0) |>
    summarise(viajes = n(), tarifa = round(mean(fare_amount), 2),
              .by = payment_type) |> arrange(desc(viajes))
  cat("resultado (collect):\n"); print(as.data.frame(collect(q)))
  cat("\nplan de consulta (explain), primeras lineas:\n")
  cap <- utils::capture.output(explain(q))
  cat(paste(utils::head(cap, 6), collapse = "\n"), "\n")
  # recaida automatica en un frame EN MEMORIA
  rara <- function(x) x^2 - sqrt(abs(x)) + 1
  dk <- as_duckdb_tibble(as_tibble(read_parquet(MUS)))
  o <- dk |> mutate(z = rara(popularity)) |> summarise(m = round(mean(z), 1)) |>
    collect()
  cat("recaida a dplyr (funcion R propia) sin error, media z =", o$m, "\n")
}

# --- 6. particionar y podar --------------------------------------------------
demo_particion <- function() {
  sal <- tempfile("taxi_part_"); dir.create(sal)
  open_dataset(taxi_files()) |>
    filter(tpep_pickup_datetime >= as.Date("2024-01-01"),
           tpep_pickup_datetime <  as.Date("2025-01-01"),
           trip_distance > 0, fare_amount > 0) |>
    mutate(anio = lubridate::year(tpep_pickup_datetime),
           mes  = lubridate::month(tpep_pickup_datetime)) |>
    select(anio, mes, trip_distance, fare_amount, tip_amount, payment_type) |>
    group_by(anio, mes) |> write_dataset(sal, format = "parquet")
  dsp <- open_dataset(sal)
  t_todo <- crono({ a <- dsp |> summarise(n = n(), d = mean(trip_distance)) |>
                      collect() })
  t_uno  <- crono({ b <- dsp |> filter(anio == 2024, mes == 3) |>
                      summarise(n = n(), d = mean(trip_distance)) |> collect() })
  cat("12 particiones:", t_todo, "s (n =", format(a$n, big.mark = " "), ")\n")
  cat("1 particion podada (marzo):", t_uno, "s (n =", format(b$n, big.mark = " "),
      ", dist", round(b$d, 3), ")\n")
  unlink(sal, recursive = TRUE)
}

# --- 7. benchmark: la misma agregacion por los cuatro caminos ---------------
demo_bench <- function() {
  files <- taxi_files(); glob <- taxi_glob()
  gc()
  mA <- crono({ todo <- open_dataset(files) |>
    select(payment_type, trip_distance, fare_amount) |> collect()
    todo |> filter(trip_distance > 0, fare_amount > 0) |>
      summarise(n = n(), tarifa = mean(fare_amount), .by = payment_type) })
  ram <- round(as.numeric(object.size(todo)) / 1e6); rm(todo); gc()
  tB <- crono(open_dataset(files) |> filter(trip_distance > 0, fare_amount > 0) |>
    summarise(n = n(), tarifa = mean(fare_amount), .by = payment_type) |> collect())
  con <- dbConnect(duckdb())
  tC <- crono(dbGetQuery(con, sprintf(
    "SELECT payment_type, count(*) n, avg(fare_amount) t FROM read_parquet('%s')
     WHERE trip_distance>0 AND fare_amount>0 GROUP BY payment_type", glob)))
  dbDisconnect(con, shutdown = TRUE)
  tD <- NA
  if (requireNamespace("duckplyr", quietly = TRUE)) {
    tD <- crono(duckplyr::read_parquet_duckdb(glob) |>
      filter(trip_distance > 0, fare_amount > 0) |>
      summarise(n = n(), tarifa = mean(fare_amount), .by = payment_type) |> collect())
  }
  cat(sprintf("A cargar-todo dplyr: %5.2f s (RAM %d MB, 3 de 19 col)\n", mA, ram))
  cat(sprintf("B arrow perezoso   : %5.2f s (solo el resultado en RAM)\n", tB))
  cat(sprintf("C DuckDB SQL       : %5.2f s (solo el resultado en RAM)\n", tC))
  cat(sprintf("D duckplyr         : %5.2f s (solo el resultado en RAM)\n", tD))
}

# --- 8. perfil del ano: cuatro preguntas a 41 M de filas --------------------
demo_perfil <- function() {
  con <- dbConnect(duckdb()); glob <- taxi_glob()
  f <- function(q) dbGetQuery(con, sprintf(q, glob))
  dow <- f("SELECT dayofweek(tpep_pickup_datetime) dow, count(*) n,
            round(avg(fare_amount),2) tarifa FROM read_parquet('%s')
            WHERE fare_amount>0 AND tpep_pickup_datetime>='2024-01-01'
            AND tpep_pickup_datetime<'2025-01-01' GROUP BY dow ORDER BY n DESC")
  cat("dia mas ajetreado (dow):", dow$dow[1], "->", format(dow$n[1], big.mark=" "),
      "viajes | mas caro:", dow$dow[which.max(dow$tarifa)], "->",
      max(dow$tarifa), "$\n")
  ing <- f("SELECT round(sum(total_amount)/1e6,1) M FROM read_parquet('%s')
            WHERE total_amount>0 AND tpep_pickup_datetime>='2024-01-01'
            AND tpep_pickup_datetime<'2025-01-01'")
  cat("ingreso anual (millones $):", round(ing$M), "\n")
  q <- f("SELECT round(quantile_cont(trip_distance,0.5),2) mediana,
          round(quantile_cont(trip_distance,0.9),2) p90,
          round(avg(trip_distance),2) media FROM read_parquet('%s')
          WHERE trip_distance>0 AND trip_distance<100
          AND tpep_pickup_datetime>='2024-01-01'
          AND tpep_pickup_datetime<'2025-01-01'")
  cat("distancia: mediana", q$mediana, "| p90", q$p90, "| media", q$media,
      "(sesgo a la derecha)\n")
  z <- f("SELECT count(DISTINCT PULocationID) z FROM read_parquet('%s')")
  cat("zonas de recogida distintas:", z$z, "\n")
  dbDisconnect(con, shutdown = TRUE)
}

# --- ejecucion ---------------------------------------------------------------
if (sys.nframe() == 0L) {
  if (is.null(MUS)) { cat("(sin musica.parquet: nada que ejecutar)\n"); quit() }
  cat("\n== 1. Arrow columnar y compresion ==\n"); demo_arrow()
  cat("\n== 2. puente zero-copy arrow -> DuckDB ==\n"); demo_puente()
  if (is.null(TAXI_DIR)) {
    cat("\n(sin dataset de taxis: se saltan las demos de gran escala)\n")
  } else {
    cat("\n== 3. arrow open_dataset perezoso + pushdown ==\n"); demo_dataset()
    cat("\n== 4. DuckDB SQL sobre Parquet ==\n"); demo_duckdb()
    cat("\n== 5. duckplyr perezoso + explain + recaida ==\n"); demo_duckplyr()
    cat("\n== 6. particionar y podar ==\n"); demo_particion()
    cat("\n== 7. benchmark de los cuatro caminos ==\n"); demo_bench()
    cat("\n== 8. perfil del ano en cuatro preguntas ==\n"); demo_perfil()
  }
  cat("\nFIN cap09 OK\n")
}
