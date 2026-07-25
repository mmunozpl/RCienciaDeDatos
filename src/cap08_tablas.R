# src/cap08_tablas.R -- analisis tabular con dplyr y tidyr.
# Acompana al cap. 8: reproduce los ejemplos (verbos, group_by, joins, pivot,
# temporal, rendimiento, integrador) sobre el catalogo real. Semillas fijas.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(purrr)
  library(lubridate); library(slider)
})
crono <- function(expr) unname(system.time(expr)["elapsed"])

leer_musica <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) return(NULL)
  candidatos <- c("data/processed/musica.parquet", "../data/processed/musica.parquet")
  ruta <- candidatos[file.exists(candidatos)][1]
  if (is.na(ruta)) return(NULL)
  arrow::read_parquet(ruta)
}

demo_verbos <- function(mus) {
  cat("class:", paste(class(mus), collapse = " "), "\n")
  cat("dims:", nrow(mus), "x", ncol(mus), "\n")
  cat("pistas pop>95:", mus |> filter(popularity > 95) |> nrow(), "\n")
  print(mus |> arrange(desc(popularity)) |> select(track_name, popularity) |> head(2))
  print(mus |> summarise(n = n(), pop = round(mean(popularity), 1)))
  # mutate patterns
  print(mus |> mutate(banda = case_when(tempo < 90 ~ "lenta", tempo < 140 ~ "media",
                                        .default = "rapida")) |> count(banda))
  # filtrar/seleccionar
  cat("salsa+tango con energy>0.8:",
      mus |> filter(track_genre %in% c("salsa","tango"), energy > 0.8) |> nrow(), "\n")
}

demo_groupby <- function(mus) {
  print(mus |> group_by(track_genre) |>
        summarise(n = n(), pop = round(mean(popularity), 1), .groups = "drop") |>
        slice_max(pop, n = 3))
  # .by en linea
  print(mus |> summarise(pop = round(mean(popularity), 1), .by = track_genre) |>
        slice_max(pop, n = 2))
  # mutate agrupado (ventana)
  print(mus |> mutate(pop_gen = mean(popularity), .by = track_genre) |>
        select(track_name, track_genre, popularity, pop_gen) |> head(2))
  # ranking dentro de grupo
  print(mus |> group_by(track_genre) |>
        mutate(rango = row_number(desc(popularity))) |> filter(rango <= 1) |>
        ungroup() |> filter(track_genre %in% c("salsa","tango")) |>
        select(track_genre, track_name, rango))
  # media de medias vs global (ponderada)
  g <- mus |> summarise(m = mean(popularity), n = n(), .by = track_genre)
  cat("media de medias:", round(mean(g$m), 3),
      "| global:", round(mean(mus$popularity), 3),
      "| ponderada:", round(weighted.mean(g$m, g$n), 3), "\n")
}

demo_joins <- function() {
  pistas <- tibble(track_id = c("t1","t2","t3","t4"),
                   genero = c("pop","rock","pop","jazz"), pop = c(80,55,91,40))
  generos <- tibble(genero = c("pop","rock","salsa"),
                    familia = c("popular","popular","latina"))
  cat("left_join filas:", nrow(left_join(pistas, generos, by = "genero")), "\n")
  cat("inner_join filas:", nrow(inner_join(pistas, generos, by = "genero")), "\n")
  cat("full_join filas:", nrow(full_join(pistas, generos, by = "genero")), "\n")
  cat("anti_join (sin pareja):",
      anti_join(pistas, generos, by = "genero")$genero, "\n")
  # join por desigualdad
  tramos <- tibble(lo = c(0,40,70), hi = c(40,70,101), nivel = c("bajo","medio","alto"))
  print(tibble(pop = c(10,45,78,95)) |> left_join(tramos, join_by(pop >= lo, pop < hi)))
}

demo_pivot <- function() {
  ancho <- tibble(pista = c("A","B"), ene_2023 = c(100,200), ene_2024 = c(150,220))
  largo <- ancho |> pivot_longer(starts_with("ene_"), names_to = "anio",
                                 names_prefix = "ene_", values_to = "escuchas")
  print(largo)
  print(largo |> pivot_wider(names_from = anio, values_from = escuchas))
  # names_sep con .value
  multi <- tibble(pista = "A", ene_min = 100, ene_max = 200, feb_min = 90, feb_max = 180)
  print(multi |> pivot_longer(-pista, names_to = c("mes", ".value"), names_sep = "_"))
  # sucio -> tidy
  sucio <- tribble(~artista, ~escuchas_2023, ~escuchas_2024,
                   "Rosalia", "1.200", "1.800", "Bad Bunny", "3.400", "4.100")
  tidy <- sucio |> pivot_longer(starts_with("escuchas_"), names_to = "anio",
                               names_prefix = "escuchas_", values_to = "escuchas") |>
    mutate(anio = as.integer(anio), escuchas = as.integer(gsub("\\.", "", escuchas)))
  cat("tidy escuchas clase:", class(tidy$escuchas), "\n")
}

demo_temporal <- function() {
  set.seed(2026)
  serie <- tibble(fecha = ymd("2026-01-01") + days(0:364),
                  escuchas = rpois(365, 50 + 20*sin(2*pi*yday(fecha)/365)))
  mes <- serie |> group_by(mes = floor_date(fecha, "month")) |>
    summarise(total = sum(escuchas), .groups = "drop")
  cat("mes pico:", format(mes$mes[which.max(mes$total)]), "\n")
  # ventana movil
  serie <- serie |> mutate(mm7 = slide_dbl(escuchas, mean, .before = 6, .complete = TRUE))
  cat("primera media movil valida (fila 7):", round(serie$mm7[7], 1), "\n")
  # lag
  print(tibble(x = c(10,12,9,14)) |> mutate(cambio = x - lag(x)))
  # componentes
  f <- ymd("2026-03-15")
  cat("componentes:", year(f), as.character(month(f, label=TRUE)),
      as.character(wday(f, label=TRUE)), "\n")
  cat("mas un mes civil:", format(f %m+% months(1)), "\n")
}

demo_rendimiento <- function(mus) {
  set.seed(2026); sub <- mus |> slice_sample(n = 20000)
  tb <- crono({ s <- 0; for (i in 1:nrow(sub)) s <- s + sub$popularity[i] })
  tv <- crono(sum(sub$popularity))
  cat("bucle filas vs sum:", round(tb / max(tv, 1e-4)), "x\n")
  # copy-on-modify
  if (requireNamespace("lobstr", quietly = TRUE)) {
    mod <- mus |> mutate(nuevo = popularity * 2)
    cat("columnas no tocadas comparten memoria:",
        lobstr::obj_addr(mus$energy) == lobstr::obj_addr(mod$energy), "\n")
  }
  # dtplyr
  if (requireNamespace("dtplyr", quietly = TRUE)) {
    r <- dtplyr::lazy_dt(mus) |> filter(popularity > 50) |>
      group_by(track_genre) |> summarise(n = n()) |> as_tibble()
    cat("dtplyr filas:", nrow(r), "\n")
  }
}

demo_integrador <- function(mus) {
  res <- mus |>
    filter(popularity > 0) |>
    group_by(track_genre) |>
    summarise(n = n(), pop = mean(popularity), energia = mean(energy),
              pct_explicito = mean(explicit) * 100, .groups = "drop") |>
    filter(n >= 500) |>
    mutate(indice = scale(pop)[,1] + scale(energia)[,1]) |>
    slice_max(indice, n = 5) |>
    mutate(across(where(is.numeric), \(x) round(x, 1)))
  print(res |> select(track_genre, n, pop, energia, pct_explicito))
  # correlacion por grupo
  cors <- mus |> summarise(r = round(cor(energy, popularity), 3), .by = track_genre) |>
    arrange(desc(r))
  cat("cor energy-pop: max", cors$track_genre[1], cors$r[1],
      "| min", tail(cors$track_genre,1), tail(cors$r,1), "\n")
  # muchos modelos
  mods <- mus |> filter(track_genre %in% c("rock","jazz","classical")) |>
    group_by(track_genre) |> nest() |>
    mutate(pend = map_dbl(data, \(d) coef(lm(popularity ~ energy, d))[["energy"]]))
  print(mods |> select(track_genre, pend) |> mutate(pend = round(pend, 1)))
  # duplicados
  cat("pares (id, genero) duplicados:",
      nrow(mus |> count(track_id, track_genre) |> filter(n > 1)), "\n")
}

if (sys.nframe() == 0L) {
  mus <- leer_musica()
  if (is.null(mus)) { cat("(sin parquet: solo demos que no lo necesitan)\n") }
  demos <- list(joins = function() demo_joins(),
                pivot = function() demo_pivot(),
                temporal = function() demo_temporal())
  if (!is.null(mus)) demos <- c(
    list(verbos = function() demo_verbos(mus),
         groupby = function() demo_groupby(mus)),
    demos,
    list(rendimiento = function() demo_rendimiento(mus),
         integrador = function() demo_integrador(mus)))
  for (nombre in names(demos)) {
    cat("\n==", nombre, "==\n")
    demos[[nombre]]()
  }
}
