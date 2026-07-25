# src/cap05_io.R -- entrada/salida, ficheros y formatos.
# Acompana al cap. 5: reproduce los experimentos del capitulo sin tocar la
# red (todo es local y deterministico; semillas fijas). Los tiempos son
# cocientes orientativos: los absolutos dependen de la maquina.

suppressPackageStartupMessages({
  library(fs); library(readr); library(dplyr); library(tibble)
  library(jsonlite); library(DBI)
})
crono <- function(expr) unname(system.time(expr)["elapsed"])
raiz <- path(tempdir(), "cap05_demo"); dir_create(raiz)

demo_conexiones <- function() {
  old <- setwd(raiz); on.exit(setwd(old))
  writeLines(c("primera linea", "segunda linea", "tercera linea"), "notas.txt")
  con <- file("notas.txt", "r")
  l1 <- readLines(con, n = 1); l2 <- readLines(con, n = 1)
  close(con)
  cat("cursor con estado:", l1, "|", l2, "\n")
  cat("PAR1 es la firma parquet:", {
    writeBin(as.raw(c(0x50, 0x41, 0x52, 0x31)), "cab.bin")
    rawToChar(readBin("cab.bin", "raw", n = 4)) }, "\n")
  con_gz <- gzfile("generos.txt.gz", "w")
  writeLines(rep("pop,rock,jazz", 1000), con_gz); close(con_gz)
  cat("gz transparente:", file.size("generos.txt.gz"), "B;",
      "identico:", identical(readLines("generos.txt.gz"),
                             rep("pop,rock,jazz", 1000)), "\n")
}

demo_rutas <- function() {
  p <- path("data", "raw", "musica.csv")
  cat("piezas:", path_file(p), "|", path_dir(p), "|", path_ext(p), "\n")
  cat("humano:", as.character(fs_bytes(19433368)), "\n")
  dir_create(path(raiz, "proyecto", c("raw", "processed")))
  destino <- path(raiz, "proyecto", "raw", "a.csv")
  writeLines("x", destino)
  file_chmod(destino, "a-w")     # crudo intocable
  r <- suppressWarnings(
    tryCatch(writeLines("y", destino), error = function(e) "denegado"))
  cat("raw de solo lectura:", r, "\n")
  file_chmod(destino, "u+w")
}

demo_codificacion <- function() {
  old <- setwd(raiz); on.exit(setwd(old))
  s <- "señal"
  cat("chars:", nchar(s), "| bytes:", nchar(s, type = "bytes"), "\n")
  bytes_l1 <- iconv(s, "UTF-8", "latin1", toRaw = TRUE)[[1]]
  writeBin(bytes_l1, "senal_latin1.txt")
  mal <- readLines("senal_latin1.txt", encoding = "UTF-8", warn = FALSE)
  cat("mal leido valida UTF-8:", validUTF8(mal), "\n")
  bien <- iconv(readLines("senal_latin1.txt", warn = FALSE), "latin1", "UTF-8")
  cat("reparado con iconv:", bien, "\n")
  cat("mojibake inverso:", iconv(list(charToRaw(s)), "latin1", "UTF-8"), "\n")
  con_l1 <- file("artistas_latin1.csv", "w", encoding = "latin1")
  writeLines(c("artista", "Café Tacvba", "Mägo de Oz"), con_l1); close(con_l1)
  print(guess_encoding("artistas_latin1.csv"))
  print(read_csv("artistas_latin1.csv",
                 locale = locale(encoding = "latin1"), show_col_types = FALSE))
}

demo_csv <- function() {
  old <- setwd(raiz); on.exit(setwd(old))
  linea <- 'spotify:4uLU6hMC,"Tyler, The Creator",IGOR,pop'
  cat("strsplit trocea en", length(strsplit(linea, ",")[[1]]), "(mal)\n")
  writeLines(c("track id,1a posicion,codigo,fecha",
               "t001,3,007,2026-03-01", "t002,12,042,2026-03-02"), "tipos.csv")
  b <- read.csv("tipos.csv"); r <- read_csv("tipos.csv", show_col_types = FALSE)
  cat("base pierde ceros:", b$codigo[1], "| readr los guarda:", r$codigo[1],
      "| fecha:", class(r$fecha), "\n")
  writeLines(c("id,pop", "a,10", "b,alto", "c,30"), "sucio.csv")
  s <- suppressWarnings(read_csv("sucio.csv",
        col_types = cols(id = col_character(), pop = col_integer())))
  cat("problems() ficha", nrow(problems(s)), "celda(s); NA en pop:",
      sum(is.na(s$pop)), "\n")
  writeLines(c("titulo;duracion", "Uno;3,52"), "europeo.csv")
  e <- read_csv2("europeo.csv", show_col_types = FALSE)
  cat("read_csv2 europeo:", e$duracion, "\n")
  d <- tibble(id = sprintf("t%05d", 1:5000), g = "pop")
  write_csv(d, "d.csv"); write_csv(d, "d.csv.gz")
  cat("csv:", file.size("d.csv"), "B | csv.gz:", file.size("d.csv.gz"), "B\n")
}

demo_json <- function() {
  x <- fromJSON('[{"id":"t1","pop":80},{"id":"t2","pop":65}]')
  cat("array de objetos ->", class(x), "de", nrow(x), "filas\n")
  cat("caja: ", toJSON(list(pop = 74)),
      " | sin caja: ", toJSON(list(pop = 74), auto_unbox = TRUE), "\n", sep = "")
  cat("NA por defecto:", toJSON(c(1.5, NA)),
      "| como null:", toJSON(c(1.5, NA), na = "null"), "\n")
  big <- fromJSON('{"n": 9007199254740993}')
  cat("entero grande redondeado:", format(big$n, digits = 22),
      "| como texto:", fromJSON('{"n": 9007199254740993}',
                               bigint_as_char = TRUE)$n, "\n")
  cat("validate: ", validate('{"a": 1}'), "/", validate('{"a": 1'), "\n")
  d <- tibble(id = c("t1", "t2"), pop = c(80L, 65L))
  f <- path(raiz, "eventos.jsonl")
  con <- file(f, "w"); stream_out(d, con, verbose = FALSE); close(con)
  cat("jsonl:", readLines(f)[1], "...\n")
}

demo_parquet <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) { cat("(sin arrow)\n"); return(invisible()) }
  suppressPackageStartupMessages(library(arrow))
  RUTA <- path(dirname(dirname(raiz)), "nada.parquet")  # placeholder
  candidatos <- c("data/processed/musica.parquet",
                  "../data/processed/musica.parquet")
  real <- candidatos[file.exists(candidatos)][1]
  set.seed(2026)
  d <- tibble(track_id = sprintf("t%06d", 1:100000),
              track_genre = factor(sample(c("pop","rock","jazz"), 1e5, TRUE)),
              popularity = sample(0:100, 1e5, TRUE))
  csv_f <- path(raiz, "d.csv"); pq_f <- path(raiz, "d.parquet")
  write_csv(d, csv_f)
  write_parquet(d, pq_f, compression = "zstd")
  cat("csv:", round(file.size(csv_f)/2^10), "kB | parquet zstd:",
      round(file.size(pq_f)/2^10), "kB\n")
  t_csv <- crono(read_csv(csv_f, show_col_types = FALSE))
  t_pq <- crono(read_parquet(pq_f))
  cat("leer csv/parquet:", round(t_csv / max(t_pq, 1e-4), 1), "x\n")
  vuelto <- read_parquet(pq_f)
  cat("el factor sobrevive:", is.factor(vuelto$track_genre), "\n")
  if (!is.na(real)) {
    mus <- read_parquet(real)
    cat("catalogo real:", nrow(mus), "x", ncol(mus),
        "| dup clave (id, genero):",
        sum(duplicated(mus[c("track_id", "track_genre")])), "\n")
  }
}

demo_rds <- function() {
  d0 <- tibble(id = c("007", "042"), genero = factor(c("pop", "rock")),
               audio = list(c(0.1, 0.9), 0.4))
  f <- path(raiz, "p.rds")
  saveRDS(d0, f)
  cat("viaje redondo RDS:", identical(readRDS(f), d0), "\n")
  m <- lm(mpg ~ wt, data = mtcars)
  fm <- path(raiz, "m.rds"); saveRDS(m, fm)
  cat("modelo en", file.size(fm), "B; coef wt:",
      round(coef(readRDS(fm))[2], 3), "\n")
}

demo_descarga <- function() {
  old <- setwd(raiz); on.exit(setwd(old))
  writeLines(c("id,pop", "t1,80"), "origen.csv")
  url_local <- paste0("file://", path(raiz, "origen.csv"))
  descargar_idempotente <- function(url, destino, sha256 = NULL) {
    if (file.exists(destino)) return(invisible(destino))
    tmp <- paste0(destino, ".part")
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    if (!is.null(sha256)) {
      real <- as.character(openssl::sha256(file(tmp)))
      if (!identical(real, sha256)) { unlink(tmp); stop("hash no coincide") }
    }
    file.rename(tmp, destino)
    invisible(destino)
  }
  descargar_idempotente(url_local, "copia.csv")
  h <- as.character(openssl::sha256(file("copia.csv")))
  cat("descargado; sha256:", substr(h, 1, 16), "...\n")
  r <- tryCatch(descargar_idempotente(url_local, "otra.csv", sha256 = "x"),
                error = function(e) conditionMessage(e))
  cat("hash malo:", r, "| .part limpio:", !file.exists("otra.csv.part"), "\n")
}

demo_sql <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))
  pistas <- tibble(track_id = sprintf("t%03d", 1:6),
                   genero = c("pop","rock","pop","jazz","rock","pop"),
                   popularity = c(80L, 55L, 91L, 40L, 62L, 73L))
  dbWriteTable(con, "pistas", pistas)
  q <- "SELECT track_id FROM pistas WHERE genero = ?"
  cat("parametrizada:", nrow(dbGetQuery(con, q, params = list("pop"))), "filas",
      "| inyeccion neutralizada:",
      nrow(dbGetQuery(con, q, params = list("pop' OR '1'='1"))), "filas\n")
  dbBegin(con)
  dbExecute(con, "UPDATE pistas SET popularity = 0 WHERE track_id = 't001'")
  dbRollback(con)
  cat("rollback deshace:",
      dbGetQuery(con, "SELECT popularity FROM pistas WHERE track_id='t001'")[[1]],
      "\n")
  set.seed(2026)
  n <- 2e5
  dbWriteTable(con, "grande",
               data.frame(track_id = sprintf("t%07d", sample(n)),
                          popularity = sample(0:100, n, TRUE)))
  qg <- "SELECT popularity FROM grande WHERE track_id = ?"
  t_sin <- crono(for (k in 1:100) dbGetQuery(con, qg, params = list("t0001234")))
  dbExecute(con, "CREATE INDEX idx_id ON grande(track_id)")
  t_con <- crono(for (k in 1:100) dbGetQuery(con, qg, params = list("t0001234")))
  cat("indice:", round(t_sin / max(t_con, 1e-4)), "x\n")
}

demo_chunks <- function() {
  old <- setwd(raiz); on.exit(setwd(old))
  set.seed(2026)
  n <- 2e5
  write_csv(tibble(id = sprintf("t%06d", 1:n),
                   genero = sample(c("pop","rock","jazz"), n, TRUE)), "g.csv")
  acum <- new.env(hash = TRUE)
  f_trozo <- function(chunk, pos) {
    t <- table(chunk$genero)
    for (g in names(t)) {
      prev <- if (exists(g, envir = acum, inherits = FALSE)) get(g, envir = acum) else 0
      assign(g, prev + t[[g]], envir = acum)
    }
  }
  read_csv_chunked("g.csv", SideEffectChunkCallback$new(f_trozo),
                   chunk_size = 50000, show_col_types = FALSE)
  entero <- table(read_csv("g.csv", show_col_types = FALSE)$genero)
  cat("chunked == entero:",
      all(unlist(mget(names(entero), envir = acum)) == as.vector(entero)), "\n")
}

demo_integrador <- function() {
  old <- setwd(raiz); on.exit(setwd(old))
  GENEROS_BASE <- c(pop = 48, rock = 19, classical = 13)
  generar_csv_muestra <- function(destino, semilla = 2026) {
    set.seed(semilla)
    filas <- lapply(names(GENEROS_BASE), function(g) {
      n <- 1000
      pop <- pmin(100, pmax(0, round(rnorm(n, GENEROS_BASE[[g]], 18))))
      roto <- runif(n) < 0.02
      tibble(track_id = sprintf("%s%05d", substr(g, 1, 2), seq_len(n) - 1),
             track_name = paste("tema", seq_len(n) - 1),
             artists = "artista", track_genre = g,
             popularity = as.integer(pop),
             tempo = ifelse(roto, 0, round(rnorm(n, 120, 25), 3)),
             explicit = ifelse(runif(n) < 0.086, "True", "False"))
    })
    write_csv(bind_rows(filas), destino)
    invisible(destino)
  }
  generar_csv_muestra("spotify_muestra.csv")
  parsear <- function(origen) {
    d <- read_csv(origen,
                  col_types = cols(track_id = col_character(),
                                   track_name = col_character(),
                                   artists = col_character(),
                                   track_genre = col_character(),
                                   popularity = col_integer(),
                                   tempo = col_double(),
                                   explicit = col_logical()),
                  locale = locale(encoding = "UTF-8"))
    stopifnot(nrow(problems(d)) == 0)
    d |> mutate(tempo = na_if(tempo, 0))
  }
  pistas <- parsear("spotify_muestra.csv")
  cat("tempos rotos -> NA:", sum(is.na(pistas$tempo)),
      "| explicit:", sum(pistas$explicit), "\n")
  GENEROS_VALIDOS <- c("pop", "rock", "classical", "hip-hop", "jazz")
  validar <- function(d) {
    clave <- paste(d$track_id, d$track_genre)
    stopifnot(
      `genero fuera de catalogo` = all(d$track_genre %in% GENEROS_VALIDOS),
      `clave duplicada` = !anyDuplicated(clave),
      `popularidad fuera de 0-100` = all(d$popularity >= 0 & d$popularity <= 100),
      `tempo fuera de rango` = all(is.na(d$tempo) | (d$tempo > 0 & d$tempo <= 250))
    )
    invisible(d)
  }
  validar(pistas)
  cat("validacion: OK\n")
  if (requireNamespace("arrow", quietly = TRUE)) {
    suppressPackageStartupMessages(library(arrow))
    pistas |> mutate(track_genre = factor(track_genre)) |>
      write_parquet("musica_muestra.parquet", compression = "zstd")
    cat("csv/parquet:", round(file.size("spotify_muestra.csv") /
                              file.size("musica_muestra.parquet"), 1), "x\n")
    if (requireNamespace("duckdb", quietly = TRUE)) {
      con <- dbConnect(duckdb::duckdb())
      print(dbGetQuery(con, "
        SELECT track_genre, COUNT(*) AS pistas,
               ROUND(AVG(popularity), 1) AS media
        FROM read_parquet('musica_muestra.parquet')
        WHERE tempo IS NOT NULL
        GROUP BY track_genre ORDER BY media DESC"))
      dbDisconnect(con, shutdown = TRUE)
    } else {
      print(open_dataset("musica_muestra.parquet") |>
              filter(!is.na(tempo)) |> group_by(track_genre) |>
              summarise(pistas = n(), media = round(mean(popularity), 1)) |>
              arrange(desc(media)) |> collect())
    }
  }
}

if (sys.nframe() == 0L) {
  demos <- list(conexiones = demo_conexiones, rutas = demo_rutas,
                codificacion = demo_codificacion, csv = demo_csv,
                json = demo_json, parquet = demo_parquet, rds = demo_rds,
                descarga = demo_descarga, sql = demo_sql,
                chunks = demo_chunks, integrador = demo_integrador)
  for (nombre in names(demos)) {
    cat("\n==", nombre, "==\n")
    demos[[nombre]]()
  }
}
