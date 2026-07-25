# src/cap06_objetos.R -- sistemas de objetos, evaluacion ordenada y patrones.
# Acompana al cap. 6: reproduce cada ejemplo (S3, S4, R6, S7, tidy eval,
# patrones). Semillas fijas donde hay azar.

suppressPackageStartupMessages({
  library(S7); library(R6); library(rlang)
  library(dplyr); library(purrr); library(tibble)
})

demo_s3 <- function() {
  pista <- function(titulo, genero, energia)
    structure(list(titulo = titulo, genero = genero, energia = energia),
              class = "pista")
  print.pista <- function(x, ...) cat("<pista>", x$titulo, "(", x$genero, ")\n")
  duracion <- function(x, ...) UseMethod("duracion")
  duracion.pista <- function(x, ...) "3:52"
  duracion.default <- function(x, ...) stop("no se como durar esto")
  p <- pista("Nube", "pop", 0.74)
  print(p)
  cat("duracion:", duracion(p), "\n")
  cat("default falla:", tryCatch(duracion(42), error = \(e) "error"), "\n")
  # grupo Ops: sobrecarga de operadores
  dinero <- function(c) structure(as.integer(c), class = "dinero")
  print.dinero <- function(x, ...) cat(sprintf("%.2f EUR\n", unclass(x) / 100))
  Ops.dinero <- function(e1, e2) {
    v <- get(.Generic)(unclass(e1), unclass(e2))
    if (.Generic %in% c("+", "-", "*")) dinero(v) else v
  }
  print(dinero(1250) + dinero(350))
  cat("comparacion:", dinero(1250) > dinero(350), "\n")
  # protocolo tipo lm
  modelo_medias <- function(formula, datos) {
    y <- all.vars(formula)[1]; g <- all.vars(formula)[2]
    structure(list(medias = tapply(datos[[y]], datos[[g]], mean), g = g),
              class = "modelo_medias")
  }
  predict.modelo_medias <- function(object, newdata, ...)
    unname(object$medias[as.character(newdata[[object$g]])])
  set.seed(2026)
  d <- data.frame(genero = sample(c("pop","rock","jazz"), 60, TRUE),
                  energia = runif(60))
  m <- modelo_medias(energia ~ genero, d)
  cat("predict:", round(predict(m, data.frame(genero = c("pop","jazz"))), 3), "\n")
}

demo_s4 <- function() {
  setClass("Pista4", representation(titulo = "character", energia = "numeric"),
           validity = function(object)
             if (object@energia < 0 || object@energia > 1) "energia fuera de [0,1]"
             else TRUE)
  p4 <- new("Pista4", titulo = "Nube", energia = 0.74)
  cat("slot:", p4@titulo, p4@energia, "| isS4:", isS4(p4), "\n")
  cat("rechaza invalido:",
      tryCatch(new("Pista4", titulo = "X", energia = 9),
               error = \(e) "error"), "\n")
  # despacho multiple
  setGeneric("mezclar", function(a, b) standardGeneric("mezclar"))
  setClass("Voz", representation(nombre = "character"))
  setClass("Beat", representation(bpm = "numeric"))
  setMethod("mezclar", signature("Voz", "Beat"),
            function(a, b) cat("voz", a@nombre, "sobre", b@bpm, "bpm\n"))
  mezclar(new("Voz", nombre = "Ana"), new("Beat", bpm = 120))
}

demo_r6 <- function() {
  Cuenta <- R6Class("Cuenta",
    public = list(
      initialize = function(saldo = 0) private$.saldo <- saldo,
      ingresar = function(x) { private$.saldo <- private$.saldo + x; invisible(self) }),
    private = list(.saldo = 0),
    active = list(saldo = function(value)
      if (missing(value)) private$.saldo else stop("usa ingresar()")))
  cta <- Cuenta$new(100); cta$ingresar(50)
  cat("saldo:", cta$saldo, "| veta escritura:",
      tryCatch({ cta$saldo <- 9 }, error = \(e) "error"), "\n")
  # referencia vs clone
  c1 <- Cuenta$new(0); c2 <- c1; c2$ingresar(100)
  cat("referencia comparte (c1 == 100):", c1$saldo, "\n")
  c3 <- c1$clone(); c3$ingresar(1000)
  cat("clone independiente (c1 sigue 100):", c1$saldo, "\n")
  # Welford: estado en linea
  Rodante <- R6Class("Rodante", public = list(
    n = 0, media = 0, m2 = 0,
    add = function(x) { self$n <- self$n + 1; d <- x - self$media
      self$media <- self$media + d / self$n
      self$m2 <- self$m2 + d * (x - self$media); invisible(self) },
    var = function() if (self$n < 2) NA else self$m2 / (self$n - 1)))
  set.seed(2026); xs <- rnorm(10000, 50, 10)
  acc <- Rodante$new(); for (x in xs) acc$add(x)
  cat("welford media/var:", round(acc$media, 2), round(acc$var(), 1),
      "| vs base:", round(mean(xs), 2), round(var(xs), 1), "\n")
}

demo_s7 <- function() {
  Pista <- new_class("Pista",
    properties = list(titulo = class_character,
                      energia = new_property(class_numeric, default = 0.5),
                      popularidad = class_integer),
    validator = function(self)
      if (self@energia < 0 || self@energia > 1) "energia en [0,1]")
  p <- Pista(titulo = "Nube", energia = 0.74, popularidad = 74L)
  cat("propiedad:", p@titulo, p@energia, "\n")
  cat("valida al modificar:",
      tryCatch({ p@energia <- 2 }, error = \(e) "error"), "\n")
  cat("tipo incorrecto:",
      tryCatch(Pista(titulo = 42, energia = 0.5), error = \(e) "error"), "\n")
  # generico + herencia
  describir <- new_generic("describir", "x")
  method(describir, Pista) <- function(x) cat("Pista:", x@titulo, "\n")
  PistaViva <- new_class("PistaViva", parent = Pista,
    properties = list(sala = class_character))
  method(describir, PistaViva) <- function(x) {
    describir(super(x, to = Pista)); cat("  en directo desde", x@sala, "\n") }
  describir(PistaViva(titulo = "Nube", energia = 0.6, sala = "Apolo"))
  # convivencia con S3
  method(print, Pista) <- function(x, ...) cat("<Pista>", x@titulo, "\n")
  print(p)
  # convert + operadores
  Celsius <- new_class("Celsius", properties = list(t = class_numeric))
  Fahrenheit <- new_class("Fahrenheit", properties = list(t = class_numeric))
  method(convert, list(from = Celsius, to = Fahrenheit)) <-
    function(from, to, ...) Fahrenheit(t = from@t * 9/5 + 32)
  cat("convert 100C:", convert(Celsius(t = 100), to = Fahrenheit)@t, "F\n")
}

demo_tidyeval <- function() {
  pistas <- tibble(genero = c("pop","rock","pop","jazz","rock"),
                   energia = c(0.7,0.9,0.6,0.3,0.85),
                   popularidad = c(80,55,91,40,62))
  # abrazo
  media_por <- function(datos, grupo, valor)
    datos |> group_by({{ grupo }}) |>
      summarise(media = mean({{ valor }}), .groups = "drop")
  print(media_por(pistas, genero, energia))
  # nombrado
  resumir <- function(datos, col)
    datos |> summarise("media_{{col}}" := mean({{ col }}))
  print(names(resumir(pistas, energia)))
  # .data desde texto
  col <- "energia"
  cat(".data desde texto:",
      round(pistas |> summarise(m = mean(.data[[col]])) |> pull(m), 3), "\n")
  # formula
  f <- popularidad ~ energia + genero
  cat("formula vars:", paste(all.vars(f), collapse = ", "), "\n")
  m <- lm(popularidad ~ energia, data = pistas)
  cat("lm coef:", round(coef(m), 2), "\n")
  # construir expresion
  cond <- call2(">", sym("energia"), 0.6)
  cat("filas con energia>0.6:", nrow(pistas |> filter(!!cond)), "\n")
  # mini biblioteca
  frecuencias <- function(datos, col)
    datos |> count({{ col }}, sort = TRUE, name = "n") |>
      mutate(prop = round(n / sum(n), 3))
  print(frecuencias(pistas, genero))
}

demo_patrones <- function() {
  # transformador como clausura: fit/transform sin fuga
  tipificador <- function() {
    media <- NULL; desv <- NULL
    list(fit = function(x) { media <<- mean(x); desv <<- sd(x) },
         transform = function(x) (x - media) / desv)
  }
  set.seed(2026); tr <- rnorm(1000, 50, 10); te <- rnorm(5, 60, 10)
  tf <- tipificador(); tf$fit(tr)
  cat("train tipificado media/sd:", round(mean(tf$transform(tr)), 3),
      round(sd(tf$transform(tr)), 3), "\n")
  cat("test con params de train (no 0):", round(mean(tf$transform(te)), 2), "\n")
  # estrategia
  esc <- list(min_max = \(x) (x - min(x)) / (max(x) - min(x)),
              z = \(x) (x - mean(x)) / sd(x))
  cat("estrategia z rango:", round(range(esc$z(tr)), 2), "\n")
  # decorador
  con_registro <- function(fn, nombre)
    function(...) { cat("[log]", nombre, "\n"); fn(...) }
  sq <- con_registro(function(x) x^2, "cuadrado")
  cat("decorado:", sq(5), "\n")
}

demo_dominio <- function() {
  Duracion <- new_class("Duracion", properties = list(seg = class_numeric),
    validator = function(self) if (any(self@seg < 0)) "duracion negativa")
  duracion <- function(txt) {
    p <- as.integer(strsplit(txt, ":")[[1]]); Duracion(seg = p[1] * 60 + p[2]) }
  method(format, Duracion) <- function(x, ...)
    sprintf("%d:%02d", x@seg %/% 60, x@seg %% 60)
  method(print, Duracion) <- function(x, ...) cat(format(x), "\n")
  method(`+`, list(Duracion, Duracion)) <- function(e1, e2)
    Duracion(seg = e1@seg + e2@seg)
  print(duracion("3:52") + duracion("4:10"))
  album <- list(duracion("3:52"), duracion("4:10"), duracion("2:45"))
  cat("total album: "); print(Reduce(`+`, album))
  cat("vectorial: "); print(Duracion(seg = c(232, 250, 165)))
  cat("valida negativo:",
      tryCatch(duracion("-1:00"), error = \(e) "error"), "\n")
}

if (sys.nframe() == 0L) {
  demos <- list(s3 = demo_s3, s4 = demo_s4, r6 = demo_r6, s7 = demo_s7,
                tidyeval = demo_tidyeval, patrones = demo_patrones,
                dominio = demo_dominio)
  for (nombre in names(demos)) {
    cat("\n==", nombre, "==\n")
    demos[[nombre]]()
  }
}
