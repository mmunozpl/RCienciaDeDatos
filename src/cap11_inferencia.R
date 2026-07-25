# src/cap11_inferencia.R -- estadistica descriptiva, probabilidad e inferencia.
# Acompana al cap. 11: reproduce las cifras (descriptivos, distribuciones, LGN/TCL,
# intervalos de confianza, bootstrap con infer, contrastes t/Wilcoxon/ANOVA/chi2,
# permutacion, d de Cohen, p.adjust, potencia) sobre la rebanada de 6 generos.
# Cada cifra se ejecuto antes de imprimirse; semillas fijas (el azar de R no es
# portable). infer, effectsize y boot deben estar instalados.

suppressPackageStartupMessages({library(dplyr); library(arrow)})
GEN <- c("classical","hip-hop","jazz","pop","reggaeton","rock")
buscar <- function(c) { r <- c[file.exists(c)][1]; if (is.na(r)) NULL else r }
MUS <- buscar(c("data/processed/musica.parquet", "../data/processed/musica.parquet"))

leer_reb <- function() read_parquet(MUS) |> filter(track_genre %in% GEN) |>
  distinct(track_id, track_genre, .keep_all = TRUE)

# --- 1. descriptivos: centro, dispersion, forma -----------------------------
demo_describir <- function(mus) {
  x <- mus$energy
  cat("energy -> media:", round(mean(x),3), "| mediana:", round(median(x),3),
      "| sd:", round(sd(x),4), "| MAD:", round(mad(x),4), "| IQR:", round(IQR(x),3), "\n")
  cat("asimetria:", round(mean((x-mean(x))^3)/sd(x)^3,3),
      "| curtosis (exceso):", round(mean((x-mean(x))^4)/sd(x)^4-3,3), "\n")
  l <- mus$loudness
  cat("loudness (sesgada) -> media:", round(mean(l),2), "vs mediana:", round(median(l),2), "\n")
  cat("cor(energy,loudness):", round(cor(x, l),3),
      "| cor(energy,acousticness):", round(cor(x, mus$acousticness),3), "\n")
}

# --- 2. distribuciones: familia d/p/q/r -------------------------------------
demo_distribuciones <- function() {
  cat("dnorm(0):", round(dnorm(0),4), "| pnorm(1.96):", round(pnorm(1.96),4),
      "| qnorm(0.975):", round(qnorm(0.975),3), "\n")
  cat("P(|Z|<1.96):", round(pnorm(1.96)-pnorm(-1.96),4), "\n")
  cat("dbinom(3;10,0.3):", round(dbinom(3,10,0.3),4),
      "| dpois(5;3):", round(dpois(5,3),4), "\n")
}

# --- 3. LGN y TCL (simulacion) ----------------------------------------------
demo_limite <- function(mus) {
  set.seed(2026); pob <- mus$energy
  cat("LGN (sd de la media muestral):\n")
  for (n in c(10,100,1000,5000))
    cat(sprintf("  n=%4d  sd=%.4f\n", n, sd(replicate(200, mean(sample(pob,n,replace=TRUE))))))
  set.seed(7); pe <- rexp(1e5)
  cat("TCL (asimetria de la media, poblacion exp asim.", round(mean((pe-mean(pe))^3)/sd(pe)^3,2), "):\n")
  for (n in c(2,10,30,100)) {
    m <- replicate(2000, mean(sample(pe,n)))
    cat(sprintf("  n=%3d  asim=%.2f\n", n, mean((m-mean(m))^3)/sd(m)^3))
  }
}

# --- 4. intervalo de confianza y su cobertura -------------------------------
demo_ic <- function(mus) {
  x <- mus$energy
  ci <- t.test(x)$conf.int
  cat("IC 95% t.test de la media:", round(ci[1],4), "-", round(ci[2],4),
      "| EE:", round(sd(x)/sqrt(length(x)),5), "\n")
  set.seed(2026); mu <- mean(x)
  cubre <- replicate(500, { s <- sample(x,100); c <- t.test(s)$conf.int; mu>=c[1] && mu<=c[2] })
  cat("cobertura de 500 IC del 95%:", round(100*mean(cubre),1), "%\n")
}

# --- 5. bootstrap con infer (media, mediana, correlacion) -------------------
demo_bootstrap <- function(mus) {
  if (!requireNamespace("infer", quietly=TRUE)) { cat("(infer no instalado)\n"); return() }
  library(infer)
  set.seed(2026)
  bm <- mus |> specify(response=energy) |> generate(2000,"bootstrap") |> calculate(stat="mean")
  cat("IC bootstrap de la MEDIA:", paste(round(unlist(get_confidence_interval(bm,0.95,"percentile")),4),collapse=" - "), "\n")
  bmd <- mus |> specify(response=energy) |> generate(2000,"bootstrap") |> calculate(stat="median")
  cat("IC bootstrap de la MEDIANA:", paste(round(unlist(get_confidence_interval(bmd,0.95,"percentile")),4),collapse=" - "), "\n")
  bc <- mus |> specify(loudness ~ energy) |> generate(2000,"bootstrap") |> calculate(stat="correlation")
  cat("IC bootstrap de la CORRELACION:", paste(round(unlist(get_confidence_interval(bc,0.95,"percentile")),4),collapse=" - "), "\n")
}

# --- 6. contrastes: permutacion, Welch, Wilcoxon, ANOVA, chi2, efecto -------
demo_contrastes <- function(mus) {
  dos <- mus |> filter(track_genre %in% c("pop","rock"))
  tt <- t.test(energy ~ track_genre, data=dos)
  cat("t de Welch pop vs rock: t=", round(tt$statistic,2), "df=", round(tt$parameter,0),
      "p=", format.pval(tt$p.value,digits=3), "\n")
  cat("Wilcoxon p=", format.pval(wilcox.test(energy~track_genre,dos)$p.value,digits=3), "\n")
  if (requireNamespace("infer", quietly=TRUE)) {
    library(infer); set.seed(7)
    obs <- dos |> specify(energy~track_genre) |> calculate("diff in means", order=c("pop","rock"))
    nulo <- dos |> specify(energy~track_genre) |> hypothesize(null="independence") |>
      generate(2000,"permute") |> calculate("diff in means", order=c("pop","rock"))
    cat("permutacion: dif obs", round(obs$stat,4), "p=",
        format.pval(get_p_value(nulo,obs,"two-sided")$p_value,digits=3), "\n")
  }
  av <- aov(energy ~ track_genre, data=mus); s <- summary(av)[[1]]
  cat("ANOVA: F=", round(s$`F value`[1],0), "p<2e-16\n")
  ch <- chisq.test(table(mus$track_genre, mus$explicit))
  cat("chi2 explicit~genero: X2=", round(ch$statistic,0), "\n")
  if (requireNamespace("effectsize", quietly=TRUE)) {
    library(effectsize)
    cat("d de Cohen pop-rock:", round(cohens_d(energy~track_genre,data=dos)$Cohens_d,3),
        "| eta^2 ANOVA:", round(eta_squared(av)$Eta2,3), "\n")
  }
}

# --- 7. trampas: p-hacking, significativo trivial, potencia -----------------
demo_trampas <- function(mus) {
  set.seed(11)
  ps <- replicate(20, t.test(rnorm(100), rnorm(100))$p.value)
  cat("p-hacking: de 20 tests sobre ruido, significativos:", sum(ps<0.05),
      "| tras Bonferroni:", sum(p.adjust(ps,"bonferroni")<0.05), "\n")
  r <- cor.test(mus$tempo, mus$duration_ms)
  cat("significativo pero trivial: cor(tempo,duration)=", round(r$estimate,3),
      "p=", format.pval(r$p.value,digits=3), "(n=", nrow(mus), ")\n")
  n <- power.t.test(delta=0.1, sd=sd(mus$energy), sig.level=0.05, power=0.8)$n
  cat("potencia: n por grupo para detectar delta=0.1:", ceiling(n), "\n")
}

# --- 8. integrador: explicita vs limpia -------------------------------------
demo_integrador <- function(mus) {
  print(mus |> group_by(explicit) |> summarise(n=n(), media=round(mean(energy),3), sd=round(sd(energy),3)))
  ci <- t.test(energy~explicit, data=mus)$conf.int
  cat("IC dif (limpio-explicito):", round(ci[1],4), round(ci[2],4),
      "| p=", format.pval(t.test(energy~explicit,mus)$p.value,digits=3), "\n")
  if (requireNamespace("effectsize", quietly=TRUE))
    cat("d de Cohen:", round(effectsize::cohens_d(energy~explicit,data=mus)$Cohens_d,3), "\n")
}

if (sys.nframe() == 0L) {
  if (is.null(MUS)) { cat("(sin musica.parquet)\n"); quit() }
  mus <- leer_reb(); cat("rebanada:", nrow(mus), "pistas\n")
  cat("\n== 1. describir ==\n"); demo_describir(mus)
  cat("\n== 2. distribuciones d/p/q/r ==\n"); demo_distribuciones()
  cat("\n== 3. LGN y TCL ==\n"); demo_limite(mus)
  cat("\n== 4. intervalo de confianza ==\n"); demo_ic(mus)
  cat("\n== 5. bootstrap (infer) ==\n"); demo_bootstrap(mus)
  cat("\n== 6. contrastes ==\n"); demo_contrastes(mus)
  cat("\n== 7. trampas ==\n"); demo_trampas(mus)
  cat("\n== 8. integrador ==\n"); demo_integrador(mus)
  cat("\nFIN cap11 OK\n")
}
