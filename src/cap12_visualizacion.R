# src/cap12_visualizacion.R -- visualizacion de datos con ggplot2.
# Acompana al cap. 12: genera TODAS las figuras del capitulo como PDF vectoriales
# en latex/figures/cap12_*.pdf (ggplot real, no imitaciones), sobre la rebanada de
# 6 generos del catalogo. Paleta Okabe-Ito (apta para daltonicos). Ejecutar desde
# Private/ (Rscript src/cap12_visualizacion.R) o desde src/ (busca el parquet).
# Determinista: semillas fijas donde hay azar.

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(arrow); library(scales)
})

# --- rutas -------------------------------------------------------------------
buscar <- function(c) { r <- c[file.exists(c)][1]; if (is.na(r)) NULL else r }
MUS <- buscar(c("data/processed/musica.parquet", "../data/processed/musica.parquet"))
FIGS <- buscar(c("latex/figures", "../latex/figures"))
if (is.null(FIGS)) { dir.create("latex/figures", recursive = TRUE, showWarnings = FALSE); FIGS <- "latex/figures" }
GEN <- c("classical","hip-hop","jazz","pop","reggaeton","rock")
OKABE <- c("#0072B2","#D55E00","#009E73","#E69F00","#CC79A7","#56B4E9")  # daltonico-safe

mus <- read_parquet(MUS) |> filter(track_genre %in% GEN) |>
  distinct(track_id, track_genre, .keep_all = TRUE)

# tema comun: minimalista, tinta al servicio del dato (Tufte)
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "bottom")
theme_set(tema)
guardar <- function(nombre, plot, w = 6, h = 3.6) {
  ggsave(file.path(FIGS, paste0(nombre, ".pdf")), plot, width = w, height = h,
         device = cairo_pdf)
  cat("  ", nombre, ".pdf\n", sep = "")
}

# --- 1. cuarteto de Anscombe: por que hay que dibujar -----------------------
{
  aq <- datasets::anscombe |>
    pivot_longer(everything(), names_to = c(".value","conjunto"),
                 names_pattern = "(.)(.)")
  p <- ggplot(aq, aes(x, y)) +
    geom_point(color = OKABE[1], size = 1.8) +
    geom_smooth(method = "lm", se = FALSE, color = OKABE[2], linewidth = 0.7) +
    facet_wrap(~ conjunto, nrow = 1, labeller = label_both) +
    labs(title = "El cuarteto de Anscombe",
         subtitle = "misma media, misma correlacion, misma recta... formas opuestas")
  guardar("cap12_anscombe", p, 7, 2.6)
}

# --- 2. barras: energia media por genero (ordenadas) ------------------------
{
  d <- mus |> group_by(track_genre) |>
    summarise(energia = mean(energy), .groups = "drop")
  p <- ggplot(d, aes(reorder(track_genre, energia), energia)) +
    geom_col(fill = OKABE[1], width = 0.7) +
    coord_flip() +
    labs(x = NULL, y = "energía media", title = "Energía media por género")
  guardar("cap12_barras", p, 5.5, 3)
}

# --- 3. barras agrupadas vs apiladas: genero x explicito --------------------
{
  d <- mus |> mutate(explicito = if_else(explicit, "explícito", "limpio")) |>
    count(track_genre, explicito)
  p <- ggplot(d, aes(reorder(track_genre, n, sum), n, fill = explicito)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = OKABE[c(3,2)], name = NULL) +
    coord_flip() +
    labs(x = NULL, y = "pistas", title = "Contenido explícito por género")
  guardar("cap12_agrupadas", p, 5.5, 3.2)
}

# --- 4. histograma + densidad de energy -------------------------------------
{
  p <- ggplot(mus, aes(energy)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = OKABE[6], color = "white", alpha = 0.8) +
    geom_density(color = OKABE[2], linewidth = 0.9) +
    labs(x = "energy", y = "densidad", title = "Distribución de la energía")
  guardar("cap12_histograma", p, 5.5, 3)
}

# --- 5. caja + violin de energy por genero ----------------------------------
{
  p <- ggplot(mus, aes(reorder(track_genre, energy, median), energy)) +
    geom_violin(fill = OKABE[6], alpha = 0.35, color = NA) +
    geom_boxplot(width = 0.18, outlier.size = 0.4, fill = "white") +
    coord_flip() +
    labs(x = NULL, y = "energy", title = "Energía por género: caja sobre violín")
  guardar("cap12_caja_violin", p, 5.5, 3.4)
}

# --- 6. ECDF de energy para tres generos ------------------------------------
{
  tres <- mus |> filter(track_genre %in% c("classical","pop","reggaeton"))
  p <- ggplot(tres, aes(energy, color = track_genre)) +
    stat_ecdf(linewidth = 0.9) +
    scale_color_manual(values = OKABE[c(1,2,3)], name = NULL) +
    labs(x = "energy", y = "F(energy)  (proporción acumulada)",
         title = "Función de distribución empírica (ECDF)")
  guardar("cap12_ecdf", p, 5.5, 3.2)
}

# --- 7. dispersion energy vs loudness + suavizado ---------------------------
{
  set.seed(12); sub <- mus |> slice_sample(n = 1500)
  p <- ggplot(sub, aes(loudness, energy)) +
    geom_point(alpha = 0.25, color = OKABE[1], size = 0.9) +
    geom_smooth(method = "lm", color = OKABE[2], se = TRUE, linewidth = 0.8) +
    labs(x = "loudness (dB)", y = "energy",
         title = "Energía frente a volumen (r = 0,85)")
  guardar("cap12_dispersion", p, 5.5, 3.2)
}

# --- 8. sobreimpresion: alpha vs hexbin -------------------------------------
{
  library(patchwork)
  p1 <- ggplot(mus, aes(loudness, energy)) +
    geom_point(size = 0.6) + labs(title = "puntos opacos (sobreimpresión)")
  p2 <- ggplot(mus, aes(loudness, energy)) +
    geom_hex(bins = 30) + scale_fill_gradient(low = "#DEEBF7", high = OKABE[1]) +
    labs(title = "geom_hex (densidad)") + theme(legend.position = "none")
  guardar("cap12_overplot", p1 + p2, 7, 3)
}

# --- 9. facetas: energy vs valence por genero -------------------------------
{
  set.seed(1); sub <- mus |> group_by(track_genre) |> slice_sample(n = 250) |> ungroup()
  p <- ggplot(sub, aes(valence, energy)) +
    geom_point(alpha = 0.3, color = OKABE[1], size = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = OKABE[2], linewidth = 0.6) +
    facet_wrap(~ track_genre) +
    labs(x = "valence (positividad)", y = "energy",
         title = "Energía y positividad, un panel por género")
  guardar("cap12_facetas", p, 6.5, 4)
}

# --- 10. color por genero (Okabe-Ito) sobre dos rasgos ----------------------
{
  set.seed(2); sub <- mus |> group_by(track_genre) |> slice_sample(n = 200) |> ungroup()
  p <- ggplot(sub, aes(acousticness, energy, color = track_genre)) +
    geom_point(alpha = 0.6, size = 1) +
    scale_color_manual(values = OKABE, name = NULL) +
    labs(x = "acousticness", y = "energy",
         title = "Los géneros se separan en el plano acústica–energía")
  guardar("cap12_color", p, 6, 3.6)
}

# --- 11. el factor de mentira: eje truncado vs completo ---------------------
{
  library(patchwork)
  d <- mus |> group_by(track_genre) |> summarise(e = mean(energy)) |>
    filter(track_genre %in% c("pop","rock","hip-hop"))
  base <- ggplot(d, aes(track_genre, e)) + geom_col(fill = OKABE[2], width = 0.6) +
    labs(x = NULL, y = "energía media")
  p1 <- base + coord_cartesian(ylim = c(0.60, 0.69)) +
    labs(title = "eje truncado: exagera")
  p2 <- base + coord_cartesian(ylim = c(0, 1)) +
    labs(title = "eje desde cero: honesto")
  guardar("cap12_liefactor", p1 + p2, 6.5, 3)
}

# --- 12. de exploratorio a presentacion -------------------------------------
{
  library(patchwork)
  d <- mus |> group_by(track_genre) |> summarise(e = mean(energy)) |>
    mutate(destacado = track_genre == "reggaeton")
  crudo <- ggplot(d, aes(track_genre, e)) + geom_col() +
    labs(title = "exploratorio (rápido)")
  pulido <- ggplot(d, aes(reorder(track_genre, e), e, fill = destacado)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c("grey70", OKABE[2]), guide = "none") +
    geom_text(aes(label = round(e, 2)), hjust = -0.15, size = 3) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = NULL, y = "energía media",
         title = "El reggaetón lidera en energía",
         subtitle = "energía media por género (0–1)") +
    theme(plot.title.position = "plot")
  guardar("cap12_comunicacion", crudo + pulido, 8, 3.2)
}

# --- 13. integrador: retrato del catalogo -----------------------------------
{
  d <- mus |> group_by(track_genre) |>
    summarise(energia = mean(energy), bailable = mean(danceability),
              n = n(), pop = mean(popularity), .groups = "drop")
  p <- ggplot(d, aes(energia, bailable)) +
    geom_point(aes(size = n, color = pop)) +
    ggrepel::geom_text_repel(aes(label = track_genre), size = 3, seed = 1) +
    scale_color_gradient(low = OKABE[6], high = OKABE[2], name = "popularidad") +
    scale_size_area(max_size = 12, name = "nº pistas") +
    labs(x = "energía media", y = "bailabilidad media",
         title = "Un retrato de seis géneros",
         subtitle = "posición = sonido; tamaño = nº de pistas; color = popularidad")
  guardar("cap12_integrador", p, 6.5, 4.2)
}

# --- 14. jerarquia perceptual: barras vs tarta (mismos datos) ---------------
{
  library(patchwork)
  d <- mus |> count(track_genre) |> mutate(track_genre = reorder(track_genre, n))
  barras <- ggplot(d, aes(track_genre, n)) + geom_col(fill = OKABE[1]) +
    coord_flip() + labs(x = NULL, y = "pistas", title = "barras: posición y longitud")
  tarta <- ggplot(d, aes("", n, fill = track_genre)) +
    geom_col(width = 1) + coord_polar("y") +
    scale_fill_manual(values = OKABE) +
    labs(title = "tarta: ángulo y área", x = NULL, y = NULL) +
    theme_void() + theme(legend.position = "right", legend.title = element_blank(),
                         plot.title = element_text(face = "bold", size = 12))
  guardar("cap12_jerarquia", barras + tarta, 7.5, 3)
}

# --- 15. mapa de calor de correlaciones entre rasgos ------------------------
{
  rasgos <- mus |> select(energy, loudness, danceability, valence,
                          acousticness, tempo, popularity)
  m <- cor(rasgos)
  cm <- as.data.frame(as.table(m)); names(cm) <- c("a","b","r")
  p <- ggplot(cm, aes(a, b, fill = r)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", r)), size = 2.6) +
    scale_fill_gradient2(low = OKABE[1], mid = "white", high = OKABE[2],
                         midpoint = 0, limits = c(-1, 1)) +
    labs(x = NULL, y = NULL, title = "Correlaciones entre rasgos de audio") +
    theme(axis.text.x = element_text(angle = 40, hjust = 1),
          panel.grid = element_blank(), legend.position = "right")
  guardar("cap12_heatmap", p, 5.8, 4.4)
}

# --- 16. temas: el mismo grafico, tres vestidos -----------------------------
{
  library(patchwork)
  base <- ggplot(mus, aes(energy)) + geom_histogram(bins = 25, fill = OKABE[1]) +
    labs(x = "energy", y = NULL)
  g1 <- base + theme_gray()    + labs(title = "theme_gray (defecto)")
  g2 <- base + theme_minimal() + labs(title = "theme_minimal")
  g3 <- base + theme_classic() + labs(title = "theme_classic")
  guardar("cap12_temas", g1 + g2 + g3, 8.5, 2.6)
}

# --- 17. anotacion: senalar lo importante en el grafico ---------------------
{
  set.seed(3); sub <- mus |> slice_sample(n = 1200)
  p <- ggplot(sub, aes(loudness, energy)) +
    geom_point(alpha = 0.2, color = "grey50") +
    annotate("rect", xmin = -8, xmax = 0, ymin = 0.8, ymax = 1.02,
             fill = OKABE[2], alpha = 0.12) +
    annotate("text", x = -4, y = 0.6, label = "fuerte y enérgico",
             color = OKABE[2], fontface = "bold", size = 3.3) +
    annotate("segment", x = -4, y = 0.64, xend = -4, yend = 0.79,
             color = OKABE[2], arrow = arrow(length = unit(2, "mm"))) +
    labs(x = "loudness (dB)", y = "energy",
         title = "Las pistas potentes se agrupan arriba a la derecha")
  guardar("cap12_anotado", p, 6, 3.4)
}

# --- 18. densidades superpuestas por genero ---------------------------------
{
  tres <- mus |> filter(track_genre %in% c("classical","pop","reggaeton"))
  p <- ggplot(tres, aes(energy, fill = track_genre, color = track_genre)) +
    geom_density(alpha = 0.35, linewidth = 0.7) +
    scale_fill_manual(values = OKABE[c(1,2,3)], name = NULL) +
    scale_color_manual(values = OKABE[c(1,2,3)], name = NULL) +
    labs(x = "energy", y = "densidad",
         title = "Distribución de la energía por género")
  guardar("cap12_densidades", p, 5.8, 3.2)
}

# --- 19. lineas: tendencia de un rasgo a lo largo de otro -------------------
{
  d <- mus |>
    mutate(tramo = cut(tempo, breaks = seq(40, 220, 15))) |>
    group_by(tramo) |>
    summarise(energia = mean(energy), n = n(), .groups = "drop") |>
    filter(n >= 20) |>
    mutate(centro = seq(47.5, by = 15, length.out = n()))
  p <- ggplot(d, aes(centro, energia)) +
    geom_line(color = OKABE[1], linewidth = 0.9) +
    geom_point(color = OKABE[1], size = 1.6) +
    labs(x = "tempo (pulsaciones por minuto)", y = "energía media",
         title = "La energía media crece con el tempo")
  guardar("cap12_lineas", p, 5.8, 3)
}

# --- 20. jitter: mostrar los puntos con pocos datos por grupo ---------------
{
  set.seed(9)
  poco <- mus |> group_by(track_genre) |> slice_sample(n = 25) |> ungroup()
  p <- ggplot(poco, aes(track_genre, energy)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.4, fill = OKABE[6]) +
    geom_jitter(width = 0.15, alpha = 0.6, color = OKABE[1], size = 1.1) +
    coord_flip() +
    labs(x = NULL, y = "energy",
         title = "Con pocos datos, muestra los puntos (jitter sobre caja)")
  guardar("cap12_jitter", p, 5.8, 3.2)
}

# --- 21. ggridges: comparar muchas distribuciones en cascada ----------------
{
  if (requireNamespace("ggridges", quietly = TRUE)) {
    library(ggridges)
    p <- ggplot(mus, aes(energy, reorder(track_genre, energy, median),
                         fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient(scale = 1.6, color = "white",
                                             linewidth = 0.3) +
      scale_fill_gradient(low = OKABE[6], high = OKABE[2], guide = "none") +
      labs(x = "energy", y = NULL,
           title = "Energía por género: densidades en cascada (ridgeline)")
    guardar("cap12_ridges", p, 5.8, 3.6)
  } else cat("  (ggridges no instalado; se omite cap12_ridges)\n")
}

# --- 22. GGally: matriz de dispersion (panorama bivariante) -----------------
{
  if (requireNamespace("GGally", quietly = TRUE)) {
    library(GGally)
    set.seed(4)
    sub <- mus |> select(energy, loudness, danceability, acousticness) |>
      slice_sample(n = 800)
    p <- GGally::ggpairs(sub,
                         lower = list(continuous = GGally::wrap("points", alpha = 0.15, size = 0.5)),
                         upper = list(continuous = GGally::wrap("cor", size = 3)),
                         diag  = list(continuous = GGally::wrap("densityDiag"))) +
      theme_minimal(base_size = 9)
    guardar("cap12_pares", p, 6.2, 5)
  } else cat("  (GGally no instalado; se omite cap12_pares)\n")
}

cat("\nFIN cap12 figuras OK\n")
