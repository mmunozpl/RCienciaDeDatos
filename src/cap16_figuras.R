# src/cap16_figuras.R -- figuras del cap. 16 (ingenieria, despliegue, etica).
# Genera como PDF vectorial (ggplot real) las dos figuras que dependen de datos:
# la auditoria de equidad del clasificador de pago por sexo (tres tasas que no
# coinciden) y la deriva de la acustica entre dos generos (densidades corridas).
# Los diagramas conceptuales (ciclo de vida, integracion continua) van en TikZ.

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(arrow)
  library(rsample)
})
buscar <- function(c){ r<-c[file.exists(c)][1]; if(is.na(r)) NULL else r }
MUS  <- buscar(c("data/processed/musica.parquet","../data/processed/musica.parquet"))
PERF <- buscar(c("data/processed/perfiles_escucha.parquet","../data/processed/perfiles_escucha.parquet"))
FIGS <- buscar(c("latex/figures","../latex/figures")); if(is.null(FIGS)) FIGS<-"latex/figures"
OKABE <- c("#0072B2","#D55E00","#009E73","#E69F00","#CC79A7","#56B4E9")
tema <- theme_minimal(base_size=11) +
  theme(panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold",size=12), legend.position="bottom")
theme_set(tema)
guardar <- function(n,p,w=5.6,h=3.4){
  ggsave(file.path(FIGS,paste0(n,".pdf")),p,width=w,height=h,device=cairo_pdf)
  cat("  ",n,".pdf\n",sep=""); }

# --- 1. auditoria de equidad: tres tasas por sexo ---------------------------
{
  perf <- read_parquet(PERF) |> mutate(sexoF=as.integer(sexo=="F"))
  set.seed(2026); div <- initial_split(perf, prop=0.7, strata=premium)
  tr<-training(div); te<-testing(div)
  aj <- glm(premium ~ edad+minutos_dia+energia_media+n_artistas+sexoF, tr, family=binomial())
  prob <- predict(aj, te, type="response")
  tau <- quantile(prob, 1-mean(te$premium)); pred <- as.integer(prob>=tau)
  d <- lapply(c("M","F"), function(g){
    m<-te$sexo==g
    data.frame(sexo=g,
      metrica=c("selección","TPR (recall)","FPR"),
      valor=c(mean(pred[m]), mean(pred[m][te$premium[m]==1]), mean(pred[m][te$premium[m]==0])),
      prev=mean(te$premium[m]))
  }) |> bind_rows()
  d$sexo <- factor(d$sexo, c("M","F"), c("hombres","mujeres"))
  d$metrica <- factor(d$metrica, c("selección","TPR (recall)","FPR"))
  p <- ggplot(d, aes(metrica, valor, fill=sexo)) +
    geom_col(position="dodge", width=0.7) +
    geom_text(aes(label=sprintf("%.3f",valor)), position=position_dodge(0.7),
              vjust=-0.4, size=2.7) +
    scale_fill_manual(values=OKABE[c(1,5)], name=NULL) +
    coord_cartesian(ylim=c(0,0.5)) +
    labs(x=NULL, y="tasa",
         title="Auditoría de equidad: tres tasas que no coinciden")
  guardar("cap16_equidad", p, 6, 3.5)
}

# --- 2. deriva de datos: acustica de dos generos ----------------------------
{
  mus <- read_parquet(MUS)
  d <- mus |> filter(track_genre %in% c("classical","jazz")) |>
    mutate(genero=factor(track_genre, c("classical","jazz"),
                         c("clásica (referencia)","jazz (lote nuevo)")))
  ref<-d$acousticness[d$track_genre=="classical"]; nue<-d$acousticness[d$track_genre=="jazz"]
  ks <- suppressWarnings(ks.test(ref,nue))$statistic
  p <- ggplot(d, aes(acousticness, fill=genero, color=genero)) +
    geom_density(alpha=0.4, linewidth=0.7) +
    geom_vline(xintercept=mean(ref), color=OKABE[1], linetype="dashed") +
    geom_vline(xintercept=mean(nue), color=OKABE[2], linetype="dashed") +
    scale_fill_manual(values=OKABE[c(1,2)], name=NULL) +
    scale_color_manual(values=OKABE[c(1,2)], name=NULL) +
    annotate("text", x=0.35, y=Inf, vjust=1.5,
             label=sprintf("KS = %.2f  ·  PSI = 3.1", ks), size=3, color="grey30") +
    labs(x="acousticness", y="densidad",
         title="Deriva de datos: la acústica se desplaza entre géneros")
  guardar("cap16_drift", p, 6, 3.4)
}

cat("\nFIN cap16 figuras OK\n")
