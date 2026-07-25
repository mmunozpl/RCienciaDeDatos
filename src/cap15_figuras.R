# src/cap15_figuras.R -- figuras del cap. 15 (privacidad y confidencialidad).
# Genera como PDF vectorial (ggplot real) las figuras que dependen de datos:
# distribucion de la talla de grupo k (fino vs generalizado), el precio de la
# privacidad diferencial (error ~ 1/eps en log-log), el histograma de respuestas
# ruidosas a dos presupuestos, la curva ROC del ataque de pertenencia, y el
# compromiso privacidad-utilidad de los datos sinteticos. Los diagramas
# conceptuales (jerarquia de generalizacion, mapa de amenazas) van en TikZ.

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(arrow); library(yardstick)
})
buscar <- function(c){ r<-c[file.exists(c)][1]; if(is.na(r)) NULL else r }
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
rlaplace <- function(n, b){ u<-runif(n)-0.5; -b*sign(u)*log(1-2*abs(u)) }

perf <- read_parquet(PERF)

# --- 1. la talla del grupo k: fino vs generalizado --------------------------
{
  kf <- perf |> group_by(cp, fecha_nac, sexo) |> mutate(k=n()) |> pull(k)
  g <- perf |> mutate(edad10=(edad%/%10)*10) |> group_by(pais, edad10, sexo) |> mutate(k=n()) |> pull(k)
  d <- bind_rows(
    tibble(k=pmin(kf,10), tipo="fino (cp+fecha+sexo)"),
    tibble(k=pmin(g,10),  tipo="generalizado (pais+decenio+sexo)"))
  d$tipo <- factor(d$tipo, c("fino (cp+fecha+sexo)","generalizado (pais+decenio+sexo)"))
  res <- d |> group_by(tipo, k) |> summarise(n=n(), .groups="drop") |>
    group_by(tipo) |> mutate(prop=n/sum(n)) |> ungroup()
  p <- ggplot(res, aes(factor(k), prop, fill=tipo)) +
    geom_col(position="dodge", width=0.75) +
    geom_vline(xintercept=4.5, linetype="dashed", color="grey40") +
    annotate("text", x=3.0, y=max(res$prop)*0.9, label="k<5:\nreidentificable", size=2.8, color="grey30") +
    scale_fill_manual(values=OKABE[c(2,1)], name=NULL) +
    scale_x_discrete(labels=c(1:9,"10+")) +
    labs(x="tamaño del grupo k (registros con los mismos cuasi-identificadores)",
         y="proporción de registros", title="Diluirse en la multitud: el efecto de generalizar")
  guardar("cap15_kanon", p, 6.2, 3.6)
}

# --- 2. el precio de la privacidad diferencial: error ~ 1/eps (log-log) ------
{
  set.seed(2026)
  npais <- perf |> distinct(pais) |> nrow()
  eps <- c(0.05,0.1,0.2,0.5,1,2,5,10)
  d <- data.frame(eps=eps,
    err=sapply(eps, function(e) mean(replicate(300, mean(abs(rlaplace(npais, 1/e)))))))
  p <- ggplot(d, aes(eps, err)) +
    geom_line(data=data.frame(eps=eps, err=1/eps), color="grey55", linetype="dashed") +
    geom_point(color=OKABE[1], size=2) + geom_line(color=OKABE[1], linewidth=0.7) +
    scale_x_log10() + scale_y_log10() +
    annotate("text", x=2, y=2.2, label="ley 1/ε", color="grey40", size=3.2) +
    labs(x="presupuesto de privacidad ε (menos = más privado)",
         y="error medio del conteo (personas)",
         title="El precio de la privacidad: error frente a ε")
  guardar("cap15_epsilon", p, 5.6, 3.4)
}

# --- 3. histograma de respuestas ruidosas a dos presupuestos ----------------
{
  set.seed(2026)
  real <- perf |> group_by(pais) |> summarise(pr=sum(premium), .groups="drop")
  v <- max(real$pr)
  d <- bind_rows(
    tibble(resp=v+rlaplace(4000, 1/1.0), eps="ε = 1 (poco ruido)"),
    tibble(resp=v+rlaplace(4000, 1/0.1), eps="ε = 0,1 (mucho ruido)"))
  d$eps <- factor(d$eps, c("ε = 1 (poco ruido)","ε = 0,1 (mucho ruido)"))
  p <- ggplot(d, aes(resp, fill=eps)) +
    geom_histogram(bins=50, alpha=0.7, position="identity", color=NA) +
    geom_vline(xintercept=v, linetype="dashed", color="grey30") +
    annotate("text", x=v, y=Inf, label=paste("real =",v), vjust=1.5, hjust=-0.1, size=2.9, color="grey30") +
    scale_fill_manual(values=OKABE[c(1,2)], name=NULL) +
    labs(x="respuesta publicada (con ruido de Laplace)", y="frecuencia",
         title="La misma consulta, dos presupuestos de privacidad")
  guardar("cap15_dp_hist", p, 6, 3.4)
}

# --- 4. curva ROC del ataque de pertenencia ---------------------------------
{
  if (requireNamespace("xgboost", quietly=TRUE)) {
    set.seed(42)
    d <- perf |> mutate(sexoF=as.integer(sexo=="F"))
    feats <- c("edad","cp","energia_media","minutos_dia","sexoF")
    idx <- sample(nrow(d)); tr <- d[idx[1:2000],]; te <- d[idx[2001:7000],]
    Xtr<-as.matrix(tr[feats]); Xte<-as.matrix(te[feats])
    dtr<-xgboost::xgb.DMatrix(Xtr,label=tr$premium)
    roc_de <- function(par){
      set.seed(42); bst<-xgboost::xgb.train(par,dtr,nrounds=par$nrounds,verbose=0)
      ptr<-predict(bst,Xtr); pte<-predict(bst,Xte)
      ctr<-ifelse(tr$premium==1,ptr,1-ptr); cte<-ifelse(te$premium==1,pte,1-pte)
      ev<-tibble(miembro=factor(c(rep("si",length(ctr)),rep("no",length(cte))),levels=c("no","si")),
                 conf=c(ctr,cte))
      list(rc=roc_curve(ev,miembro,conf,event_level="second"),
           auc=roc_auc(ev,miembro,conf,event_level="second")$.estimate)
    }
    a<-roc_de(list(max_depth=0,eta=0.3,min_child_weight=1,nrounds=600,objective="binary:logistic",nthread=4))
    b<-roc_de(list(max_depth=3,eta=0.1,min_child_weight=100,lambda=1,nrounds=100,objective="binary:logistic",nthread=4))
    rc <- bind_rows(
      mutate(a$rc, modelo=sprintf("sobreajustado (AUC %.2f)", a$auc)),
      mutate(b$rc, modelo=sprintf("regularizado (AUC %.2f)", b$auc)))
    p <- ggplot(rc, aes(1-specificity, sensitivity, color=modelo)) +
      geom_abline(slope=1, linetype="dashed", color="grey60") +
      geom_path(linewidth=0.9) + coord_equal() +
      scale_color_manual(values=OKABE[c(2,1)], name=NULL) +
      labs(x="tasa de falsos positivos", y="tasa de verdaderos positivos",
           title="El ataque de pertenencia: memorizar es filtrar")
    guardar("cap15_mia", p, 4.8, 4.4)
  }
}

# --- 5. compromiso privacidad-utilidad (barrido de generalizacion) ----------
{
  # a mas generalizacion (menos digitos de cp / edad mas gruesa): mas k-anonimato
  # (menos riesgo) pero menos utilidad (menos entropia = menos informacion)
  set.seed(2026)
  niveles <- list(
    list(nom="crudo\n(cp+edad+sexo)", cuasi=c("cp","edad","sexo"), gen=function(d) d),
    list(nom="edad a 5 años", cuasi=c("cp5","edad5","sexo"),
         gen=function(d) mutate(d, cp5=(cp%/%1)*1, edad5=(edad%/%5)*5)),
    list(nom="edad a decenios\n+ cp regional", cuasi=c("cpr","edad10","sexo"),
         gen=function(d) mutate(d, cpr=(cp%/%100)*100, edad10=(edad%/%10)*10)),
    list(nom="solo país\n+ decenio", cuasi=c("pais","edad10","sexo"),
         gen=function(d) mutate(d, edad10=(edad%/%10)*10)))
  res <- lapply(niveles, function(L){
    d <- L$gen(perf)
    k <- d |> group_by(across(all_of(L$cuasi))) |> mutate(k=n()) |> pull(k)
    riesgo <- 100*mean(k<5)                     # % reidentificable (a menos, mejor privacidad)
    data.frame(nivel=L$nom, riesgo=riesgo)
  }) |> bind_rows()
  res$nivel <- factor(res$nivel, levels=res$nivel)
  p <- ggplot(res, aes(nivel, riesgo)) +
    geom_col(fill=OKABE[1], width=0.65) +
    geom_text(aes(label=sprintf("%.0f%%", riesgo)), vjust=-0.4, size=3) +
    labs(x="nivel de generalización (izq. = más detalle, der. = más privacidad)",
         y="% de registros reidentificables (k<5)",
         title="El compromiso: cuanto más se generaliza, menos se identifica")
  guardar("cap15_compromiso", p, 6.2, 3.4)
}

cat("\nFIN cap15 figuras OK\n")
