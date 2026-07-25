# src/cap13_figuras.R -- figuras del cap. 13 (aprendizaje automatico).
# Genera como PDF vectorial (ggplot real, no imitaciones) las figuras que
# dependen de datos: curva de sobreajuste, polinomios ajustados, curva ROC,
# matriz de confusion, convergencia del descenso de gradiente, perdida de la
# red y la leccion tabular. Los diagramas conceptuales (particion, arquitectura
# de la red) van en TikZ dentro del .tex. Determinista (semillas fijas).

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(arrow)
  library(rsample); library(yardstick)
})
buscar <- function(c){ r<-c[file.exists(c)][1]; if(is.na(r)) NULL else r }
MUS  <- buscar(c("data/processed/musica.parquet","../data/processed/musica.parquet"))
FIGS <- buscar(c("latex/figures","../latex/figures")); if(is.null(FIGS)) FIGS<-"latex/figures"
GEN  <- c("pop","rock","classical","hip-hop","jazz","reggaeton")
OKABE <- c("#0072B2","#D55E00","#009E73","#E69F00","#CC79A7","#56B4E9")
tema <- theme_minimal(base_size=11) +
  theme(panel.grid.minor=element_blank(),
        plot.title=element_text(face="bold",size=12), legend.position="bottom")
theme_set(tema)
guardar <- function(n,p,w=5.6,h=3.4) {
  ggsave(file.path(FIGS,paste0(n,".pdf")),p,width=w,height=h,device=cairo_pdf)
  cat("  ",n,".pdf\n",sep="")
}

sub <- read_parquet(MUS) |> filter(track_genre %in% GEN) |>
  distinct(track_id, track_genre, .keep_all=TRUE) |>
  group_by(track_genre) |> slice_head(n=1000) |> ungroup() |>
  mutate(track_genre=factor(track_genre,levels=GEN))
set.seed(2026); div <- initial_split(sub, prop=0.8, strata=track_genre)
tr <- training(div); te <- testing(div)

# --- 1. curva de sobreajuste (MAE train/test vs grado) ----------------------
{
  set.seed(2026); peq <- tr[sample(nrow(tr),40),]
  xtr<-peq$energy; ytr<-peq$loudness; xte<-te$energy; yte<-te$loudness
  grados <- 1:15; d <- data.frame()
  for (g in grados) {
    aj <- lm(ytr ~ poly(xtr,g))
    d <- rbind(d, data.frame(grado=g,
      train=mean(abs(ytr-predict(aj))),
      test =mean(abs(yte-as.numeric(predict(aj,data.frame(xtr=xte)))))))
  }
  dl <- pivot_longer(d, c(train,test), names_to="conjunto", values_to="mae")
  dl$conjunto <- factor(dl$conjunto, c("train","test"), c("entrenamiento","test"))
  p <- ggplot(dl, aes(grado, mae, color=conjunto)) +
    geom_line(linewidth=0.9) + geom_point(size=1.6) +
    scale_color_manual(values=OKABE[c(1,2)], name=NULL) +
    coord_cartesian(ylim=c(0,4)) +
    scale_x_continuous(breaks=c(1,3,5,7,9,11,13,15)) +
    labs(x="grado del polinomio (complejidad)", y="error absoluto medio (dB)",
         title="Sobreajuste: la brecha entre entrenamiento y test")
  guardar("cap13_sobreajuste", p)
}

# --- 2. polinomios ajustados de distinto grado ------------------------------
{
  set.seed(2026); peq <- tr[sample(nrow(tr),40),]
  rej <- data.frame(energy=seq(min(peq$energy),max(peq$energy),length.out=200))
  curvas <- lapply(c(1,3,15), function(g){
    aj <- lm(loudness ~ poly(energy,g), data=peq)
    data.frame(energy=rej$energy, loudness=predict(aj,rej), grado=paste("grado",g))
  }) |> bind_rows()
  curvas$grado <- factor(curvas$grado, paste("grado",c(1,3,15)))
  p <- ggplot() +
    geom_point(data=peq, aes(energy,loudness), color="grey55", size=1.3, alpha=0.8) +
    geom_line(data=curvas, aes(energy,loudness,color=grado), linewidth=0.9) +
    scale_color_manual(values=OKABE[c(3,1,2)], name=NULL) +
    coord_cartesian(ylim=c(min(peq$loudness)-3, max(peq$loudness)+3)) +
    labs(x="energy", y="loudness (dB)",
         title="El mismo dato, tres complejidades")
  guardar("cap13_polinomios", p)
}

# --- 3. curva ROC pop vs rock -----------------------------------------------
{
  dos <- sub |> filter(track_genre %in% c("pop","rock")) |>
    mutate(clase=factor(track_genre, levels=c("rock","pop")))
  set.seed(2026); d2 <- initial_split(dos, prop=0.8, strata=clase)
  t2<-training(d2); e2<-testing(d2)
  aj <- glm(clase ~ danceability+energy+loudness+speechiness+acousticness+valence+tempo,
            data=t2, family=binomial())
  ev <- tibble(truth=e2$clase, prob=predict(aj,e2,type="response"))
  rc <- roc_curve(ev, truth, prob, event_level="second")
  auc <- round(roc_auc(ev, truth, prob, event_level="second")$.estimate,3)
  p <- ggplot(rc, aes(1-specificity, sensitivity)) +
    geom_abline(slope=1, linetype="dashed", color="grey60") +
    geom_path(color=OKABE[1], linewidth=0.9) +
    annotate("text", x=0.62, y=0.28, label=paste("AUC =",auc),
             color=OKABE[1], fontface="bold") +
    coord_equal() +
    labs(x="tasa de falsos positivos (1 - especificidad)",
         y="tasa de verdaderos positivos (sensibilidad)",
         title="Curva ROC: pop frente a rock")
  guardar("cap13_roc", p, 4.6, 4.2)
}

# --- 4. matriz de confusion (6 generos, bosque) -----------------------------
{
  if (requireNamespace("ranger", quietly=TRUE)) {
    feats <- c("danceability","energy","loudness","speechiness","acousticness",
               "instrumentalness","liveness","valence","tempo")
    set.seed(2026)
    rf <- ranger::ranger(track_genre ~ ., data=tr[c("track_genre",feats)], num.trees=400)
    pr <- factor(predict(rf, te[feats])$predictions, levels=GEN)
    cm <- as.data.frame(table(verdad=te$track_genre, prediccion=pr))
    cm <- cm |> group_by(verdad) |> mutate(prop=Freq/sum(Freq)) |> ungroup()
    p <- ggplot(cm, aes(prediccion, verdad, fill=prop)) +
      geom_tile(color="white") +
      geom_text(aes(label=Freq), size=2.8) +
      scale_fill_gradient(low="white", high=OKABE[1], guide="none") +
      scale_y_discrete(limits=rev(GEN)) +          # diagonal alineada con x
      theme(axis.text.x=element_text(angle=35,hjust=1), panel.grid=element_blank()) +
      labs(x="predicho", y="verdadero",
           title="Matriz de confusion: seis generos (bosque aleatorio)")
    guardar("cap13_confusion", p, 5.2, 4.2)
  }
}

# --- 5. convergencia del descenso de gradiente ------------------------------
{
  FEAT <- c("energy","acousticness","danceability","valence")
  X <- scale(as.matrix(tr[FEAT])); y <- tr$loudness; n<-nrow(X); Xb<-cbind(1,X)
  beta<-rep(0,ncol(Xb)); d<-data.frame()
  for (eta in c(0.01,0.1,0.5)) {
    b<-rep(0,ncol(Xb))
    for (it in 1:200) {
      err<-as.numeric(Xb%*%b)-y; b<-b-eta*crossprod(Xb,err)/n
      if (it%%2==1) d<-rbind(d,data.frame(iter=it,ECM=mean(err^2),eta=paste("eta =",eta)))
    }
  }
  d$eta <- factor(d$eta, paste("eta =",c(0.01,0.1,0.5)))
  p <- ggplot(d, aes(iter, ECM, color=eta)) + geom_line(linewidth=0.9) +
    scale_color_manual(values=OKABE[c(3,1,2)], name=NULL) +
    coord_cartesian(ylim=c(10,60)) +
    labs(x="iteracion", y="error cuadratico medio",
         title="Descenso de gradiente: el papel de la tasa de aprendizaje")
  guardar("cap13_gradiente", p)
}

# --- 6. perdida de la red desde cero ----------------------------------------
{
  dos <- sub |> filter(track_genre %in% c("pop","rock"))
  set.seed(2026); d2 <- initial_split(dos, prop=0.8, strata=track_genre); t2<-training(d2)
  feats <- c("danceability","energy","loudness","speechiness","acousticness","valence","tempo")
  Xtr<-scale(as.matrix(t2[feats])); ytr<-as.numeric(t2$track_genre=="pop")
  relu<-function(z)pmax(z,0); sigm<-function(z)1/(1+exp(-z))
  set.seed(1); H<-8; dd<-ncol(Xtr)
  W1<-matrix(rnorm(dd*H,sd=0.3),dd,H); b1<-rep(0,H)
  W2<-matrix(rnorm(H,sd=0.3),H,1); b2<-0; eta<-0.05; n<-nrow(Xtr); hist<-data.frame()
  for (ep in 1:400) {
    Z1<-sweep(Xtr%*%W1,2,b1,"+"); A1<-relu(Z1)
    Z2<-as.numeric(A1%*%W2)+b2; A2<-sigm(Z2)
    L<--mean(ytr*log(A2+1e-9)+(1-ytr)*log(1-A2+1e-9)); hist<-rbind(hist,data.frame(epoca=ep,perdida=L))
    dZ2<-(A2-ytr)/n; dW2<-crossprod(A1,dZ2); db2<-sum(dZ2)
    dZ1<-outer(dZ2,as.numeric(W2))*(Z1>0); dW1<-crossprod(Xtr,dZ1); db1<-colSums(dZ1)
    W2<-W2-eta*dW2; b2<-b2-eta*db2; W1<-W1-eta*dW1; b1<-b1-eta*db1
  }
  p <- ggplot(hist, aes(epoca, perdida)) + geom_line(color=OKABE[2], linewidth=0.9) +
    labs(x="epoca (pasada por los datos)", y="perdida (entropia cruzada)",
         title="La red aprende: la perdida baja epoca a epoca")
  guardar("cap13_perdida", p)
}

# --- 7. la leccion tabular (exactitud por modelo) ---------------------------
{
  feats <- c("danceability","energy","loudness","speechiness","acousticness",
             "instrumentalness","liveness","valence","tempo")
  acc <- function(pr) mean(pr==te$track_genre)
  res <- c()
  res["referencia (azar)"] <- 1/6
  if (requireNamespace("nnet",quietly=TRUE)) {
    aj<-nnet::multinom(as.formula(paste("track_genre ~",paste(feats,collapse="+"))),
                       data=tr,trace=FALSE,maxit=300); res["logistica"]<-acc(predict(aj,te))
  }
  if (requireNamespace("ranger",quietly=TRUE)) {
    set.seed(2026); rf<-ranger::ranger(track_genre~.,data=tr[c("track_genre",feats)],num.trees=400)
    res["bosque aleatorio"]<-acc(factor(predict(rf,te[feats])$predictions,levels=GEN))
  }
  if (requireNamespace("xgboost",quietly=TRUE)) {
    set.seed(2026); dtr<-xgboost::xgb.DMatrix(as.matrix(tr[feats]),label=as.integer(tr$track_genre)-1L)
    bst<-xgboost::xgb.train(list(objective="multi:softmax",num_class=6,eta=0.3,max_depth=6,nthread=4),
                            dtr,nrounds=120,verbose=0)
    res["boosting"]<-acc(factor(GEN[as.integer(predict(bst,as.matrix(te[feats])))+1L],levels=GEN))
  }
  d <- data.frame(modelo=names(res), acc=as.numeric(res))
  d$modelo <- factor(d$modelo, d$modelo[order(d$acc)])
  d$dest <- d$modelo %in% c("bosque aleatorio","boosting")
  p <- ggplot(d, aes(acc, modelo, fill=dest)) + geom_col(width=0.7) +
    geom_text(aes(label=sprintf("%.3f",acc)), hjust=-0.15, size=3) +
    scale_fill_manual(values=c("grey70",OKABE[2]), guide="none") +
    scale_x_continuous(limits=c(0,0.82), expand=expansion(mult=c(0,0.02))) +
    labs(x="exactitud en test (6 generos)", y=NULL,
         title="La leccion tabular: los arboles baten a lo lineal")
  guardar("cap13_tabular", p, 6, 3.2)
}

# --- 8. frontera de decision: logistica (recta) vs bosque (curva) -----------
{
  dos <- sub |> filter(track_genre %in% c("pop","rock")) |>
    mutate(clase=factor(track_genre, levels=c("rock","pop")))
  set.seed(2026); d2<-initial_split(dos,prop=0.8,strata=clase); t2<-training(d2)
  # rejilla sobre dos rasgos para pintar la frontera
  rx <- seq(0,1,length.out=120); ry <- seq(0,1,length.out=120)
  rej <- expand.grid(energy=rx, acousticness=ry)
  log_aj <- glm(clase ~ energy + acousticness, data=t2, family=binomial())
  rej$log <- predict(log_aj, rej, type="response")
  if (requireNamespace("ranger", quietly=TRUE)) {
    set.seed(2026)
    rf <- ranger::ranger(clase ~ energy + acousticness, data=t2, num.trees=300, probability=TRUE)
    rej$rf <- predict(rf, rej)$predictions[,"pop"]
  } else rej$rf <- rej$log
  rejl <- tidyr::pivot_longer(rej, c(log,rf), names_to="modelo", values_to="p")
  rejl$modelo <- factor(rejl$modelo, c("log","rf"),
                        c("logística: frontera recta","bosque: frontera curva"))
  set.seed(1); pts <- t2[sample(nrow(t2),400),]
  p <- ggplot() +
    geom_raster(data=rejl, aes(energy, acousticness, fill=p), alpha=0.7) +
    geom_contour(data=rejl, aes(energy, acousticness, z=p), breaks=0.5, color="grey20", linewidth=0.5) +
    geom_point(data=pts, aes(energy, acousticness, color=track_genre), size=0.5, alpha=0.6) +
    scale_fill_gradient2(low=OKABE[1], mid="white", high=OKABE[2], midpoint=0.5, guide="none") +
    scale_color_manual(values=c(pop=OKABE[2],rock=OKABE[1]), name=NULL) +
    facet_wrap(~modelo) + coord_equal() +
    labs(x="energy", y="acousticness", title="La frontera de decisión: recta frente a curva")
  guardar("cap13_frontera", p, 6.6, 3.6)
}

cat("\nFIN cap13 figuras OK\n")
