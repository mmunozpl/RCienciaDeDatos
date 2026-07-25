# src/cap14_figuras.R -- figuras del cap. 14 (modelado con tidymodels).
# Genera como PDF vectorial (ggplot real) las figuras que dependen de datos:
# progresion de R2 por modelo, mapa de la malla de hiperparametros, importancia
# por permutacion (DALEX), dependencia parcial (PDP), resumen SHAP (shapviz) y
# curvas ROC de la clasificacion. Los diagramas conceptuales (k-pliegues, el
# workflow) van en TikZ en el .tex. Determinista (semillas fijas).

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(arrow)
  library(rsample); library(recipes); library(parsnip); library(workflows)
  library(tune); library(yardstick); library(dials)
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
guardar <- function(n,p,w=5.6,h=3.4){
  ggsave(file.path(FIGS,paste0(n,".pdf")),p,width=w,height=h,device=cairo_pdf)
  cat("  ",n,".pdf\n",sep=""); }
NUM <- c("danceability","energy","speechiness","acousticness",
         "instrumentalness","liveness","valence","tempo")

sub <- read_parquet(MUS) |> filter(track_genre %in% GEN) |>
  distinct(track_id, track_genre, .keep_all=TRUE) |>
  group_by(track_genre) |> slice_head(n=1000) |> ungroup() |>
  mutate(track_genre=factor(track_genre,levels=GEN))
set.seed(2026); div <- initial_split(sub, prop=0.8, strata=track_genre)
tr <- training(div); te <- testing(div)
rsq_de <- function(spec, fml=loudness~., datos=tr[c("loudness",NUM)]) {
  set.seed(2026); aj <- workflow() |> add_recipe(recipe(fml,datos)) |> add_model(spec) |> fit(tr)
  pr <- predict(aj, te) |> bind_cols(te["loudness"]); rsq(pr,loudness,.pred)$.estimate
}

# --- 1. progresion de R2 por modelo -----------------------------------------
{
  r_lin4 <- { aj<-lm(loudness~danceability+energy+acousticness+valence,tr)
              1-sum((te$loudness-predict(aj,te))^2)/sum((te$loudness-mean(te$loudness))^2) }
  d <- data.frame(
    modelo=c("lineal\n(4 rasgos)","lineal\n(8 rasgos)","bosque\naleatorio","boosting\najustado"),
    r2=c(r_lin4,
         rsq_de(linear_reg()),
         rsq_de(rand_forest(trees=500,mode="regression")|>set_engine("ranger")),
         0.922))   # boosting ajustado: del guion principal (last_fit)
  d$modelo <- factor(d$modelo, levels=d$modelo)
  d$dest <- d$r2==max(d$r2)
  p <- ggplot(d, aes(modelo, r2, fill=dest)) + geom_col(width=0.68) +
    geom_text(aes(label=sprintf("%.3f",r2)), vjust=-0.5, size=3.2) +
    scale_fill_manual(values=c("grey70",OKABE[2]), guide="none") +
    coord_cartesian(ylim=c(0,1)) +
    labs(x=NULL, y=expression(R^2~"en test"),
         title="El flujo mejora el modelo paso a paso")
  guardar("cap14_progresion", p, 6, 3.4)
}

# --- 2. la malla de hiperparametros (RMSE por combinacion) ------------------
{
  esp <- boost_tree(trees=500, tree_depth=tune(), learn_rate=tune(),
                    mode="regression") |> set_engine("xgboost")
  wf <- workflow() |> add_recipe(recipe(loudness~., tr[c("loudness",NUM)])) |> add_model(esp)
  malla <- grid_regular(tree_depth(range=c(2L,8L)), learn_rate(range=c(-2,-0.5)), levels=4)
  set.seed(2026); pl <- vfold_cv(tr, v=5)
  res <- tune_grid(wf, pl, grid=malla, metrics=metric_set(rmse))
  cm <- collect_metrics(res) |> filter(.metric=="rmse")
  p <- ggplot(cm, aes(factor(tree_depth), factor(round(learn_rate,3)), fill=mean)) +
    geom_tile(color="white") +
    geom_text(aes(label=sprintf("%.2f",mean)), size=2.9) +
    scale_fill_gradient(low=OKABE[3], high="grey85", name="RMSE (CV)") +
    labs(x="profundidad del arbol (tree_depth)", y="tasa de aprendizaje (learn_rate)",
         title="La malla de hiperparametros: cada celda, una validacion cruzada")
  guardar("cap14_malla", p, 5.8, 3.8)
}

# --- 3. importancia por permutacion (DALEX) ---------------------------------
{
  set.seed(2026)
  aj <- workflow() |> add_recipe(recipe(loudness~., tr[c("loudness",NUM)])) |>
    add_model(boost_tree(trees=400,mode="regression")|>set_engine("xgboost")) |> fit(tr)
  ex <- DALEX::explain(aj, data=te[NUM], y=te$loudness, verbose=FALSE,
                       predict_function=function(m,d) predict(m,d)$.pred)
  set.seed(1); imp <- DALEX::model_parts(ex, B=10)
  ag <- aggregate(dropout_loss~variable, imp[imp$permutation==0,], mean)
  base <- ag$dropout_loss[ag$variable=="_full_model_"]
  ag <- ag[!ag$variable %in% c("_full_model_","_baseline_"),]
  ag <- ag[order(ag$dropout_loss),]; ag$variable <- factor(ag$variable, ag$variable)
  p <- ggplot(ag, aes(dropout_loss, variable)) +
    geom_col(fill=OKABE[1], width=0.7) +
    geom_vline(xintercept=base, linetype="dashed", color="grey50") +
    labs(x="RMSE tras barajar la variable (raya: modelo intacto)", y=NULL,
         title="Importancia por permutacion: que rasgos predicen el volumen")
  guardar("cap14_importancia", p, 5.8, 3.4)
}

# --- 4. dependencia parcial (PDP) -------------------------------------------
{
  set.seed(2026)
  aj <- workflow() |> add_recipe(recipe(loudness~., tr[c("loudness",NUM)])) |>
    add_model(boost_tree(trees=400,mode="regression")|>set_engine("xgboost")) |> fit(tr)
  pdp1 <- function(var){
    rej <- seq(quantile(tr[[var]],0.02), quantile(tr[[var]],0.98), length.out=40)
    base <- tr[NUM]
    sapply(rej, function(v){ d<-base; d[[var]]<-v
      mean(predict(aj, d)$.pred) })
  }
  d1 <- data.frame(x=seq(quantile(tr$energy,0.02),quantile(tr$energy,0.98),length.out=40),
                   y=pdp1("energy"), rasgo="energy")
  d2 <- data.frame(x=seq(quantile(tr$acousticness,0.02),quantile(tr$acousticness,0.98),length.out=40),
                   y=pdp1("acousticness"), rasgo="acousticness")
  d <- rbind(d1,d2)
  d$rasgo <- factor(d$rasgo, levels=c("energy","acousticness"))  # energy a la izquierda
  p <- ggplot(d, aes(x,y,color=rasgo)) + geom_line(linewidth=0.9) +
    facet_wrap(~rasgo, scales="free_x") +
    scale_color_manual(values=OKABE[c(2,1)], guide="none") +
    labs(x="valor del rasgo", y="volumen previsto (dB)",
         title="Dependencia parcial: como mueve cada rasgo la prediccion")
  guardar("cap14_pdp", p, 6, 3.2)
}

# --- 5. resumen SHAP (shapviz) ----------------------------------------------
{
  set.seed(2026)
  aj <- workflow() |> add_recipe(recipe(loudness~., tr[c("loudness",NUM)])) |>
    add_model(boost_tree(trees=400,mode="regression")|>set_engine("xgboost")) |> fit(tr)
  fitted <- extract_fit_engine(aj)
  set.seed(1); idx <- sample(nrow(te), 800)
  sv <- shapviz::shapviz(fitted, X_pred=as.matrix(te[idx,NUM]), X=te[idx,NUM])
  p <- shapviz::sv_importance(sv, kind="beeswarm", max_display=8) +
    labs(title="Valores SHAP: contribucion de cada rasgo, pista a pista") +
    theme_minimal(base_size=10) +
    theme(plot.title=element_text(face="bold",size=11), legend.position="right")
  guardar("cap14_shap", p, 6, 3.6)
}

# --- 6. curvas ROC de la clasificacion (una-contra-resto) -------------------
{
  set.seed(2026)
  aj <- workflow() |> add_recipe(recipe(track_genre~., tr[c("track_genre",NUM)])) |>
    add_model(boost_tree(trees=400,mode="classification")|>set_engine("xgboost")) |> fit(tr)
  prp <- predict(aj, te, type="prob") |> bind_cols(te["track_genre"])
  rc <- roc_curve(prp, track_genre, paste0(".pred_",GEN))
  auc <- round(roc_auc(prp, track_genre, paste0(".pred_",GEN))$.estimate,3)
  p <- ggplot(rc, aes(1-specificity, sensitivity, color=.level)) +
    geom_abline(slope=1, linetype="dashed", color="grey60") +
    geom_path(linewidth=0.7) +
    scale_color_manual(values=OKABE, name=NULL) +
    coord_equal() +
    labs(x="tasa de falsos positivos", y="tasa de verdaderos positivos",
         title=paste0("ROC una-contra-resto por genero (AUC medio ",auc,")"))
  guardar("cap14_roc", p, 5, 4.4)
}

# --- 7. curva de calibracion (clasificacion binaria pop-vs-rock) ------------
{
  dos <- sub |> filter(track_genre %in% c("pop","rock")) |>
    mutate(clase=factor(track_genre, levels=c("rock","pop")))
  set.seed(2026); d2<-initial_split(dos,prop=0.8,strata=clase); t2<-training(d2); e2<-testing(d2)
  aj <- workflow() |> add_recipe(recipe(clase~., t2[c("clase",NUM)])) |>
    add_model(boost_tree(trees=400,mode="classification")|>set_engine("xgboost")) |> fit(t2)
  prob <- predict(aj, e2, type="prob")$.pred_pop
  real <- as.numeric(e2$clase=="pop")
  br <- cut(prob, breaks=seq(0,1,0.1), include.lowest=TRUE)
  cal <- data.frame(prob, real, br) |> group_by(br) |>
    summarise(declarada=mean(prob), observada=mean(real), n=n(), .groups="drop")
  p <- ggplot(cal, aes(declarada, observada)) +
    geom_abline(slope=1, linetype="dashed", color="grey55") +
    geom_line(color=OKABE[1], linewidth=0.8) +
    geom_point(aes(size=n), color=OKABE[1]) +
    scale_size_area(max_size=6, guide="none") +
    coord_equal(xlim=c(0,1), ylim=c(0,1)) +
    labs(x="probabilidad declarada por el modelo", y="frecuencia observada de acierto",
         title="Curva de calibracion: ¿son fieles las probabilidades?")
  guardar("cap14_calibracion", p, 4.6, 4.2)
}

cat("\nFIN cap14 figuras OK\n")
