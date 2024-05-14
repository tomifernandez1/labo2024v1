# Corrida general del Workflow de semillerio

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("rlang")
require("yaml")
require("data.table")
require("ParamHelpers")

# creo environment global
envg <- env()

envg$EXPENV <- list()
envg$EXPENV$exp_dir <- "~/buckets/b1/exp_semillero/" ##cambiar nombre exp
dir.create(envg$EXPENV$exp_dir, showWarnings = FALSE)
envg$EXPENV$wf_dir <- "~/buckets/b1/flow1/" ##cambias nombre flow
dir.create(envg$EXPENV$wf_dir, showWarnings = FALSE)
envg$EXPENV$wf_dir_local <- "~/flow1/"
envg$EXPENV$repo_dir <- "~/labo2024v1/"
envg$EXPENV$datasets_dir <- "~/buckets/b1/datasets/"
envg$EXPENV$arch_sem <- "mis_semillas.txt"

# default
envg$EXPENV$gcloud$RAM <- 512
envg$EXPENV$gcloud$cCPU <- 24

#------------------------------------------------------------------------------
# Error catching

options(error = function() {
  traceback(20)
  options(error = NULL)
  
  cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
    file = "z-Rabort.txt",
    append = TRUE 
    )

  stop("exiting after script error")
})
#------------------------------------------------------------------------------
# inicializaciones varias

dir.create( envg$EXPENV$wf_dir, showWarnings = FALSE)
dir.create( envg$EXPENV$wf_dir, showWarnings = FALSE)
dir.create( envg$EXPENV$wf_dir_local, showWarnings = FALSE)
setwd( envg$EXPENV$wf_dir_local )

#------------------------------------------------------------------------------
# cargo la  "libreria" de los experimentos

exp_lib <- paste0( envg$EXPENV$repo_dir,"/src/lib/z590_exp_lib_01.r")
source( exp_lib )

#------------------------------------------------------------------------------

DT_incorporar_dataset_baseline <- function( pmyexp, parch, pserver="local")
{
  if( -1 == (param_local <- exp_init_datos( pmyexp, parch, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z511_DT_incorporar_dataset.r"

  param_local$primarykey <- c("numero_de_cliente", "foto_mes" )
  param_local$entity_id <- c("numero_de_cliente" )
  param_local$periodo <- c("foto_mes" )
  param_local$clase <- c("clase_ternaria" )

  return( exp_correr_script( param_local ) ) # linea fija}
}
#------------------------------------------------------------------------------
# Catastrophe Analysis  baseline

CA_catastrophe_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z521_CA_reparar_dataset.r"

  # Opciones MachineLearning EstadisticaClasica Ninguno
  param_local$metodo <- "MachineLearning" # MachineLearning EstadisticaClasica Ninguno

  return( exp_correr_script( param_local ) ) # linea fija}
}
#------------------------------------------------------------------------------
# Data Drifting baseline

DR_drifting_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z531_DR_corregir_drifting.r"

  # No me engraso las manos con Feature Engineering manual
  param_local$variables_intrames <- TRUE
  # valores posibles
  #  "ninguno", "rank_simple", "rank_cero_fijo", "deflacion", "estandarizar"
  param_local$metodo <- "rank_cero_fijo"

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# FE historia baseline

FE_historia_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija


  param_local$meta$script <- "/src/workflow-01/z541_FE_historia.r"

  param_local$lag1 <- TRUE
  param_local$lag2 <- TRUE # no me engraso con los lags de orden 2
  param_local$lag3 <- FALSE # no me engraso con los lags de orden 3

  # baseline
  param_local$Tendencias1$run <- TRUE  # FALSE, no corre nada de lo que sigue
  param_local$Tendencias1$ventana <- 6
  param_local$Tendencias1$tendencia <- TRUE
  param_local$Tendencias1$minimo <- TRUE
  param_local$Tendencias1$maximo <- TRUE
  param_local$Tendencias1$promedio <- TRUE
  param_local$Tendencias1$ratioavg <- TRUE
  param_local$Tendencias1$ratiomax <- TRUE

  # baseline
  param_local$Tendencias2$run <- TRUE
  param_local$Tendencias2$ventana <- 6
  param_local$Tendencias2$tendencia <- TRUE
  param_local$Tendencias2$minimo <- TRUE
  param_local$Tendencias2$maximo <- TRUE
  param_local$Tendencias2$promedio <- TRUE
  param_local$Tendencias2$ratioavg <- TRUE
  param_local$Tendencias2$ratiomax <- TRUE


  # vaseline
  param_local$RandomForest$run <- TRUE
  param_local$RandomForest$num.trees <- 20
  param_local$RandomForest$max.depth <- 4
  param_local$RandomForest$min.node.size <- 1000
  param_local$RandomForest$mtry <- 40

  # varia de 0.0 a 2.0, si es 0.0 NO se activan
  param_local$CanaritosAsesinos$ratio <- 0.0
  # desvios estandar de la media, para el cutoff
  param_local$CanaritosAsesinos$desvios <- 4.0

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Training Strategy baseline 202109

TS_strategy_baseline_202109 <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z551_TS_training_strategy.r"


  param_local$future <- c(202109)
  param_local$final_train <- c(202107, 202106, 
                               202105, 202104, 202103, 202102, 202101, 
                               202012, 202011, 202010, 202009, 202008, 202007, # 202006 - Excluyo este mes con variables rotas
                               202005, 202004, 202003, 202002, 202001,
                               201912, 201911, 201910, #- Excluyo este mes con variables rotas
                               201909, 201908, 201907, 201906, 201905, #- Excluyo este mes con variables rotas
                               201904, 201903 #, 201902, 201901 - Excluyo estos meses para tener misma cantidad que en el training
  )
  
  
  param_local$train$training <- c(202105, 202104, 202103, 202102, 202101, 
                                  202012, 202011, 202010, 202009, 202008, 202007, # 202006 - Excluyo este mes con variables rotas
                                  202005, 202004, 202003, 202002, 202001,
                                  201912, 201911, 201910, #- Excluyo este mes con variables rotas
                                  201909, 201908, 201907, 201906, 201905, #- Excluyo este mes con variables rotas
                                  201904, 201903, 201902, 201901 # Incluyo estos 2 ultimos meses que no aplican arriba
  )
  param_local$train$validation <- c(202106)
  param_local$train$testing <- c(202107)

  # undersampling  baseline
  param_local$train$undersampling <- 0.5

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Training Strategy baseline  202107

TS_strategy_baseline_202107 <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z551_TS_training_strategy.r"


  param_local$future <- c(202107)
  param_local$final_train <- c(202105, 202104, # Acá pongo los dos considerados en el validation y testing del training
                               202103, 202102, 202101, 
                               202012, 202011, 202010, 202009, 202008, 202007, # 202006
                               202005, 202004, 202003, 202002, 202001, 
                               201912, 201911, 201910, 
                               201909, 201908, 201907, 201906, 201905, 
                               201904, 201903
  ) # en esta parte excluyo dos meses para compensar
  
  
  param_local$train$training <- c(202103, 202102, 202101, 
                                  202012, 202011, 202010, 202009, 202008, 202007, #202006,
                                  202005, 202004, 202003, 202002, 202001, 
                                  201912, 201911, 201910,
                                  201909, 201908, 201907, 201906, 201905, 
                                  201904, 201903, 201902, 201901 # Incluyo dos adicionales para compensar
  )
  param_local$train$validation <- c(202104)
  param_local$train$testing <- c(202105)

  # undersampling  baseline
  param_local$train$undersampling <- 0.5

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# Hyperparamteter Tuning baseline

HT_tuning_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z561_HT_lightgbm.r"

  # En caso que se haga cross validation, se usa esta cantidad de folds
  param_local$lgb_crossvalidation_folds <- 5

  # Hiperparametros  del LightGBM
  #  los que tienen un solo valor son los que van fijos
  #  los que tienen un vector,  son los que participan de la Bayesian Optimization
  
  param_local$lgb_param <- list(
    boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest # NC: Puse dart para probar
    objective = "binary",
    metric = "custom",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE, # para reducir warnings
    verbosity = -100,
    
    max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
    
    min_sum_hessian_in_leaf = 0.5, #  min_sum_hessian_in_leaf >= 0.0
    
    lambda_l1 = 0.1, # lambda_l1 >= 0.0
    lambda_l2 = 0.01, # lambda_l2 >= 0.0
    max_bin = 31L, # lo debo dejar fijo, no participa de la BO
    num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds
    
    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
    
    scale_pos_weight = 0.4, # scale_pos_weight > 0.0
    bagging_fraction = 0.8, # 0.0 < bagging_fraction <= 1.0
    pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
    neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
    is_unbalance = FALSE, #
    extra_trees = FALSE,
    learning_rate = 0.01, # Sugerencia del profe, y mis experimentos dieron también usar este learning rate
    
    # White Gloves Bayesian Optimization, with a happy narrow exploration
    
    min_gain_to_split = 0.5, # min_gain_to_split >= 0.0
    feature_fraction = 0.3,
    min_data_in_leaf = c( 500L, 900L, "integer" ),   
    num_leaves = c( 700L, 1000L, "integer" )
  )


  # una Beyesian de Guantes Blancos, solo hace 15 iteraciones
  param_local$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# proceso ZZ_final  baseline

ZZ_final_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z571_ZZ_final.r"

  # Que modelos quiero, segun su posicion en el ranking e la Bayesian Optimizacion, ordenado por ganancia descendente
  param_local$modelos_rank <- c(1)

  param_local$kaggle$envios_desde <-  9500L
  param_local$kaggle$envios_hasta <- 11500L
  param_local$kaggle$envios_salto <-   500L

  # para el caso que deba graficar
  param_local$graficar$envios_desde <-  8000L
  param_local$graficar$envios_hasta <- 20000L
  param_local$graficar$ventana_suavizado <- 2001L

  # Una corrida de Guantes Blancos solo usa 5 semillas
  param_local$qsemillas <- 5

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
# proceso ZZ_final  baseline

ZZ_final_semillerio_baseline <- function( pmyexp, pinputexps, pserver="local")
{
  if( -1 == (param_local <- exp_init( pmyexp, pinputexps, pserver ))$resultado ) return( 0 )# linea fija

  param_local$meta$script <- "/src/workflow-01/z881_ZZ_final_semillerio.r"

  # Que modelos quiero, segun su posicion en el ranking e la Bayesian Optimizacion, ordenado por ganancia descendente
  param_local$modelos_rank <- c(1)

  param_local$kaggle$envios_desde <-  9500L
  param_local$kaggle$envios_hasta <- 11500L
  param_local$kaggle$envios_salto <-   500L

  # para el caso que deba graficar
  param_local$graficar$envios_desde <-  8000L
  param_local$graficar$envios_hasta <- 20000L
  param_local$graficar$ventana_suavizado <- 2001L

  # El parametro fundamental de semillerio
  # Es la cantidad de LightGBM's que ensamblo
  param_local$semillerio <- 20

  return( exp_correr_script( param_local ) ) # linea fija
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# A partir de ahora comienza la seccion de Workflows Completos
#------------------------------------------------------------------------------
# Este es el  Workflow baseline con semillerio
# Que predice 202109
# y ya genera archivos para Kaggle

corrida_baseline_semillerio_202109 <- function( pnombrewf, pvirgen=FALSE )
{
  if( -1 == exp_wf_init( pnombrewf, pvirgen) ) return(0) # linea fija

  DT_incorporar_dataset_baseline( "DT0001-sem", "competencia_2024.csv.gz")
  CA_catastrophe_baseline( "CA0001-sem", "DT0001-sem" )

  DR_drifting_baseline( "DR0001-sem", "CA0001-sem" )
  FE_historia_baseline( "FE0001-sem", "DR0001-sem" )

  TS_strategy_baseline_202109( "TS0001-sem", "FE0001-sem" )

  HT_tuning_baseline( "HT0001-sem", "TS0001-sem" )

  # El ZZ depente de HT y TS
  ZZ_final_semillerio_baseline( "ZZ0001-sem", c("HT0001-sem","TS0001-sem") )


  exp_wf_end( pnombrewf, pvirgen ) # linea fija
}
#------------------------------------------------------------------------------
# Este es el  Workflow baseline con semillerio
# Que predice 202107
# genera completas curvas de ganancia
#   NO genera archivos para Kaggle
# por favor notal como este script parte de FE0001


corrida_baseline_semillerio_202107 <- function( pnombrewf, pvirgen=FALSE )
{
  if( -1 == exp_wf_init( pnombrewf, pvirgen) ) return(0) # linea fija

  DT_incorporar_dataset_baseline( "DT0001-sem", "competencia_2024.csv.gz")
  CA_catastrophe_baseline( "CA0001-sem", "DT0001-sem" )

  DR_drifting_baseline( "DR0001-sem", "CA0001-sem" )
  FE_historia_baseline( "FE0001-sem", "DR0001-sem" )

  TS_strategy_baseline_202107( "TS0002-sem", "FE0001-sem" )

  HT_tuning_baseline( "HT0002-sem", "TS0002-sem" )

  # El ZZ depente de HT y TS
  ZZ_final_semillerio_baseline( "ZZ0002-sem", c("HT0002-sem","TS0002-sem") )


  exp_wf_end( pnombrewf, pvirgen ) # linea fija
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa


corrida_baseline_semillerio_202109( "basem01" )


# Luego partiendo de  FE0001
# genero TS0002, HT0002 y ZZ0002

#corrida_baseline_semillerio_202107( "basem02" )

 