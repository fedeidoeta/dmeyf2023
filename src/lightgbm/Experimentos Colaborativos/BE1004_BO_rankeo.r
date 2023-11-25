# Script BO baseline para Experimento colaborativo

# se entrena con clase_binaria2  POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm,

## be1004-rankeo: 
# + Train: c(201906, 201907, 201908, 201909, 
#                          201910, 201911, 201912, 202011, 
#                          202012, 202101, 202102, 202103, 
#                          202104, 202105)
# + Undersampling = 0.4
# + Rankeo variables monetarias


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("lightgbm")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento <- "BE1004_rankeo"

PARAM$input$dataset <- "./datasets/competencia_03_V2.csv.gz"

# los meses en los que vamos a entrenar
#  mucha magia emerger de esta eleccion
PARAM$input$testing <- c(202107)
PARAM$input$validation <- c(202106)
PARAM$input$training <- c(201906, 201907, 201908, 201909, 
                          201910, 201911, 201912, 202011, 
                          202012, 202101, 202102, 202103, 
                          202104, 202105)

# Experimentos

missing_values <- FALSE
media_lag_delta <- TRUE
rankeo <- FALSE


# Sin undersampling
PARAM$trainingstrategy$undersampling <- 0.4
PARAM$trainingstrategy$semilla_azar <- 270001

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Aqui poner su segunda semilla
PARAM$lgb_semilla <- 270001
#------------------------------------------------------------------------------

# Hiperparametros FIJOS de  lightgbm
PARAM$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  num_iterations = 9999, # un numero muy grande, lo limita early_stopping_rounds
  
  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  
  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$lgb_semilla
)


# Aqui se cargan los hiperparametros que se optimizan
#  en la Bayesian Optimization
PARAM$bo_lgb <- makeParamSet(
  makeNumericParam("learning_rate", lower = 0.02, upper = 0.3),
  makeNumericParam("feature_fraction", lower = 0.01, upper = 1.0),
  makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
  makeIntegerParam("min_data_in_leaf", lower = 100L, upper = 50000L)
)

# si usted es ambicioso, y tiene paciencia, podria subir este valor a 100
PARAM$bo_iteraciones <- 50 # iteraciones de la Optimizacion Bayesiana

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./exp/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)
  
  if (!file.exists(archivo)) # Escribo los titulos
  {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )
    
    cat(linea, file = archivo)
  }
  
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )
  
  cat(linea, file = archivo, append = TRUE) # grabo al archivo
  
  if (verbose) cat(linea) # imprimo por pantalla
}
#------------------------------------------------------------------------------
GLOBAL_arbol <- 0L
GLOBAL_gan_max <- -Inf
vcant_optima <- c()

fganancia_lgbm_meseta <- function(probs, datos) {
  vlabels <- get_field(datos, "label")
  vpesos <- get_field(datos, "weight")
  
  
  GLOBAL_arbol <<- GLOBAL_arbol + 1
  tbl <- as.data.table(list(
    "prob" = probs,
    "gan" = ifelse(vlabels == 1 & vpesos > 1,
                   PARAM$hyperparametertuning$POS_ganancia,
                   PARAM$hyperparametertuning$NEG_ganancia  )
  ))
  
  setorder(tbl, -prob)
  tbl[, posicion := .I]
  tbl[, gan_acum := cumsum(gan)]
  
  tbl[, gan_suavizada :=
        frollmean(
          x = gan_acum, n = 2001, align = "center",
          na.rm = TRUE, hasNA = TRUE
        )]
  
  gan <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  
  
  pos <- which.max(tbl[, gan_suavizada])
  vcant_optima <<- c(vcant_optima, pos)
  
  if (GLOBAL_arbol %% 10 == 0) {
    if (gan > GLOBAL_gan_max) GLOBAL_gan_max <<- gan
    
    cat("\r")
    cat(
      "Validate ", GLOBAL_iteracion, " ", " ",
      GLOBAL_arbol, "  ", gan, "   ", GLOBAL_gan_max, "   "
    )
  }
  
  
  return(list(
    "name" = "ganancia",
    "value" = gan,
    "higher_better" = TRUE
  ))
}
#------------------------------------------------------------------------------

EstimarGanancia_lightgbm <- function(x) {
  gc()
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1L
  
  # hago la union de los parametros basicos y los moviles que vienen en x
  param_completo <- c(PARAM$lgb_basicos, x)
  
  param_completo$early_stopping_rounds <-
    as.integer(400 + 4 / param_completo$learning_rate)
  
  GLOBAL_arbol <<- 0L
  GLOBAL_gan_max <<- -Inf
  vcant_optima <<- c()
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  modelo_train <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalidate),
    eval = fganancia_lgbm_meseta,
    param = param_completo,
    verbose = -100
  )
  
  cat("\n")
  
  cant_corte <- vcant_optima[modelo_train$best_iter]
  
  # aplico el modelo a testing y calculo la ganancia
  prediccion <- predict(
    modelo_train,
    data.matrix(dataset_test[, campos_buenos, with = FALSE])
  )
  
  tbl <- copy(dataset_test[, list("gan" = ifelse(clase_ternaria == "BAJA+2",
                                                 PARAM$hyperparametertuning$POS_ganancia, 
                                                 PARAM$hyperparametertuning$NEG_ganancia))])
  
  tbl[, prob := prediccion]
  setorder(tbl, -prob)
  tbl[, gan_acum := cumsum(gan)]
  tbl[, gan_suavizada := frollmean(
    x = gan_acum, n = 2001,
    align = "center", na.rm = TRUE, hasNA = TRUE
  )]
  
  
  ganancia_test <- tbl[, max(gan_suavizada, na.rm = TRUE)]
  
  cantidad_test_normalizada <- which.max(tbl[, gan_suavizada])
  
  rm(tbl)
  gc()
  
  ganancia_test_normalizada <- ganancia_test
  
  
  # voy grabando las mejores column importance
  if (ganancia_test_normalizada > GLOBAL_gananciamax) {
    GLOBAL_gananciamax <<- ganancia_test_normalizada
    tb_importancia <- as.data.table(lgb.importance(modelo_train))
    
    fwrite(tb_importancia,
           file = paste0("impo_", sprintf("%03d", GLOBAL_iteracion), ".txt"),
           sep = "\t"
    )
    
    rm(tb_importancia)
  }
  
  
  # logueo final
  ds <- list("cols" = ncol(dtrain), "rows" = nrow(dtrain))
  xx <- c(ds, copy(param_completo))
  
  xx$early_stopping_rounds <- NULL
  xx$num_iterations <- modelo_train$best_iter
  xx$estimulos <- cantidad_test_normalizada
  xx$ganancia <- ganancia_test_normalizada
  xx$iteracion_bayesiana <- GLOBAL_iteracion
  
  loguear(xx, arch = "BO_log.txt")
  
  set.seed(PARAM$lgb_semilla, kind = "L'Ecuyer-CMRG")
  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# creo la carpeta donde va el experimento
dir.create("./exp/ExpColaborativo/", showWarnings = FALSE)
dir.create(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"))

# en estos archivos quedan los resultados
kbayesiana <- paste0(PARAM$experimento, ".RDATA")
klog <- paste0(PARAM$experimento, ".txt")


####################### MISSING VALUES ##############################

if (missing_values) {

#  Coloca NA en 0 en meses y features selectos
cat("\nRegistros en 0 a NA\n")
zero_ratio <- list(
  list(mes = 202006, campo = 
         c("active_quarter", "catm_trx","catm_trx_other","ccajas_consultas",
           "ccajas_depositos", "ccajas_extracciones","ccajas_otras",
           "ccajas_transacciones","ccajeros_propios_descuentos",
           "ccallcenter_transacciones","ccheques_depositados",
           "ccheques_depositados_rechazados","ccheques_emitidos",
           "ccheques_emitidos_rechazados","ccomisiones_otras",
           "cextraccion_autoservicio","chomebanking_transacciones",
           "cmobile_app_trx","ctarjeta_debito_transacciones",
           "ctarjeta_master_descuentos","ctarjeta_master_transacciones",
           "ctarjeta_visa_descuentos","ctarjeta_visa_transacciones",
           "internet","mactivos_margen","matm","matm_other","mautoservicio",
           "mcajeros_propios_descuentos","mcheques_depositados",
           "mcheques_depositados_rechazados","mcheques_emitidos","mcheques_emitidos_rechazados",
           "mcomisiones","mcomisiones_otras","mcuentas_saldo","mextraccion_autoservicio",
           "mpasivos_margen","mrentabilidad","mrentabilidad_annual",
           "mtarjeta_master_consumo","mtarjeta_master_descuentos","mtarjeta_visa_consumo",
           "mtarjeta_visa_descuentos","tcallcenter","thomebanking","tmobile_app")),
  list(mes = 202009, campo = 
         c("ccajeros_propios_descuentos",
           "ctarjeta_master_descuentos",
           "ctarjeta_visa_descuentos",
           "mcajeros_propios_descuentos",
           "mtarjeta_master_descuentos",
           "mtarjeta_visa_descuentos")),
  list(mes = 202010, campo = 
         c("ccajeros_propios_descuentos",
           "ctarjeta_master_descuentos",
           "ctarjeta_visa_descuentos",
           "mcajeros_propios_descuentos",
           "mtarjeta_master_descuentos",
           "mtarjeta_visa_descuentos")),
  list(mes = 202102, campo = 
         c("ccajeros_propios_descuentos",
           "ctarjeta_master_descuentos",
           "ctarjeta_visa_descuentos",
           "mcajeros_propios_descuentos",
           "mtarjeta_master_descuentos",
           "mtarjeta_visa_descuentos")),
  list(mes = 202105, campo = 
         c("ccajas_depositos"))
)

for (par in zero_ratio) {
  mes <- par$mes
  feature <- par$campo
  dataset[foto_mes == mes, (feature) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols = feature]
}
}
#-------------------------------------------------------------------

########################### MEDIA + LAG + DELTA ########################
if (media_lag_delta) {
  cat("\nMedia en ventana\n")

  setorder(dataset, numero_de_cliente, foto_mes)

  cols_con_lag <- setdiff(
    colnames(dataset),
    c("clase_ternaria", "foto_mes", "numero_de_cliente",
      "cliente_edad", "cliente_antiguedad")
  )

  #----- media
  if (TRUE) {
    n <- 5L
    cols_media = c()
    for(col in cols_con_lag)
    {
      cols_media = c(cols_media, paste0("Media.", n,"_", col))
    }

    dataset[, (cols_media) := frollmean(.SD, n=(n), fill=NA, na.rm=TRUE, align="right", algo="fast"),
                          .SDcols = (cols_con_lag), by=numero_de_cliente]

    dataset[, (cols_media) := shift(.SD, n=1L, fill=NA, type="lag"),
                          .SDcols = (cols_media), by=numero_de_cliente]

    rm(cols_media, n)
  }

  cat("\nLag y delta lag de 1, 3 y 6  meses\n")

  n_lags = c(1,3,6)

  for (i in n_lags)
  {
    cols_lag = c()
    for(col in cols_con_lag)
    {
      cols_lag = c(cols_lag, paste0("lag.",i,".",col))
    }
    dataset[, (cols_lag) := shift(.SD, n=(i), fill=NA, type="lag"),
                          .SDcols = (cols_con_lag), by=numero_de_cliente]

    rm(cols_lag)
  }

  #----- delta lags
  if (TRUE) {
    for (i in n_lags)
    {
      for(col in cols_con_lag)
      {
        col_lag = paste0("lag.",i,".",col)
        col_delta_lag = paste0("delta.",i,".",col)
        dataset[, (col_delta_lag) := get(col) - get(col_lag)]
      }
      rm(col_lag, col_delta_lag)
    }
  }

  rm(cols_con_lag)
}
########################### DATA DRIFT ########################

if (rankeo){
  col_moneda  <- colnames(dataset)
  col_moneda  <- col_moneda[col_moneda %like% "^(m|Visa_m|Master_m|vm_m)"]

  for( campo in col_moneda)
  {
    cat( campo, " " )
    rankcolumns <- paste("rank", campo, sep=".")
    dataset[ get(campo) ==0, (rankcolumns) := 0 ]
    dataset[ get(campo) > 0, (rankcolumns) :=   frank(  get(campo), ties.method="dense")  / .N, by= foto_mes ]
    dataset[ get(campo) < 0, (rankcolumns) :=  -frank( -get(campo), ties.method="dense")  / .N, by= foto_mes ]
    dataset[ , (campo) := NULL ]
  }
}

# ahora SI comienza la optimizacion Bayesiana

GLOBAL_iteracion <- 0 # inicializo la variable global
GLOBAL_gananciamax <- -1 # inicializo la variable global

# si ya existe el archivo log, traigo hasta donde llegue
if (file.exists(klog)) {
  tabla_log <- fread(klog)
  GLOBAL_iteracion <- nrow(tabla_log)
  GLOBAL_gananciamax <- tabla_log[, max(ganancia)]
}



# paso la clase a binaria que tome valores {0,1}  enteros
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$trainingstrategy$semilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)

# defino los datos que forman parte de validation
#  no hay undersampling
dataset[, validation := 0L]
dataset[ foto_mes %in% PARAM$input$validation,  validation := 1L]

dvalidate <- lgb.Dataset(
  data = data.matrix(dataset[validation == 1L, campos_buenos, with = FALSE]),
  label = dataset[validation == 1L, clase01],
  weight = dataset[validation == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)


# defino los datos de testing
dataset[, testing := 0L]
dataset[ foto_mes %in% PARAM$input$testing,  testing := 1L]


dataset_test <- dataset[testing == 1, ]

# libero espacio
rm(dataset)
gc()

# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando la ganancia
  noisy = TRUE,
  par.set = PARAM$bo_lgb, # definido al comienzo del programa
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600, # se graba cada 600 segundos
  save.file.path = kbayesiana
) # se graba cada 600 segundos

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = PARAM$bo_iteraciones
) # cantidad de iteraciones

# defino el método estandar para la creacion de los puntos iniciales,
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())


# establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# inicio la optimizacion bayesiana
if (!file.exists(kbayesiana)) {
  run <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else {
  run <- mboContinue(kbayesiana) # retomo en caso que ya exista
}


cat("\n\nLa optimizacion Bayesiana ha terminado\n")