# para correr el Google Cloud
#   8 vCPU
#  256 GB memoria RAM

#fe2004-rankeo:
# + PARAM$input$training <- c(201906, 201907, 201908, 201909, 
#                          201910, 201911, 201912, 202011, 
#                          202012, 202101, 202102, 202103, 
#                          202104, 202105, 202106, 202107)

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("primes")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "FE2004_rankeo"

PARAM$input$dataset <- "./datasets/competencia_03_V2.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(201906, 201907, 201908, 201909, 
                          201910, 201911, 201912, 202011, 
                          202012, 202101, 202102, 202103, 
                          202104, 202105, 202106, 202107)

PARAM$input$future <- c(202109) # meses donde se aplica el modelo

# Experimentos

missing_values <- FALSE
media_lag_delta <- FALSE
rankeo <- TRUE


# hiperparametros intencionalmente NO optimos
PARAM$finalmodel$optim$num_iterations <- 694
PARAM$finalmodel$optim$learning_rate <- 0.163389737714256
PARAM$finalmodel$optim$feature_fraction <- 0.787660383245211
PARAM$finalmodel$optim$min_data_in_leaf <- 33256
PARAM$finalmodel$optim$num_leaves <- 161

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


#______________________________________________________
# Feature engineering


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
  }
}

# Fin FE
#--------------------------------------


# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------

# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/ExpColaborativo/", showWarnings = FALSE)
dir.create(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"))


# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


PARAM$finalmodel$semilla <- 270029

#--------------------------------------------------
#Genero semillas

set.seed( 270001 )   #dejo fija esta semilla
cant_semillas  <- 100

#me genero un vector de semilla buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #funcion que genera primos
semillas  <- sample(primos)[ 1:cant_semillas ]   #tomo una muestra de numeros primos al azar
semillas  <- c( 270001, semillas )

#------------------------------------------------------------------------------
# Inicializo variables
control = 0
c = 0

tb_final <- data.table(numero_de_cliente = character(), foto_mes = numeric(), prob_acum = numeric())

#------------------------------------------------------------------------------
#Comienza semillerio

for (semilla_i in semillas) {

  PARAM$finalmodel$semilla <- semilla_i


  # Hiperparametros FIJOS de  lightgbm
  PARAM$finalmodel$lgb_basicos <- list(
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

    bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
    pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
    neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
    is_unbalance = FALSE, #
    scale_pos_weight = 1.0, # scale_pos_weight > 0.0

    drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
    max_drop = 50, # <=0 means no limit
    skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

    extra_trees = TRUE, # Magic Sauce

    seed = PARAM$finalmodel$semilla
  )

  # genero el modelo
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
    PARAM$finalmodel$optim)

  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )

  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
    archivo_importancia <- paste0("impo_",PARAM$experimento, "_", semilla_i, ".txt")

  fwrite(tb_importancia,
    file = archivo_importancia,
    sep = "\t"
  )

  #--------------------------------------


  # aplico el modelo a los datos sin clase
  dapply <- dataset[foto_mes == PARAM$input$future]

  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )

  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]

  # grabo las probabilidad del modelo
  fwrite(tb_entrega,
          file = paste0("prediccion_",semilla_i,".txt"),
          sep = "\t"
  )
  if (control == 0)
  {
      tb_final <- tb_entrega[, list(numero_de_cliente, prob_acum = 0L)]
      control = 1 
  }
   if (control == 1)
   {
      tb_final <- tb_final[tb_entrega, on = c("numero_de_cliente"), nomatch = 0]
      tb_final[, prob_acum := rowSums(cbind(prob_acum, prob), na.rm = TRUE)]
      c <- c+1
   }
   if ((c %% 10)==0)
   {
      fwrite(tb_final,
        file = paste0("prediccion_acum_",c,".txt"),
        sep = "\t" ) # Guardo los resultados

      tb_entrega_parcial <- tb_final
      setorder(tb_entrega_parcial, -prob_acum)
      cortes <- seq(8000, 13000, by = 500)
      for (envios in cortes) {
        tb_entrega_parcial[, Predicted := 0L]
        tb_entrega_parcial[1:envios, Predicted := 1L]

        fwrite(tb_entrega_parcial[, list(numero_de_cliente, Predicted)],
          file = paste0(c,"_entrega_parcial_",PARAM$experimento, "_", envios, ".csv"),
          sep = ","
        )
      }
   }
  
  tb_final[, foto_mes := NULL]
  tb_final[, prob := NULL]
  rm(tb_entrega, tb_entrega_parcial)
}

tb_entrega <- tb_final

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob_acum)


# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]

  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
    file = paste0("0_",PARAM$experimento, "_", envios, ".csv"),
    sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
