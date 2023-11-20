# para correr el Google Cloud
#   8 vCPU
#  128 GB memoria RAM

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("primes")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "final_baseline_sem"

PARAM$input$dataset <- "./datasets/competencia_03_V2.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(202010, 202011, 202012, 202101, 202102, 202103, 202104,202105)

PARAM$input$future <- c(202107) # meses donde se aplica el modelo

# hiperparametros intencionalmente NO optimos
PARAM$finalmodel$optim$num_iterations <- 1222
PARAM$finalmodel$optim$learning_rate <- 0.02002008
PARAM$finalmodel$optim$feature_fraction <- 0.91207577
PARAM$finalmodel$optim$min_data_in_leaf <- 10701
PARAM$finalmodel$optim$num_leaves <- 384

#--------------------------------------------------
#Genero semillas

set.seed( 270001 )   #dejo fija esta semilla
cant_semillas  <- 1

#me genero un vector de semilla buscando numeros primos al azar
primos  <- generate_primes(min=100000, max=1000000)  #funcion que genera primos
semillas  <- sample(primos)[ 1:cant_semillas ]   #tomo una muestra de numeros primos al azar
semillas  <- c( 119839, semillas )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
truth <- dataset[foto_mes == PARAM$input$future,c("numero_de_cliente","clase_ternaria")]


####################### MISSING VALUES ##############################

# Baseline


############################ FIN MISSING VALUES #####################


# agrego lag1, lag3 y lag6

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

setorder(dataset, numero_de_cliente, foto_mes)

periods <- c(1, 3, 6)  # Seleccionar cantidad de periodos 

for (i in periods){
    lagcolumns <- paste("lag", all_columns,i, sep=".")
    dataset[, (lagcolumns):= shift(.SD, type = "lag", fill = NA, n=i), .SDcols = all_columns,  by =numero_de_cliente]
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

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/ExpColaborativo/", showWarnings = FALSE)
dir.create(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"))

ganancias <- tibble::tribble(~semilla,~ganancia,~envios)


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

  # ordeno por probabilidad descendente
  setorder(tb_entrega, -prob)


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
      file = paste0(PARAM$experimento, "_",semilla_i,"_", envios, ".csv"),
      sep = ","
    )
      tb_ganancias <- tb_entrega[truth, on = c("numero_de_cliente"), nomatch = 0]
      tb_ganancias <- tb_ganancias[Predicted == 1,]
      tb_ganancias[,gan := fifelse(clase_ternaria == "BAJA+2",273000,-7000)]
      
      ganancia <- tibble::tribble(~semilla,~ganancia,~envios,
                                  semilla_i, sum(tb_ganancias$gan),envios)
      
      ganancias <- rbind(ganancias,ganancia)
    
  }
  print(paste0("Iteracion ",semilla_i, " finalizada"))
}


write.csv(ganancias,
       file = paste0("0_",PARAM$experimento,"_ganancias_semillerio.csv"),
       sep = ","
)

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
