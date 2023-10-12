############### Impossible is Nothing ###################
# Optimizacion Bayesiana de hiperparametros de  rpart
# que va directamente contra el Public Leaderboard

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

require("parallel")

# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


# Defino la  Optimizacion Bayesiana
PARAM <- list()
PARAM$experimento <- "RP001"

# cantidad de iteraciones de la Optimizacion Bayesiana
PARAM$BO_iter <- 34 # iteraciones inteligentes

#  de los hiperparametros
PARAM$hs <- makeParamSet(
  makeIntegerParam("minsplit", lower = 500L, upper = 1500L), #antes lower = 500L, upper = 1500L
  makeIntegerParam("minbucket", lower = 200L, upper = 1000L),# antes lower = 200L, upper = 800L
  makeIntegerParam("maxdepth", lower = 6L, upper = 20L), # paso lower 6 --> 9, upper: 12--> 10
  makeIntegerParam("corte", lower = 8000L, upper = 15000L), # lower: 8000-->9000
  forbidden = quote(minbucket > 0.5 * minsplit)
)
# minbuket NO PUEDE ser mayor que la mitad de minsplit

PARAM$semilla_azar <- 270001 # primer semilla

#------------------------------------------------------------------------------

leer_numero <- function( mensaje ) {
  res <- readline( mensaje )
  while( is.na( as.numeric( res ))) {
    cat( "Debe introducir un numero, el separador decimal es la coma\n" )
    res <- readline( mensaje )
  }

  return( as.numeric(res) )
}
#------------------------------------------------------------------------------

leer_verificado <- function( mensaje ) {
  repeat {
  
    n1 <- leer_numero( mensaje )
    cat( "Por favor, vuelva a cargar el mismo numero\n" )
    n2 <- leer_numero( mensaje )

   if( n1 != n2 )  cat( "Los numeros no coinciden, debe volver a cargar\n\n" )
   if( n1== n2 ) break
  }

  return( n1 )
}
#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(reg, arch = NA, folder = "./work/", ext = ".txt",
                    verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  # Escribo los titulos
  if (!file.exists(archivo)) {
    linea <- paste0(
      "fecha\t",
      paste(list.names(reg), collapse = "\t"), "\n"
    )

    cat(linea, file = archivo)
  }

  # la fecha y hora
  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t",
    gsub(", ", "\t", toString(reg)), "\n"
  )

  # grabo al archivo
  cat(linea, file = archivo, append = TRUE)

  # imprimo por pantalla
  if (verbose) cat(linea)
}
#----------------------------------------------------------------------------
# param tiene los hiperparametros del arbol

ArbolSimple <- function( data, param, iteracion) {

  param2 <- copy( param )
  param2$cp <- -1
  param2$minsplit <- param$minsplit 
  param2$minbucket <- param$minbucket
  param2$corte <- param$corte

  modelo <- rpart("clase_binaria ~ . - clase_ternaria",
    data = dtrain,
    xval = 0,
    control = param2,
    weights = pesos
  )

  # aplico el modelo a los datos de testing
  # aplico el modelo sobre los datos de testing
  # quiero que me devuelva probabilidades
  prediccion <- predict(modelo,
    dapply,
    type = "prob"
  )

  # esta es la probabilidad de baja
  prob_baja <- prediccion[, "POS"]

  tablita <- copy( dapply[, list(numero_de_cliente) ] )
  tablita[ , prob := prob_baja ]
  setorder( tablita, -prob )

  # grabo el submit a Kaggle
  tablita[ , Predicted := 0L ]
  tablita[ 1:param2$corte, Predicted := 1L ]

  nom_submit <- paste0("RP001", sprintf( "%03d", iteracion ), ".csv" )
  fwrite( tablita[ , list(numero_de_cliente, Predicted)],
          file= nom_submit,
          sep= "," )

  # solicito que el humano a cargo ingrese la ganancia publica
  mensaje <- paste0( "haga el submit a Kaggle de ", nom_submit,
                     " y cargue la ganancia publica : " )

  ganancia_public <- leer_verificado( mensaje )

  return(ganancia_public)
}
#------------------------------------------------------------------------------
# esta funcion solo puede recibir los parametros que se estan optimizando
# el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia <- function(x) {
  GLOBAL_iteracion <<- GLOBAL_iteracion + 1

  # x los hiperparametros del arbol
  ganancia_public <- ArbolSimple( dtrain, x, GLOBAL_iteracion )

  # logueo
  xx <- x
  xx$cp <- -1
  xx$ganancia <- ganancia_public
  xx$iteracion <- GLOBAL_iteracion
  loguear(xx, arch = archivo_log)

  # para que mlrMBO tenga todo reseteado
  set.seed( PARAM$semilla_azar )

  return(ganancia_public)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Establezco el Working Directory
setwd("C:/Users/feder/Documents/Maestria_en_Ciencia_de_datos/4_DM_en_Economia_y_Finanzas")

# cargo los datos
PARAM <- list()
PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

PARAM$input$training <- c(201907, 201908, 201909, 201910, 201911, 
                          201912, 202011, 202012, 202101, 202102, 
                          202103, 202104, 202105)

PARAM$input$future <- c(202107)


dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

#______________________________________________________
# Feature engineering


#_______________________________________________
# FI: Coloco NA a todos los registos en 0
zero_ratio <- list(
  list(mes = 202006, campo = 
    c("active_quarter", "internet", "mrentabilidad", "mrentabilidad_annual", 
      "mcomisiones", "mactivos_margen", "mpasivos_margen", "mcuentas_saldo", 
      "ctarjeta_debito_transacciones","mautoservicio", "ctarjeta_visa_transacciones", 
      "mtarjeta_visa_consumo","ctarjeta_master_transacciones", "mtarjeta_master_consumo",
      "ccomisiones_otras", "mcomisiones_otras","cextraccion_autoservicio","mextraccion_autoservicio",
      "ccheques_depositados","mcheques_depositados","ccheques_emitidos","mcheques_emitidos",
      "ccheques_depositados_rechazados","mcheques_depositados_rechazados","ccheques_emitidos_rechazados",
      "mcheques_emitidos_rechazados","tcallcenter","ccallcenter_transacciones","thomebanking",
      "chomebanking_transacciones","ccajas_transacciones","ccajas_consultas","ccajas_depositos",
      "ccajas_extracciones","ccajas_otras","catm_trx","matm","catm_trx_other","matm_other",
      "tmobile_app","cmobile_app_trx")),
  list(mes = 201910, campo = 
    c("mrentabilidad", "mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen",
    "ccomisiones_otras","mcomisiones_otras","chomebanking_transacciones")),
  list(mes = 201905, campo = 
    c("mrentabilidad", "mrentabilidad_annual", "mcomisiones","mactivos_margen","mpasivos_margen",
    "ccomisiones_otras","mcomisiones_otras")),
  list(mes = 201904, campo = 
    c("ctarjeta_visa_debitos_automaticos","mttarjeta_visa_debitos_automaticos"))
)

for (par in zero_ratio) {
  mes <- par$mes
  feature <- par$campo
  dataset[foto_mes == mes, (feature) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols = feature]
}

#______________________________________________________________
# FI: hago lag de los ultimos 6 meses de todas las features (menos numero cliente, foto mes y clase ternaria)

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

setorder(dataset, numero_de_cliente, foto_mes)

periods <- seq(1, 6) # Seleccionar cantidad de periodos 

for (i in periods){
    lagcolumns <- paste("lag", all_columns,i, sep=".")
    dataset[, (lagcolumns):= shift(.SD, type = "lag", fill = NA, n=i), .SDcols = all_columns,  by =numero_de_cliente]
}

# Delta LAG de 1 y 2 periodos

for (vcol in all_columns){
  dataset[, paste("delta", vcol,1, sep=".") := get(vcol) - get(paste("lag", vcol,1, sep="."))]
}

for (vcol in all_columns){
  dataset[, paste("delta", vcol,2, sep=".") := get(vcol) - get(paste("lag", vcol,2, sep="."))]
}


#________________________________________________
# FI: Ranking de cada cliente de cada mes en todas las features con 0 fijo - V2

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

# Fin FE
#--------------------------------------

# defino la clase_binaria2
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

dtrain <- dataset[foto_mes %in% PARAM$input$training]
dapply <- dataset[foto_mes == PARAM$input$future]

#dapply[ , clase_ternaria := NA ]


# definicion vector de pesos para oversampling
pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ] )

# creo la carpeta donde va el experimento
#  HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create( paste0("./exp/", PARAM$experimento, "/"), 
           showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd( paste0("./exp/", PARAM$experimento, "/") )


# en estos archivos quedan los resultados
archivo_log <- "BO_log.txt"
archivo_BO <- "bayesian.RDATA"

# leo si ya existe el log
#  para retomar en caso que se se corte el programa
GLOBAL_iteracion <- 0

if (file.exists(archivo_log)) {
  tabla_log <- fread(archivo_log)
  GLOBAL_iteracion <- nrow(tabla_log)
}

# Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar <- EstimarGanancia

configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,
#  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
# minimize= FALSE estoy Maximizando la ganancia
obj.fun <- makeSingleObjectiveFunction(
  fn = funcion_optimizar,
  minimize = FALSE,
  noisy = TRUE,
  par.set = PARAM$hs,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl(
  save.on.disk.at.time = 600,
  save.file.path = archivo_BO
)

ctrl <- setMBOControlTermination(ctrl, iters = PARAM$BO_iter)
ctrl <- setMBOControlInfill(ctrl,  crit = makeMBOInfillCritEI())

surr.km <- makeLearner("regr.km",
  predict.type = "se",
  covtype = "matern3_2", control = list(trace = TRUE)
)


# para que mlrMBO tenga todo reseteado
set.seed( PARAM$semilla_azar )

# inicio la optimizacion bayesiana
if (!file.exists(archivo_BO)) {
  run <- mbo(
    fun = obj.fun,
    learner = surr.km,
    control = ctrl
  )
} else {
  run <- mboContinue(archivo_BO)
}
# retomo en caso que ya exista

###########################################################