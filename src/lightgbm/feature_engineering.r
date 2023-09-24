# Script que devuelve el LAG de n periodos de las columnas seleccionadas

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")


PARAM <- list()
PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(202101, 202102, 202103, 202104, 202105)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

#setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

dataset[1,8:10]

columns <- c("ctrx_quarter", 
"mcaja_ahorro",
"mcuentas_saldo", 
"cpayroll_trx",
"mrentabilidad_annual",
"cliente_edad",
"Visa_msaldopesos",
"mprestamos_personales",
"numero_de_cliente",
"ctarjeta_visa_transacciones",
"cliente_antiguedad",
"Master_fechaalta",
"mpasivos_margen",
"Visa_Fvencimiento",
"mcuenta_corriente",
"Master_Fvencimiento",
"Visa_fechaalta",
"mrentabilidad",
"mactivos_margen",
"Master_mfinanciacion_limite",
"chomebanking_transacciones",
"mpayroll",
"Visa_mfinanciacion_limite",
"mtarjeta_visa_consumo",
"mcomisiones_otras",
"mcomisiones",
"mtransferencias_recibidas",
"mcomisiones_mantenimiento",
"Visa_mlimitecompra",
"Visa_msaldototal",
"Visa_mpagominimo",
"ctarjeta_master",
"Master_mlimitecompra",
"Visa_mpagospesos",
"cproductos",
"Visa_mconsumospesos",
"mautoservicio",
"Master_fultimo_cierre",
"ccomisiones_otras",
"mcaja_ahorro_dolares")

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

setorder(dataset, numero_de_cliente, foto_mes)

periods <- seq(1, 3) # Seleccionar cantidad de periodos 

for (i in periods){
    lagcolumns <- paste("lag", all_columns,i, sep=".")
    dataset[, (lagcolumns):= shift(.SD, type = "lag", fill = NA, n=i), .SDcols = all_columns,  by =numero_de_cliente]
}
# Check de los primeros 3 meses y 100 registros
head(dataset[, c("numero_de_cliente", "ctrx_quarter", "lag.ctrx_quarter.1", "lag.mautoservicio.2")], 100)


##################################################################################################
# Coloca NA a todos los registros en 0

# Antes de los NA

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

dataset[1:3, ..all_columns]

dataset[, (all_columns) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols = all_columns]

#Despues de NA
head(dataset,5)

###################################################################################################
# Ranking en order descendente de cada feature respecto al mes de cada cliente, 
# frente a empate igualo posicion

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

for (col in all_columns){
    rankcolumns <- paste("rank", col, sep=".")
    dataset[, (rankcolumns):= frank(-.SD[[col]], ties.method= "dense"), by = foto_mes]
}

## Check de creacion de rank features

dataset[1:100, c("numero_de_cliente","foto_mes", "ctrx_quarter", "rank.ctrx_quarter")]
setorder(dataset, foto_mes, rank.mcuentas_saldo)
dataset[rank.mcuentas_saldo<=2 & foto_mes==202006, c("numero_de_cliente","foto_mes", "mcuentas_saldo", "rank.mcuentas_saldo")]

all_columns
