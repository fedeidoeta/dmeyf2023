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

head(dataset[,8:10], 1)

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
unique(dataset$foto_mes)
setorder(dataset, numero_de_cliente, foto_mes)

periods <- seq(1, 3) # Seleccionar cantidad de periodos 

for (i in periods){
    lagcolumns <- paste("lag", all_columns,i, sep=".")
    dataset[, (lagcolumns):= shift(.SD, type = "lag", fill = NA, n=i), .SDcols = all_columns,  by =numero_de_cliente]
}
# Check de los primeros 3 meses
head(dataset[, c("numero_de_cliente", "ctrx_quarter", "lag.ctrx_quarter.1", "lag.mautoservicio.2")], 100)
