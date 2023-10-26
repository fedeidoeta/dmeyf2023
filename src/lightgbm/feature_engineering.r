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
#setwd("C:/Users/feder/Documents/Maestria_en_Ciencia_de_datos/4_DM_en_Economia_y_Finanzas")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

dataset[1,8:10]

columns <- c("ctrx_quarter", 
"mcaja_ahorro",
"mcuentas_saldo", 
#"cpayroll_trx",
#"mrentabilidad_annual",
#"cliente_edad",
#"Visa_msaldopesos",
#"mprestamos_personales",
#"numero_de_cliente",
#"ctarjeta_visa_transacciones",
#"cliente_antiguedad",
#"Master_fechaalta",
#"mpasivos_margen",
#"Visa_Fvencimiento",
#"mcuenta_corriente",
#"Master_Fvencimiento",
#"Visa_fechaalta",
#"mrentabilidad",
#"mactivos_margen",
#"Master_mfinanciacion_limite",
#"chomebanking_transacciones",
#"mpayroll",
#"Visa_mfinanciacion_limite",
#"mtarjeta_visa_consumo",
#"mcomisiones_otras",
#"mcomisiones",
#"mtransferencias_recibidas",
#"mcomisiones_mantenimiento",
#"Visa_mlimitecompra",
#"Visa_msaldototal",
#"Visa_mpagominimo",
#"ctarjeta_master",
#"Master_mlimitecompra",
#"Visa_mpagospesos",
#"cproductos",
#"Visa_mconsumospesos",
#"mautoservicio",
#"Master_fultimo_cierre",
#"ccomisiones_otras",
"mcaja_ahorro_dolares")

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

setorder(dataset, numero_de_cliente, foto_mes)

periods <- seq(1, 3) # Seleccionar cantidad de periodos 

for (i in periods){
    lagcolumns <- paste("lag", columns,i, sep=".")
    dataset[, (lagcolumns):= shift(.SD, type = "lag", fill = NA, n=i), .SDcols = columns,  by =numero_de_cliente]
}

#deltas

for (vcol in columns)
{
  dataset[, paste("delta", vcol,2, sep=".") := get(vcol) - get(paste("lag", vcol,2, sep="."))]
}

head(dataset[, delta.ctrx_quarter.2])




# Check de los primeros 3 meses y 100 registros
head(dataset[, c("numero_de_cliente", "ctrx_quarter", "lag.ctrx_quarter.1", "lag.mautoservicio.2")], 100)


##################################################################################################
# Coloca NA a todos los registros en 0 -- V1 - si hay valores que corresponden realmente a 0, este script los mata

# Antes de los NA

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

dataset[1:3, ..all_columns]

dataset[, (all_columns) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols = all_columns]

#Despues de NA
head(dataset,5)

##################################################################################################
#  Coloca NA en 0 en meses y features selectos -- V2
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

## Check 
head(dataset[foto_mes==201904, mttarjeta_visa_debitos_automaticos],10)


###################################################################################################
# Ranking en order descendente de cada feature respecto al mes de cada cliente, 
# frente a empate igualo posicion

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

col_moneda  <- colnames(dataset)
col_moneda  <- col_moneda[col_moneda %like% "^(m|Visa_m|Master_m|vm_m)"]
for (col in col_moneda){
    rankcolumns <- paste("rank", col, sep=".")
    dataset[, (rankcolumns):= frank(-.SD[[col]], ties.method= "dense"), by = foto_mes]
}

## Check de creacion de rank features

dataset[1:100, c("numero_de_cliente","foto_mes", "ctrx_quarter", "rank.ctrx_quarter")]
setorder(dataset, foto_mes, rank.mcuentas_saldo)
dataset[rank.mcuentas_saldo<=2 & foto_mes==202006, c("numero_de_cliente","foto_mes", "mcuentas_saldo", "rank.mcuentas_saldo")]

all_columns


######################################################################################
# Ranking con 0 vale la pena? Tiene sentido para marcar al arbol donde 
# es la referencia 0 en cada periodo

for (col in columns){
  rankcolumns <- paste("rank", col, sep=".")
  dataset[, (rankcolumns) :=
             ifelse(.SD[[col]] > 0, frank(.SD[[col]], ties.method = "dense"),
                    -frank(-.SD[[col]], ties.method = "dense")), by = foto_mes]

}

#Check
setorder(dataset, numero_de_cliente, foto_mes)
dataset[numero_de_cliente==29202973, c("numero_de_cliente","foto_mes", "mcuentas_saldo", "rank.mcuentas_saldo")]

head(dataset)

######################################################################################
# Ranking con 0 vale la pena? Tiene sentido para marcar al arbol donde --- V2
# es la referencia 0 en cada periodo

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
head(dataset[, rank.mrentabilidad])

colnames(dataset)

#################################################################

#Mising Values
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
#setwd("C:/Users/feder/Documents/Maestria_en_Ciencia_de_datos/4_DM_en_Economia_y_Finanzas")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# ordeno el dataset
setorder(dataset, foto_mes, numero_de_cliente)

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

ratio_df <- data.frame(campo = character(0), ratio = numeric(0))
for (campo in campos_buenos) {
  tbl <- dataset[
    ,
    list("zero_ratio" = sum(get(campo) == 0, na.rm = TRUE) / .N)
  ]
#cat("Campo: ",campo,"Zero ratio: ",tbl$zero_ratio)
#cat("\n")
ratio_df <- rbind(ratio, data.frame(campo = campo, ratio = tbl$zero_ratio))
}

library(ggplot2)

setorder(ratio_df, -ratio)
ratio_ths <- ratio_df[ratio_df$ratio<0.1,]

ratio_ths

ggplot(data= ratio_ths, aes(x= campo, y= ratio))+
  geom_bar(stat = "identity")+
  coord_flip()

nrow(ratio_ths)
