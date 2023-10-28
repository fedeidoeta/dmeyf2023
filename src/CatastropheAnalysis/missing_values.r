#################################################################

#Mising Values
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")


PARAM <- list()
PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

#setwd("~/buckets/b1")
#setwd("C:/Users/feder/Documents/Maestria_en_Ciencia_de_datos/4_DM_en_Economia_y_Finanzas")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# ordeno el dataset
setorder(dataset, foto_mes, numero_de_cliente)

#####################################################################################
# EDA Missing values

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

##################################################################
#  Coloca NA en 0 en meses y features selectos
zero_ratio <- list(
  list(mes = 201901, campo = 
    c("mtransferencias_recibidas","ctransferencias_recibidas")),
  list(mes = 201902, campo = 
    c("mtransferencias_recibidas","ctransferencias_recibidas")),
  list(mes = 201903, campo = 
    c("mtransferencias_recibidas","ctransferencias_recibidas")),
  list(mes = 201904, campo = 
    c("ctarjeta_visa_debitos_automaticos","mttarjeta_visa_debitos_automaticos",
    "mtransferencias_recibidas","ctransferencias_recibidas","ctarjeta_visa_debitos_automaticos")),
  list(mes = 201905, campo = 
    c("mrentabilidad", "mrentabilidad_annual", "mcomisiones","mactivos_margen","mpasivos_margen",
    "ccomisiones_otras","mcomisiones_otras","mtransferencias_recibidas","ctransferencias_recibidas")),
  list(mes = 201910, campo = 
    c("mrentabilidad", "mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen",
    "ccomisiones_otras","mcomisiones_otras","chomebanking_transacciones")),
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
      "tmobile_app","cmobile_app_trx"))
)

for (par in zero_ratio) {
  mes <- par$mes
  feature <- par$campo
  dataset[foto_mes == mes, (feature) := lapply(.SD, function(x) ifelse(x == 0, NA, x)), .SDcols = feature]
}

## Check 
head(dataset[foto_mes<=201910 & numero_de_cliente == 29183981, c("numero_de_cliente","foto_mes","mcomisiones_otras")],10)


imputar_media <- function(campo, meses){

    for (mes in meses) {
        tbl <- dataset[ foto_mes < mes, get(campo), by = numero_de_cliente]
        tbl <- tbl[, round(mean(V1, na.rm = TRUE), digits = 2), by = numero_de_cliente]
        dataset <- dataset[tbl, on= .(numero_de_cliente)]
        dataset[foto_mes == mes, paste0(campo) := ifelse(get(campo) == 0, V1, get(campo))]
        dataset [, V1 := NULL]
    }
    return(dataset)
}

dataset <- imputar_media("mcomisiones_otras", c(201905,201910))




imputar_mediana <- function(campo, meses){

    for (mes in meses) {
        tbl <- dataset[ foto_mes < mes, get(campo), by = numero_de_cliente]
        tbl <- tbl[, round(median(V1, na.rm = TRUE), digits = 2), by = numero_de_cliente]
        dataset <- dataset[tbl, on= .(numero_de_cliente)]
        dataset[foto_mes == mes, paste0(campo) := ifelse(get(campo) == 0, V1, get(campo))]
        dataset [, V1 := NULL]
    }
    return(dataset)
}

dataset <- imputar_mediana("mcomisiones_otras", c(201905,201910))