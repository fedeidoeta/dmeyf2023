# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

setwd("C:/Users/fidoeta/Documents/VS Code/Maestria") # establezco la carpeta donde voy a trabajar

PARAM <- list()

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/EA4871/", showWarnings = FALSE)
setwd("./exp/EA4871")

# uso esta semilla para los canaritos
set.seed(270001)

# agrego canaritos randomizados
dataset2 <- copy(dataset)
# quito algunas variables de dataset2
dataset2[ , numero_de_cliente := NULL ]
dataset2[ , clase_ternaria := NULL ]
dataset2[ , foto_mes := NULL ]

# agrego azar
dataset2[ , azar := runif( nrow(dataset2) ) ]
# randomizo, manteniendo las relaciones entre las variables
setorder( dataset2, azar )
dataset2[ , azar := NULL ]  # borra azar

columnas <- copy(colnames(dataset2))

# creo efectivamente los canaritos
#  1/5  de las variables del dataset
for( i in sample( 1:ncol(dataset2) , round( ncol(dataset)/5 ) )  )
{
  dataset[, paste0("canarito", i) :=  dataset2[ , get(columnas[i]) ]  ]
}
# defino la clase_binaria2
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

dtrain <- dataset[foto_mes == 202103]
dapply <- dataset[foto_mes == 202105]
dapply[ , clase_ternaria := NA ]

# Seteo pesos para oversampling
pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ])

#dtrain[, pesos := ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ]

# Dejo crecer el arbol sin ninguna limitacion
# sin limite de altura ( 30 es el maximo que permite rpart )
# sin limite de minsplit ( 2 es el minimo natural )
# sin limite de minbukcet( 1 es el minimo natural )
# los canaritos me protegeran
modelo_original <- rpart(
    formula = "clase_binaria ~ . - clase_ternaria",
    data = dtrain,
    model = TRUE,
    xval = 0,
    cp = -1,
    minsplit = 2, # dejo que crezca y corte todo lo que quiera
    minbucket = 1,
    maxdepth = 30,
    weight = pesos
)
head(modelo_original$frame, 10)

# hago el pruning de los canaritos
# haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[
    modelo_original$frame$var %like% "canarito",
    "complexity"
] <- -666

modelo_pruned <- prune(modelo_original, -666)

prediccion <- predict(modelo_pruned, dapply, type = "prob")

# esta es la probabilidad de baja
prob_baja <- prediccion[, "POS"]

tablita <- copy( dapply[, list(numero_de_cliente) ] )
tablita[ , prob := prob_baja ]
setorder( tablita, -prob )

PARAM$corte <- 9500

# grabo el submit a Kaggle
tablita[ , Predicted := 0L ]
tablita[ 1:PARAM$corte, Predicted := 1L ]

fwrite(tablita[ , list(numero_de_cliente, Predicted)], paste0("canaritos_9500_pesos.csv"), sep = ",")

pdf(file = "./modelo_pruned_pesos.pdf", width=28, height=4)
prp(modelo_pruned, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()