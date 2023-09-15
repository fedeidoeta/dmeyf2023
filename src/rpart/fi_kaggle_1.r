# Script para lograr una ganancia en el Public Leaderboard de 70.8161 en Kaggle. 
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rpart")
require("rpart.plot")

# Establezco el Working Directory
setwd("C:/Users/feder/Documents/Maestria_en_Ciencia_de_datos/4_DM_en_Economia_y_Finanzas")
PARAM <- list()

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/KG0001/", showWarnings = FALSE)
setwd("./exp/KG0001")

# seteo semilla
set.seed(270001) #270001, 270029, 270031, 270037, 270059, 270071

# defino la clase_binaria2
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

dtrain <- dataset[foto_mes == 202103]
dapply <- dataset[foto_mes == 202105]
dapply[ , clase_ternaria := NA ]

# Seteo pesos para oversampling
pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ])

#Modelo hiperparametros finales
#La optimizacion Bayesiana de hiperparametros utilizada en rpart
#se encuentra en el archivo fi399_BO_Public_oversampling.r
#https://github.com/fedeidoeta/dmeyf2023/blob/main/src/rpart/fi399_BO_Public_oversampling.r

modelo <- rpart(
    formula = "clase_binaria ~ . - clase_ternaria",
    data = dtrain,
    model = TRUE,
    xval = 0,
    cp = -1,
    minsplit = 820,
    minbucket = 380,
    maxdepth = 9, # parsimonia
    weight = pesos
)
# Prediccion al dataset 202105
prediccion <- predict(modelo, dapply, type = "prob")

# esta es la probabilidad de baja
prob_baja <- prediccion[, "POS"]

tablita <- copy( dapply[, list(numero_de_cliente) ] )
tablita[ , prob := prob_baja ]
setorder( tablita, -prob )

# Seteo corte de los primeros 9500 clientes con mayor prob de baja
PARAM$corte <- 9500

# grabo el submit a Kaggle
tablita[ , Predicted := 0L ]
tablita[ 1:PARAM$corte, Predicted := 1L ]

#Se guarda archivo para enviar a kaggle
fwrite(tablita[ , list(numero_de_cliente, Predicted)], paste0("kaggle_05.csv"), sep = ",")

#Se guarda imagen del arbol resultante 
pdf(file = "./modelo_kaggle_5.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
