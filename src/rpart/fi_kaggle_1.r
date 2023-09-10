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

# uso esta semilla para los canaritos
set.seed(270001)
# defino la clase_binaria2
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

dtrain <- dataset[foto_mes == 202103]
dapply <- dataset[foto_mes == 202105]
dapply[ , clase_ternaria := NA ]

# Seteo pesos para oversampling
pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ])

#Modelo con hiperparametros optimizados
modelo <- rpart(
    formula = "clase_binaria ~ . - clase_ternaria",
    data = dtrain,
    model = TRUE,
    xval = 0,
    cp = -1,
    minsplit = 821, # dejo que crezca y corte todo lo que quiera
    minbucket = 376,
    maxdepth = 10,
    weight = pesos
)


prediccion <- predict(modelo, dapply, type = "prob")

# esta es la probabilidad de baja
prob_baja <- prediccion[, "POS"]

tablita <- copy( dapply[, list(numero_de_cliente) ] )
tablita[ , prob := prob_baja ]
setorder( tablita, -prob )

PARAM$corte <- 9525

# grabo el submit a Kaggle
tablita[ , Predicted := 0L ]
tablita[ 1:PARAM$corte, Predicted := 1L ]

fwrite(tablita[ , list(numero_de_cliente, Predicted)], paste0("kaggle_01.csv"), sep = ",")

pdf(file = "./modelo_kaggle_1.pdf", width=28, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
