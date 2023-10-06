# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
require("randomForest")


PARAM <- list()
PARAM$input$training <- c(202101, 202102, 202103, 202104, 202105)
PARAM$experimento <- "CLU_2"

# Aqui empieza el programa 
setwd("~/buckets/b1") 

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

dataset[1,8:10]

dataset[is.na(dataset), ] <- 0 

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

data_clust <- dataset[clase_ternaria =="BAJA+2" & foto_mes %in% PARAM$input$training]

colnames(data_clust)

rf.fit <- randomForest(x = data_clust[, ..all_columns], y = NULL, ntree = 1000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=7)
data_clust$rf.clusters <- rf.cluster
table(rf.cluster, data_clust$foto_mes)


# creo las carpetas donde van los resultados 
 # creo la carpeta donde va el experimento 
 dir.create("./exp/", showWarnings = FALSE) 
 dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE) 
  
 # Establezco el Working Directory DEL EXPERIMENTO 
 setwd(paste0("./exp/", PARAM$experimento, "/")) 
 fwrite(data_clust[, c("numero_de_cliente", "foto_mes","rf.clusters")],file = paste0(PARAM$experimento, ".csv"),sep = ",")
