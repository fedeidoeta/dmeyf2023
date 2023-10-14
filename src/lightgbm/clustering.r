# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
require("randomForest")


PARAM <- list()
PARAM$input$training <- c(202101, 202102, 202103, 202104, 202105)
PARAM$experimento <- "CLU_2_3"

# Aqui empieza el programa 
setwd("~/buckets/b1") 

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# Imputo con 0 a todos los NA
dataset[is.na(dataset), ] <- 0 ## Revisar si conviene colocar en 0 todos los NA

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

data_clust <- dataset[clase_ternaria =="BAJA+2" & foto_mes %in% PARAM$input$training]

colnames(data_clust)

rf.fit <- randomForest(x = data_clust[, ..all_columns], y = NULL, ntree = 1000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=5)
data_clust$rf.clusters <- rf.cluster
table(rf.cluster, data_clust$foto_mes)

feature_importance <- importance(rf.fit)

feature_names <- rownames(feature_importance)

# Agregar una columna con los nombres de las características
feature_importance <- cbind("Feature" = feature_names, feature_importance)


# creo las carpetas donde van los resultados 
# creo la carpeta donde va el experimento 
dir.create("./exp/", showWarnings = FALSE) 
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE) 

# Establezco el Working Directory DEL EXPERIMENTO 
setwd(paste0("./exp/", PARAM$experimento, "/")) 

write.table(as.data.frame(feature_importance), file = paste0("feature_importance.txt"), sep = "\t", col.names = TRUE, row.names = FALSE)

fwrite(data_clust[, c("numero_de_cliente", "foto_mes","rf.clusters")],file = paste0(PARAM$experimento, ".csv"),sep = ",")


#########################################################################
# Dendrograma

install.packages("ggdendro")

pdf("dendograma_test.pdf")

library(ggdendro)
dendrogram <- hclust(as.dist(1-rf.fit$proximity), method = 'ward.D')
ggdendrogram(dendrogram, rotate = FALSE, labels = FALSE, theme_dendro = TRUE)
dev.off()


##############################################################
#Utilizo Kmeans para hallar el k optimo

set.seed(270001)
wcss <- vector()
for(i in 1:20){
  wcss[i] <- sum(kmeans(as.dist(1-rf.fit$proximity), i)$withinss)
}

library(ggplot2)

pdf("codo.pdf")
ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

dev.off()


###########################################################
# Analisis de los resultados


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")

PARAM <- list()

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"
PARAM$input$cluster <- "./datasets/CLU_2_3.csv"
PARAM$experimento <- "CLU_hist_2"
PARAM$archivo <- "CLU_hist_2_mean"


#setwd("./datasets/")

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
clu <- fread(PARAM$input$cluster, stringsAsFactors = TRUE)

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# Join dataset con clusters
data_clust_hist <- dataset[clu, on = "numero_de_cliente"]

#Selecciono periodos para analizar la media
periodos <- c(202101, 202102, 202103, 202104, 202105)

#Selecciono columnas
all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

#Resultados cluster con historia
data_clust_hist_select <- data_clust_hist[foto_mes %in% periodos]
data_clust_hist_select[is.na(data_clust_hist_select), ] <- 0
resultados <- data_clust_hist_select[, lapply(.SD, mean), by = rf.clusters, .SDcols = all_columns]


#Resultados continua
resultados_cont <- dataset[foto_mes %in% periodos]
resultados_cont[is.na(resultados_cont), ] <- 0
resultados_cont <- resultados_cont[, lapply(.SD, mean), .SDcols = all_columns]

resultados_cont <- resultados_cont[, rf.clusters:= 99]

combined_resultado <- rbind(resultados, resultados_cont)


fwrite(resultados,file = paste0(PARAM$archivo, ".csv"),sep = ",")

fwrite(data_clust_hist,file = paste0("data_clust_hist_2021.csv"),sep = ",")

pdf( paste0("snap_mean_cont", ".pdf"))

for (campo in all_columns){

text(
  x = barplot(combined_resultado[[campo]],
        main = campo,
        col = viridis::viridis(6),
        names.arg = combined_resultado$rf.clusters),
  y = combined_resultado[[campo]],  # Ajusta la posición vertical de las etiquetas
    labels = round(combined_resultado[[campo]],3),  # Etiquetas con los valores de Y
    pos = 1,  # Posición de las etiquetas (3 = arriba)
    col = c("white","white","white","black","black","black","black"),  # Color del texto
    cex = 0.8  # Tamaño del texto
)
}
dev.off()
#####################################################

# Boxplot

campo <- "mrentabilidad"

all_columns <- setdiff(
  colnames(data_clust_hist),
  c("numero_de_cliente", "foto_mes", "clase_ternaria","i.foto_mes","rf.clusters")
)
#options(ggplot2.width = 8, ggplot2.height =5) 

#campo <- "ctrx_quarter"

pdf( paste0("boxplot_2_3", ".pdf"))

for (campo in all_columns) {

p <- ggplot(data = data_clust_hist_select, aes( x = as.factor(rf.clusters), y = !!sym(campo), fill = as.factor(rf.clusters)))+
  geom_boxplot()

print(p)
}

dev.off()



#######################################################################
## Graficos de todo la historia del dataset
#
##library(ggplot2)
#
#
### Relativizo 
#data_clust_hist[, rank_foto_mes := foto_mes - max(foto_mes), by = numero_de_cliente]
#data_clust_hist[, rank_foto_mes_2 := -rank(rank_foto_mes, ties.method = "min")+1, by = numero_de_cliente]
#
#dataset[, rank_foto_mes := foto_mes - max(foto_mes), by = numero_de_cliente]
#dataset[, rank_foto_mes_2 := -rank(rank_foto_mes, ties.method = "min")+1, by = numero_de_cliente]
#
#
#all_columns <- setdiff(
#  colnames(data_clust_hist),
#  c("numero_de_cliente", "foto_mes", "clase_ternaria","i.foto_mes","rf.clusters")
#)
#
#columns <- c("ctrx_quarter", 
#"mcaja_ahorro",
#"mcuentas_saldo")
#
##campo <- "ctrx_quarter"
#pdf( paste0("mean_continua_2", ".pdf"))
#
#for (campo in all_columns) {
#
#tbl <- data_clust_hist[
#    ,
#    list("mean" = sum(get(campo), na.rm = TRUE) / .N),
#    by= .(rf.clusters,rank_foto_mes_2)
#  ]
#
#tbl_sano <- dataset[
#    ,
#    list("mean" = sum(get(campo), na.rm = TRUE) / .N),
#    by= rank_foto_mes_2
#  ]
#tbl_sano <- tbl_sano[, rf.clusters:= 99]
#
#combined_tbl <- rbind(tbl, tbl_sano)
#
#
## Excluye los ultimos dos periodos
##combined_tbl <- subset(combined_tbl, !(foto_mes %in% c(202107, 202106)))
#
## Crear un gráfico de líneas múltiples
#p <- ggplot(data = combined_tbl, aes(x = as.factor(rank_foto_mes_2), y = mean, group = rf.clusters, color = as.factor(rf.clusters))) +
#  geom_line() +
#  labs(
#    title = paste0("Media de ", campo, " por Cluster"),
#    x = "Periodo",
#    y = "Media"
#  ) +
#  theme_minimal() +
#  scale_x_discrete(labels = tbl$rank_foto_mes_2) +
#  theme(text = element_text(size = 7),
#
#        axis.text.x = element_text(size = 5),
#        legend.text = element_text(size = 5))
#
#print(p)
#}
#
#dev.off()
#
###################################################################

dataset[numero_de_cliente==67783300]

#Graficos con mes relativos


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
library("ggplot2")

PARAM <- list()

PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"
PARAM$input$cluster <- "./datasets/CLU_2_3.csv"
PARAM$experimento <- "CLU_hist_2_3"
PARAM$archivo <- "CLU_hist_2_mean"

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
clu <- fread(PARAM$input$cluster, stringsAsFactors = TRUE)

dataset <- subset(dataset, !(foto_mes %in% c(202107, 202106)))

setorder(dataset, numero_de_cliente, foto_mes)
dataset[, rank_foto_mes := rank(foto_mes, ties.method = "min"), by = numero_de_cliente]
max_rank_foto_mes <- dataset[, max(rank_foto_mes), by = numero_de_cliente]
setnames(max_rank_foto_mes, "V1", "max_rank_foto_mes")
dataset <- merge(dataset, max_rank_foto_mes, by = "numero_de_cliente")
dataset[, mes_relativo := rank_foto_mes - max_rank_foto_mes]

data_clust_hist <- dataset[clu, on = "numero_de_cliente"]

# Grafico

all_columns <- setdiff(
  colnames(data_clust_hist),
  c("numero_de_cliente", "foto_mes", "clase_ternaria","i.foto_mes","rf.clusters")
)
#options(ggplot2.width = 8, ggplot2.height =5) 

#campo <- "ctrx_quarter"

pdf( paste0("mean_continua_4", ".pdf"), width = 12, height = 6)

for (campo in all_columns) {

tbl <- data_clust_hist[
    ,
    list("mean" = sum(get(campo), na.rm = TRUE) / .N),
    by = .(rf.clusters, mes_relativo)
]

p <- ggplot(tbl, aes(mes_relativo, mean, colour = as.factor(rf.clusters), 
            group = rf.clusters, shape = as.factor(rf.clusters))) +
  geom_smooth(se = TRUE) +
  stat_smooth(level = 0.95) +
  labs(
    x = 'Mes a la baja',
    y = campo,
    title = paste0("Media de ", campo, " por Cluster")
  ) +
  theme_minimal() +
  scale_alpha(guide = "none")

  print(p)
}

dev.off()

