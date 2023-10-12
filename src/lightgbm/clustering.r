# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("rlist")
require("randomForest")


PARAM <- list()
PARAM$input$training <- c(202101, 202102, 202103, 202104, 202105)
PARAM$experimento <- "CLU_2_2"

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
PARAM$input$cluster <- "./datasets/CLU_2.csv"
PARAM$experimento <- "CLU_hist_2"
PARAM$archivo <- "CLU_hist_2_mean"


#setwd("./datasets/")

dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)
clu <- fread(PARAM$input$cluster, stringsAsFactors = TRUE)

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

dataset[1,8:10]

data_clust_hist <- dataset[clu, on = "numero_de_cliente"]

#Selecciono periodos para analizar la media

periodos <- c(202101, 202102, 202103, 202104, 202105)

data_clust_hist_select <- data_clust_hist[foto_mes %in% periodos]

#columnas_numericas <- colnames(data_clust_hist_select)[sapply(data_clust_hist_select, is.numeric)]

all_columns <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

data_clust_hist_select[is.na(data_clust_hist_select), ] <- 0

resultados <- data_clust_hist_select[, lapply(.SD, mean), by = rf.clusters, .SDcols = all_columns]

fwrite(resultados,file = paste0(PARAM$archivo, ".csv"),sep = ",")

fwrite(data_clust_hist,file = paste0("data_clust_hist_2021.csv"),sep = ",")

pdf( paste0("snap_mean", ".pdf"))

for (campo in all_columns){

text(
  x = barplot(resultados[[campo]],
        main = campo,
        col = viridis::viridis(5),
        names.arg = resultados$rf.clusters),
  y = resultados[[campo]],  # Ajusta la posición vertical de las etiquetas
    labels = round(resultados[[campo]],3),  # Etiquetas con los valores de Y
    pos = 1,  # Posición de las etiquetas (3 = arriba)
    col = c("white","white","white","black","black","black","black"),  # Color del texto
    cex = 0.8  # Tamaño del texto
)
}
dev.off()
#######################################################################
# Graficos de todo la historia del dataset

#library(ggplot2)

all_columns <- setdiff(
  colnames(data_clust_hist),
  c("numero_de_cliente", "foto_mes", "clase_ternaria","i.foto_mes","rf.clusters")
)

columns <- c("ctrx_quarter", 
"mcaja_ahorro",
"mcuentas_saldo")

#campo <- "ctrx_quarter"
pdf( paste0("mean", ".pdf"))

for (campo in all_columns) {

tbl <- data_clust_hist[
    ,
    list("mean" = sum(get(campo), na.rm = TRUE) / .N),
    by= .(rf.clusters,foto_mes)
  ]

# Excluye los ultimos dos periodos
tbl <- subset(tbl, !(foto_mes %in% c(202107, 202106)))

# Crear un gráfico de líneas múltiples
p <- ggplot(data = tbl, aes(x = as.factor(foto_mes), y = mean, group = rf.clusters, color = as.factor(rf.clusters))) +
  geom_line() +
  labs(
    title = paste0("Media de ", campo, " por Cluster"),
    x = "Periodo",
    y = "Media"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = tbl$foto_mes) +
  theme(text = element_text(size = 7),

        axis.text.x = element_text(size = 5),
        legend.text = element_text(size = 5))

print(p)
}

dev.off()

############################################################################
######################## NO SIRVE ##########################################
# Graficos de un periodo seleccionado

#library(ggplot2)

all_columns <- setdiff(
  colnames(data_clust_hist),
  c("numero_de_cliente", "foto_mes", "clase_ternaria","i.foto_mes","rf.clusters")
)

# Selecciono periodos a graficar
periodos <- c(202101, 202102, 202103, 202104, 202105)

pdf( paste0("mean_periodos", ".pdf"))

for (campo in all_columns) {

tbl <- data_clust_hist[
    ,
    list("mean" = sum(get(campo), na.rm = TRUE) / .N),
    by= .(rf.clusters,foto_mes)
  ]

#Filtra periodos seleccionados
tbl_2 <- tbl[foto_mes %in% periodos]

# Crear un gráfico de líneas múltiples
p <- ggplot(data = tbl_2, aes(x = as.factor(foto_mes), y = mean, group = rf.clusters, color = as.factor(rf.clusters))) +
  geom_line() +
  labs(
    title = paste0("Media de ", campo, " por Cluster"),
    x = "Periodo",
    y = "Media"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = tbl_2$foto_mes) +
  theme(text = element_text(size = 7),
        axis.text.x = element_text(size = 5),
        legend.text = element_text(size = 5))

print(p)
}

dev.off()

