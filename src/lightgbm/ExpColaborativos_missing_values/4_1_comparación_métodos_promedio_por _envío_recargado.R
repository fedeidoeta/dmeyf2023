rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")
require("gridExtra")

#Ir agregando los resultados de cada experimento
resultados_experimentos <- list()

resultados_experimentos$baseline_recargado  <- fread( "./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_baseline_recargado_V2_final_baseline_recargado_V2_ganancias_semillerio.csv")
resultados_experimentos$imputacionNA_recargado  <- fread( "./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_NA_recargado_V2_0_final_NA_recargado_V2_ganancias_semillerio.csv" )

resultados_experimentos_parte1 <- list()
resultados_experimentos_parte1$baseline_recargado_parte1  <- fread( "./datasets/ExpColaborativos_missing_values/ExpColaborativo_final_baseline_recargado_final_baseline_recargado_ganancias_semillerio_parte1.csv")
resultados_experimentos_parte1$imputacionNA_recargado_parte1  <- fread( "./datasets/ExpColaborativos_missing_values/ExpColaborativo_final_NA_recargado_final_NA_recargado_ganancias_semillerio_parte1.csv" )

resultados_experimentos$baseline_recargado <- rbind(resultados_experimentos_parte1$baseline_recargado_parte1, resultados_experimentos$baseline_recargado)
resultados_experimentos$imputacionNA_recargado <- rbind(resultados_experimentos_parte1$imputacionNA_recargado_parte1, resultados_experimentos$imputacionNA_recargado)


#Corrijo los titulos de baseline
#setnames(resultados_experimentos[[1]], old = colnames(resultados_experimentos[[1]]), 
#         new = colnames(resultados_experimentos[[2]]))

#Igualo cantidad y semilla
resultados_experimentos$baseline_recargado <- resultados_experimentos$baseline_recargado[resultados_experimentos$baseline_recargado$semilla %in% resultados_experimentos$imputacionNA_recargado$semilla]

#divido por un millon las ganancias

for (i in seq_along(resultados_experimentos)) {
  resultados_experimentos[[i]][, ganancia := ganancia / 1e6]
}

file_names <- names(resultados_experimentos)

data_list <- lapply(file_names, function(file_name) resultados_experimentos[[file_name]])

data_combined <- do.call(rbind, Map(cbind, data_list, archivo = file_names))

grafico <- ggplot(data_combined, aes(x = envios, y = ganancia, color = archivo)) +
  geom_smooth(method = "auto", se = TRUE) +  
  labs(title = "Ganancias promedio por método aplicado según cantidad de envíos", x = "Cant. envíos", y = "Ganancia") +
  theme_minimal()


ggsave("gráfico_promedios_por_método_recargado.pdf", height=6, width=10, plot = grafico, device = "pdf")
