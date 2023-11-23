rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")
require("gridExtra")

#Ir agregando los resultados de cada experimento
resultados_experimentos <- list()

resultados_experimentos$baseline  <- fread( "./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_baseline_final_baseline_ganancias_semillerio.csv")
resultados_experimentos$imputacionNA  <- fread( "./datasets/ExpColaborativos_missing_values/ExpColaborativo_exp_final_imputaciónNA_final_imputaciónNA_ganancias_semillerio.csv" )
resultados_experimentos$imputacionNA_ajus  <- fread( "./datasets/ExpColaborativos_missing_values/ExpColaborativo_final_NA_ajustado_final_NA_ajustado_ganancias_semillerio.csv" )
resultados_experimentos$imputacionMedia  <- fread( "./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_media_0_final_media_ganancias_semillerio.csv" )
resultados_experimentos$imputacionMediana  <- fread("./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_mediana_0_final_mediana_ganancias_semillerio.csv")
resultados_experimentos$imputacionAnterior  <- fread("./datasets/ExpColaborativos_missing_values/ExpColaborativo_final_imputación_anterior_final_imputación_anterior_ganancias_semillerio.csv")
resultados_experimentos$imputacionBase_zero_as_missing  <- fread("./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_base_zero_as_missing_0_final_base_zero_as_missing_ganancias_semillerio.csv")
resultados_experimentos$imputacionNA_zero_as_missing  <- fread("./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_NA_zero_as_missing_0_final_NA_zero_as_missing_ganancias_semillerio.csv")


setnames(resultados_experimentos[[1]], old = colnames(resultados_experimentos[[1]]), 
         new = colnames(resultados_experimentos[[2]]))

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


ggsave("gráfico_promedios_por_método.pdf", height=6, width=10, plot = grafico, device = "pdf")
