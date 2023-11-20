rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")
require("gridExtra")

#Ir agregando los resultados de cada experimento
resultados_experimentos <- list()

resultados_experimentos$resultados_baseline  <- fread( "exp_ExpColaborativo_final_baseline_final_baseline_ganancias_semillerio.csv" )
resultados_experimentos$resultados_NA  <- fread( "ExpColaborativo_exp_final_imputaciónNA_final_imputaciónNA_ganancias_semillerio.csv" )
resultados_experimentos$resultados_media  <- fread( "exp_ExpColaborativo_final_media_0_final_media_ganancias_semillerio.csv" )
resultados_experimentos$resultados_mediana  <- fread( "exp_ExpColaborativo_final_mediana_0_final_mediana_ganancias_semillerio.csv" )
resultados_experimentos$resultados_anterior  <- fread("ExpColaborativo_final_imputación_anterior_final_imputación_anterior_ganancias_semillerio.csv")
resultados_experimentos$resultados_NA_ajustado  <- fread( "ExpColaborativo_final_NA_ajustado_final_NA_ajustado_ganancias_semillerio.csv" )

setnames(resultados_experimentos[[1]], old = colnames(resultados_experimentos[[1]]), 
         new = colnames(resultados_experimentos[[2]]))

#divido por un millon las ganancias

for (i in seq_along(resultados_experimentos)) {
  resultados_experimentos[[i]][, ganancia := ganancia / 1e6]
}

file_names <- c(
  "resultados_baseline",
  "resultados_NA",
  "resultados_media",
  "resultados_mediana",
  "resultados_anterior",
  "resultados_NA_ajustado"
)

data_list <- lapply(file_names, function(file_name) resultados_experimentos[[file_name]])

data_combined <- do.call(rbind, Map(cbind, data_list, archivo = file_names))

grafico <- ggplot(data_combined, aes(x = envios, y = ganancia, color = archivo)) +
  geom_smooth(method = "auto", se = TRUE) +  
  labs(title = "Ganancias promedio por método aplicado según cantidad de envíos", x = "Cant. envíos", y = "Ganancia") +
  theme_minimal()


ggsave("gráfico_promedios_por_método.pdf", height=6, width=10, plot = grafico, device = "pdf")