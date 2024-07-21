rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("ggplot2")
require("gridExtra")

#Ir agregando los resultados de cada experimento
resultados_experimentos <- list()

#resultados_experimentos$baseline  <- fread( "./datasets/Experimentos_colaborativos_202107/exp_ExpColaborativo_202107_202107_FE2001_baseline_20_202107_FE2001_baseline_ganancias_semillerio.csv")
resultados_experimentos$baseline  <- fread( "./datasets/Experimentos_colaborativos_202107/exp_ExpColaborativo_202107_202107_FE3001_20_202107_FE3001_ganancias_semillerio.csv")
resultados_experimentos$missing_values  <- fread( "./datasets/Experimentos_colaborativos_202107/exp_ExpColaborativo_202107_202107_FE3002_m_20_202107_FE3002_m_ganancias_semillerio.csv")
resultados_experimentos$media_delta_lag  <- fread( "./datasets/Experimentos_colaborativos_202107/exp_ExpColaborativo_202107_202107_FE2003_media_delta_lag_20_202107_FE2003_media_delta_lag_ganancias_semillerio.csv")
resultados_experimentos$ranking  <- fread( "./datasets/Experimentos_colaborativos_202107/exp_ExpColaborativo_202107_202107_FE2004_rankeo_20_202107_FE2004_rankeo_ganancias_semillerio.csv")
resultados_experimentos$intra_mes  <- fread( "./datasets/Experimentos_colaborativos_202107/exp_ExpColaborativo_202107_202107_FE3005_m_m_r_i_20_202107_FE3005_m_m_r_i_ganancias_semillerio.csv")


#setnames(resultados_experimentos[[1]], old = colnames(resultados_experimentos[[1]]), 
#         new = colnames(resultados_experimentos[[2]]))

#divido por un millon las ganancias

for (i in seq_along(resultados_experimentos)) {
  resultados_experimentos[[i]][, ganancia := ganancia / 1e6]
}

file_names <- names(resultados_experimentos)

data_list <- lapply(file_names, function(file_name) resultados_experimentos[[file_name]])

data_combined <- do.call(rbind, Map(cbind, data_list, experimento = file_names))

nsemilla <- 1

grafico <- ggplot(data_combined[semilla==nsemilla,], aes(x = envios, y = ganancia, color = experimento)) +
  geom_smooth(method = "auto", se = TRUE) +  
  labs(title = "Ganancias promedio por método aplicado según cantidad de envíos", x = "Cant. envíos", y = "Ganancia") +
  theme_minimal()
grafico

ggsave("TEST_gráfico_promedios_por_método.pdf", height=6, width=10, plot = grafico, device = "pdf")
