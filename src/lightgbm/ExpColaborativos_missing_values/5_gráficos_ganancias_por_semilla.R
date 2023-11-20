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
resultados_experimentos$resultados_anterior  <- fread( "ExpColaborativo_final_imputación_anterior_final_imputación_anterior_ganancias_semillerio.csv")
resultados_experimentos$resultados_NA_ajustado  <- fread( "ExpColaborativo_final_NA_ajustado_final_NA_ajustado_ganancias_semillerio.csv" )

setnames(resultados_experimentos[[1]], old = colnames(resultados_experimentos[[1]]), 
         new = colnames(resultados_experimentos[[2]]))

#divido por un millon las ganancias

for (i in seq_along(resultados_experimentos)) {
  resultados_experimentos[[i]][, ganancia := ganancia / 1e6]
}
crear_grafico <- function(dataset, nombre_dataset) {
  # Calcula el promedio por cada valor de 'envíos'
  promedio_por_envio <- aggregate(ganancia ~ envios, data = dataset, FUN = mean)
  
  ggplot(dataset, aes(x = envios, y = ganancia, color = as.factor(semilla))) +
    geom_line() +
    geom_line(data = promedio_por_envio, aes(y = ganancia), color = "black", size = 1.5, show.legend = FALSE) + # Línea del promedio sin leyenda
    labs(title = paste("Ganancias por Envío según semilla -", nombre_dataset), x = "Envío", y = "Ganancia") + # Eliminada la leyenda 'color'
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, max(dataset$envios), by = 500), labels = seq(0, max(dataset$envios), by = 500)) +
    guides(color = FALSE) # Elimina leyenda de colores por semilla
}


pdf("graficos_combinados.pdf", width = 10, height = 6, onefile = TRUE)

for (i in seq_along(resultados_experimentos)) {
  grafico <- crear_grafico(resultados_experimentos[[i]], names(resultados_experimentos)[i])
  print(grafico)
}

dev.off()

cat("\n\nPDF con gráficos finalizado\n")