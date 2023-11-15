#limpio la memoria
#no hace falta correr en una VM
#cree un proyecto y subí los archivos ahí

rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#setwd("~/buckets/b1/")


#colaborativo <- list()

#colaborativo$resultados1  <- "./ExpColaborativo/exp/exp_ExpColaborativo_final_baseline_final_baseline_ganancias_semillerio.csv"
#colaborativo$resulatados2  <- "./ExpColaborativo/exp/ExpColaborativo_exp_final_imputaciónNA_final_imputaciónNA_ganancias_semillerio.csv"


#Ir agregando los resultados de cada experimento
resultados_experimentos <- list()

resultados_experimentos$resultados1  <- fread( "exp_ExpColaborativo_final_baseline_final_baseline_ganancias_semillerio.csv" )
resultados_experimentos$resultados2  <- fread( "ExpColaborativo_exp_final_imputaciónNA_final_imputaciónNA_ganancias_semillerio.csv" )
resultados_experimentos$resultados3  <- fread( "exp_ExpColaborativo_final_media_0_final_media_ganancias_semillerio.csv" )
#resultados_experimentos$resultados4  <- fread(AGREGAR y no hace falta cambiar más nada)

#divido por un millon las ganancias

for (i in seq_along(resultados_experimentos)) {
  resultados_experimentos[[i]][, ganancia := ganancia / 1e6]
}


#armo función para sacar ganancia promedio considerando ganancia máxima de la semilla
#y 500 envíos a cada lado
calcular_ganancias <- function(resultados) {
  max_ganancia <- resultados[, .(max_ganancia = max(ganancia)), by = semilla]
  
  resultados_merged <- merge(resultados, max_ganancia, by = "semilla")
  
  resultados_merged[, lag_ganancia := shift(ganancia, type = "lag"), by = semilla]
  resultados_merged[, lead_ganancia := shift(ganancia, type = "lead"), by = semilla]
  
  ganancias <- resultados_merged[resultados_merged$ganancia == resultados_merged$max_ganancia, 
                                 .(semilla, max_ganancia, lag_ganancia, lead_ganancia)]
  
  ganancias[, promedio := rowMeans(.SD, na.rm = TRUE), .SDcols = c("max_ganancia", "lag_ganancia", "lead_ganancia")]
  
  return(ganancias)
}

#calculo la ganancia para cada semilla:

ganancias <- setNames(lapply(resultados_experimentos, calcular_ganancias), paste0("ganancias", seq_along(resultados_experimentos)))

sink("resultados_comparaciones.txt")

# Comparación con baseline
for (i in seq_along(ganancias)[-1]) {
  ganancias_i <- ganancias[[i]]
  
  resultado_wilcox <- wilcox.test(ganancias$ganancias1$promedio, ganancias_i$promedio)
  
  media_baseline <- mean(ganancias$ganancias1$promedio)
  media_ganancias_i <- mean(ganancias_i$promedio)
  
  # Imprimo resultados
  cat("Comparación con ganancias", i, ":\n")
  cat("Estadístico de Wilcoxon:", resultado_wilcox$statistic, "\n")
  cat("P-valor:", resultado_wilcox$p.value, "\n")
  cat("Media de ganancias1:", media_baseline, "\n")
  cat("Media de ganancias_i:", media_ganancias_i, "\n")
  cat("\n------------------------\n")
}

sink()



##dejo Wilcoxon para comparar dos modelos:

#wilcox.test(ganancias1[, promedio], 
            #ganancias2[, promedio])

#mean(ganancias1[, promedio])
#mean(ganancias2[, promedio])
