#limpio la memoria
#no hace falta correr en una VM
#cree un proyecto y subí los archivos ahí

rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#Ir agregando los resultados de cada experimento
resultados_experimentos <- list()

resultados_experimentos$baseline_recargado  <- fread( "./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_baseline_recargado_V2_final_baseline_recargado_V2_ganancias_semillerio.csv")
resultados_experimentos$imputacionNA_recargado  <- fread( "./datasets/ExpColaborativos_missing_values/exp_ExpColaborativo_final_NA_recargado_V2_0_final_NA_recargado_V2_ganancias_semillerio.csv" )
#Igualo cantidad y semilla
resultados_experimentos$baseline_recargado <- resultados_experimentos$baseline_recargado[resultados_experimentos$baseline_recargado$semilla %in% resultados_experimentos$imputacionNA_recargado$semilla]
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
  
  ganancias <- ganancias[!duplicated(ganancias$semilla), ]
  
  ganancias[, promedio := rowMeans(.SD, na.rm = TRUE), .SDcols = c("max_ganancia", "lag_ganancia", "lead_ganancia")]
  
  return(ganancias)
}

#calculo la ganancia para cada semilla:

ganancias <- setNames(lapply(resultados_experimentos, calcular_ganancias), paste0("ganancias", seq_along(resultados_experimentos)))

sink("resultados_comparaciones_recargado.txt")

# Comparación con baseline
for (i in seq_along(ganancias)[-1]) {
  ganancias_i <- ganancias[[i]]
  
  resultado_wilcox <- wilcox.test(ganancias$ganancias1$promedio, ganancias_i$promedio)
  
  media_baseline <- mean(ganancias$ganancias1$promedio)
  media_ganancias_i <- mean(ganancias_i$promedio)
  
  # Imprimo resultados
  cat("Comparación con ganancias", names(resultados_experimentos[i]), ":\n")
  cat("Estadístico de Wilcoxon:", resultado_wilcox$statistic, "\n")
  cat("P-valor:", resultado_wilcox$p.value, "\n")
  cat("Media de", names(resultados_experimentos[1]) ,":", media_baseline, "\n")
  cat("Media de", names(resultados_experimentos[i]) ,":", media_ganancias_i, "\n")
  cat("\n------------------------\n")
}

sink()

wilcox.test(ganancias$ganancias1$promedio, ganancias_i$promedio)


datos <- data.frame(
          grupo   = as.factor(rep(c("Baseline", "imputoNA"), c(70, 70))),
          valores = c(ganancias$ganancias1$promedio, ganancias_i$promedio))

wilcox_test(valores ~ grupo, data = datos, distribution = "exact", conf.int=0.95)


ganancias$ganancias1$promedio
