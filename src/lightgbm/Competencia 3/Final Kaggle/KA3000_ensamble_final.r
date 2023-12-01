#Elegido competencia 3

#Kaggle
setwd('~/buckets/b1')
require("data.table")

PARAM <- list()

PARAM$experimento <- "ensamble_ka41_ka42_ka43_100_30"


ka9241 <- fread("./exp/KA9241/prediccion_acum_140.txt")
ka9242 <- fread("./exp/KA9242/prediccion_acum_100.txt")
ka9243 <- fread("./exp/KA9243/prediccion_acum_30.txt")

df <- rbind(ka9241,
            ka9242,
            ka9243)

df <- df[,prob_acum_2:= sum(prob_acum),by = numero_de_cliente]

df<- unique(df, by = 'numero_de_cliente')

df <- setorder(df,-prob_acum_2)

df[, c("numero_de_cliente","prob_acum_2")]

dir.create(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"))

cortes <- seq(8000, 15000, by = 500)

#cortes <- 10400
for (envios in cortes) {
  df[, Predicted := 0L]
  df[1:envios, Predicted := 1L]
  
  fwrite(df[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}

