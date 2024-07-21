setwd('~/buckets/b1')
require("data.table")

PARAM <- list()

PARAM$experimento <- "ensamble_b_m_m_r_i_me"

baseline <- fread("./exp/ExpColaborativo/FE2001_baseline/prediccion_acum_100.txt")
missing_values <- fread("./exp/ExpColaborativo/FE2002_missing_values/prediccion_acum_100.txt")
media_lag_delta <- fread("./exp/ExpColaborativo/FE2003_media_lag_delta/prediccion_acum_80.txt")
rankeo <- fread("./exp/ExpColaborativo/FE2004_rankeo/prediccion_acum_100.txt")
intra_mes <- fread("./exp/ExpColaborativo/FE2005_intra_mes/prediccion_acum_100.txt")
meses_extra <- fread("./exp/ExpColaborativo/FE2006_meses_extra/prediccion_acum_100.txt")
ka9241 <- fread("./exp/KA9241/prediccion_acum_140.txt")

df <- rbind(baseline,
            missing_values,
            media_lag_delta,
            rankeo,
            intra_mes,
            meses_extra)

df <- df[,prob_acum_2:= sum(prob_acum),by = numero_de_cliente]

df<- unique(df, by = 'numero_de_cliente')

df <- setorder(df,-prob_acum_2)

df[, c("numero_de_cliente","prob_acum_2")]

dir.create(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"))

cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  df[, Predicted := 0L]
  df[1:envios, Predicted := 1L]
  
  fwrite(df[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}

####################################

#Kaggle
setwd('~/buckets/b1')
require("data.table")

PARAM <- list()

PARAM$experimento <- "ensamble_ka41_ka42_100"


ka9241 <- fread("./exp/KA9241/prediccion_acum_140.txt")
ka9242 <- fread("./exp/KA9242/prediccion_acum_100.txt")


df <- rbind(ka9241,
            ka9242)

df <- df[,prob_acum_2:= sum(prob_acum),by = numero_de_cliente]

df<- unique(df, by = 'numero_de_cliente')

df <- setorder(df,-prob_acum_2)

df[, c("numero_de_cliente","prob_acum_2")]

dir.create(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"), showWarnings = FALSE)
setwd(paste0("./exp/ExpColaborativo/", PARAM$experimento, "/"))

cortes <- seq(8000, 15000, by = 500)

cortes <- 9750
for (envios in cortes) {
  df[, Predicted := 0L]
  df[1:envios, Predicted := 1L]
  
  fwrite(df[, list(numero_de_cliente, Predicted)],
         file = paste0(PARAM$experimento, "_", envios, ".csv"),
         sep = ","
  )
}

###############################

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

