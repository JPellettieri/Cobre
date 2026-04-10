library(readxl)
library(dplyr)

setwd("~/cobre/Cobre")
Datos <- read_excel("BD Cobre.xlsx",col_names = TRUE)
summary (Datos)

#Paso variables a numeric
Datos <- Datos %>%
  mutate(across(-c(id, Medio, Inoculo), as.numeric))

Dvivo <- Datos%>% filter(Medio == "In vivo")
VRVivo <- Datos[, c("Pvastago", "PRaiz", "CuVastago", "CuRaiz", 
                  "PSuelo", "BCTallo", "FT", "CuSuelo", 
                  "BCRaiz", "Lhif", "gt", "gfe")]

library(corrplot)
#matriz de correlacion
cor_VRVivo <- cor(VRVivo, use = "complete.obs", method = "pearson")

corrplot(cor_VRVivo, 
         method = "color", 
         type = "upper",
         addCoef.col = "black",  # muestra valores
         tl.col = "black", 
         tl.srt = 45)
