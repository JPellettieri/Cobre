library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)

setwd("~/cobre/Cobre")
Datos <- read_excel("BD Cobre.xlsx",col_names = TRUE)
summary (Datos)

#Paso variables a numeric
Datos <- Datos %>%
  mutate(across(-c(id, Medio, Inoculo), as.numeric))

Dvivo <- Datos%>% filter(Medio == "in vivo")
VRVivo <- Dvivo[, c("CuSuelo","PSuelo", "CuRaiz", "PRaiz", "CuVastago", "PVastago", "BCRaiz" , "BCTallo", "FT",  
                    "Frec", "Int", "Lhif", "gt", "gfe","adherido", "p seco raiz", "p s aereo" , "PStotal")]
summary(Dvivo)

#matriz de correlacion
cor_VRVivo <- cor(VRVivo, use = "complete.obs", method = "pearson")

corrplot(cor_VRVivo, 
         method = "color", 
         type = "upper",
         addCoef.col = "black",  # muestra valores
         number.cex = 0.6,
         tl.col = "black", 
         tl.srt = 45,
         title= "In vivo")

#### un grafico para cada nivel de cobre vs con y sin inoculo ps aereo y ps raiz###
##### IN VIVO  ######
Dvivo$Inoculo <- as.factor(Dvivo$Inoculo)
Dvivo$Cu <- as.factor(Dvivo$Cu)
library(ggplot2)

ggplot(Dvivo, aes(x = Inoculo, y = `p seco raiz`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "Peso seco raíz", title= "In vivo") +
  theme_minimal()

ggplot(Dvivo, aes(x = Inoculo, y = `p s aereo`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "Peso vastago", title= "In vivo") +
  theme_minimal()

ggplot(Dvivo, aes(x = Inoculo, y = `adherido`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = " adherido", title= "In vivo") +
  theme_minimal()


ggplot(Dvivo, aes(x = Inoculo, y = `gt`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "gt", title= "In vivo") +
  theme_minimal()

# de cobre 1 a cobre 2 hay un salto pasa de ser fertilizante a ser contaminante buscar bibliografia

ggplot(Dvivo, aes(x = Inoculo, y = `gfe`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "gfe", title= "In vivo") +
  theme_minimal()
#GFE me da mas alto que GT chequear metodo de extraccion
# la dif en gt y gfe es indicio que el suelo con 2 Cu es acumulacion de glomalina historica porque tiene caracteres contaminantes. La formacion de glomalina en el ensayo es igual en 1Cu y 2Cu?? AyV no estan tan seguras jaja

ggplot(Dvivo, aes(x = Inoculo, y = `Lhif`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "Lhifal", title= "In vivo") +
  theme_minimal()
#Aumenta la glomalina y baja el largo hifal a meida que aumenta el cu

ggplot(Dvivo, aes(x = Inoculo, y = `BCTallo`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "Bioacumulacion tallo", title= "In vivo") +
  theme_minimal()

ggplot(Dvivo, aes(x = Inoculo, y = `BCRaiz`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "Bioacumulacion raiz", title= "In vivo") +
  theme_minimal()
#inmoviliza no bioacumula

ggplot(Dvivo, aes(x = Inoculo, y = `FT`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "Ft", title= "In vivo") +
  theme_minimal()
# fija en suelo Trasloca menos porque con el inoculo no capta. la glomalina fija

ggplot(Dvivo, aes(x = Inoculo, y = `Frec`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "Frecuencia", title= "In vivo") +
  theme_minimal()

ggplot(Dvivo, aes(x = Inoculo, y = `Int`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "intensidad", title= "In vivo") +
  theme_minimal()

ggplot(Dvivo, aes(x = Inoculo, y = `EE`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "indice de eficiencia de Extraccion", title= "In vivo") +
  theme_minimal()

ggplot(Dvivo, aes(x = Inoculo, y = `PStotal`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "peso seco total", title= "In vivo") +
  theme_minimal()


##### IN VITRO  ######
#coorplot in vitro
DVitro <- Datos%>% filter(Medio == "in vitro")
VRVitro <- DVitro[, c( "Frec", "Int", "Esporas", "Fe", 
                    "PSuelo", "CuSuelo", 
                     "Lhif", "gt", "gfe")]
summary(DVitro)
summary(VRVitro)
#matriz de correlacion
cor_VRVitro <- cor(VRVitro, use = "complete.obs", method = "pearson")

corrplot(cor_VRVitro, 
         method = "color", 
         type = "upper",
         addCoef.col = "black",  # muestra valores
         number.cex = 0.6,
         tl.col = "black", 
         tl.srt = 45,
         title= "In vitro")

#Graficos de barras
DVitro%>%
  mutate (Inoculo = as.character(Inoculo))

ggplot(DVitro, aes(x = Inoculo, y = `Frec`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "frecuencia", title= "In Vitro") +
  theme_minimal()

ggplot(DVitro, aes(x = Inoculo, y = `Int`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "intensidad", title= "In Vitro") +
  theme_minimal()

ggplot(DVitro, aes(x = Inoculo, y = `Esporas`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "Esporas", title= "In Vitro") +
  theme_minimal()

ggplot(DVitro, aes(x = Inoculo, y = `gt`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "glomalina total", title= "In Vitro") +
  theme_minimal()

ggplot(DVitro, aes(x = Inoculo, y = `gfe`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "glomalina facil extraccion", title= "In Vitro") +
  theme_minimal()

ggplot(DVitro, aes(x = Inoculo, y = `PSuelo`, fill = Inoculo)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.2) +
  facet_wrap(~ Cu) +
  labs(x = "Inóculo", y = "PSuelo", title= "In Vitro") +
  theme_minimal()
