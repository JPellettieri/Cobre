library(readxl)
library(dplyr)

setwd("~/cobre/Cobre")
Datos <- read_excel("datos paper CU.xlsx",col_names = TRUE,  sheet = "Datos")
#formatos para que las cualis esten como factores

Datos <- Datos %>%
  mutate(across(c(gfe, gt, Fe, Zn, Cr, Ni, Pb, `Cu Suelo`, `P Suelo`, Cu, `%F`,	`%I`, `Cu Suelo`)
                ~as.numeric(gsub(",", ".", .))))
Datos <- Datos %>%
  mutate(
    gfe= as.numeric(gfe))
    # Esporas = as.numeric(gsub(",", ".")),
    # `Long Hih` = as.numeric(gsub(",", "."))
Datos %>%
  mutate(Medio = as.factor(Medio)) %>%
  mutate(Inoculo = as.factor(Inoculo ))
#  mutate(Cu = as.factor(Cu))
  
# Convertir comas a punto y a numérico si hace falta
# Datos <- Datos %>%
#   mutate(across(where(is.character), ~gsub(",", ".", .))) %>%
#   mutate(across(where(is.character), as.numeric, .names = "num_{col}"))


#Filto para trabajar solo con los datos in vitro

Dvitro <- Datos%>% filter(Medio == "In vitro")
VR <- Dvitro %>%
  select( Cu, gfe, gt,Fe, `Cu Suelo`, `P Suelo`, `Esporas`, 	`Long Hih`, `%F`,	`%I`, `Cu Suelo`)

library(corrplot)
cor_mat <- cor(VR, use = "complete.obs")
corrplot(cor_mat, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8, addCoef.col = "black",
         number.cex = 0.7,
         number.digits = 2)


library(GGally)
library(ggplot2)


Datos %>%
  mutate(Medio = as.factor(Medio)) %>%
  ggplot(aes(color = TRAT)) +
  ggpairs(columns = c("gfe", "gt", "Fe", "Zn", "Cr", "Ni", "Pb"),
          aes(color = TRAT, alpha = 0.6))

library(tidyr)
library(purrr)

# Crear lista por tratamiento
cor_list <- Datos %>%
  group_split(TRAT) %>%
  setNames(unique(Datos$TRAT)) %>%
  map(~ cor(select(.x, where(is.numeric)), use = "complete.obs"))

library(reshape2)
library(ggplot2)

cor_df <- Datos %>%
  group_by(TRAT) %>%
  summarise(cor = list(cor(select(cur_data(), where(is.numeric)), 
                           use = "complete.obs"))) %>%
  unnest(cor)

# Alternativa más simple:
Datos %>%
  group_by(TRAT) %>%
  group_map(~ {
    cor_mat <- cor(select(.x, where(is.numeric)), use = "complete.obs")
    corrplot(cor_mat, method = "color", main = unique(.x$TRAT))
  })

library(ggcorrplot)

Datos %>%
  split(.$TRAT) %>%
  lapply(function(df) {
    cor_mat <- cor(select(df, where(is.numeric)), use = "complete.obs")
    ggcorrplot(cor_mat, lab = TRUE, title = unique(df$TRAT))
  })

##### IN VIVO

Dvivo <- Datos%>% filter(Medio == "In vivo")
VRvivo <- Dvivo %>%
  select( Cu, gfe, gt,Fe, `Cu Suelo`, `P Suelo`, 	`Long Hih`, `%F`,	`%I`, `Cu Suelo`, `Cu Vástago`,    `Cu Raiz`, `P Vástago`,  `P Suelo` , `Cu Suelo`, `P Raíz`,  `BC Raiz`,   `BC Tallo`, `FT`)
summary(Dvivo)
library(corrplot)
cor_mat <- cor(VR, use = "complete.obs")
corrplot(cor_mat, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8, addCoef.col = "black",
         number.cex = 0.7,
         number.digits = 2)


library(GGally)
library(ggplot2)


Datos %>%
  mutate(Medio = as.factor(Medio)) %>%
  ggplot(aes(color = TRAT)) +
  ggpairs(columns = c("gfe", "gt", "Fe", "Zn", "Cr", "Ni", "Pb"),
          aes(color = TRAT, alpha = 0.6))

library(tidyr)
library(purrr)

# Crear lista por tratamiento
cor_list <- Datos %>%
  group_split(TRAT) %>%
  setNames(unique(Datos$TRAT)) %>%
  map(~ cor(select(.x, where(is.numeric)), use = "complete.obs"))

library(reshape2)
library(ggplot2)

cor_df <- Datos %>%
  group_by(TRAT) %>%
  summarise(cor = list(cor(select(cur_data(), where(is.numeric)), 
                           use = "complete.obs"))) %>%
  unnest(cor)