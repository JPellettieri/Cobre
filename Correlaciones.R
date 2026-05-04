########## Correlacion

## Correlación Parcial:
#Si la correlación entre X e Y es significativa, pero al calcular la correlación parcial (controlando por Z) el p-valor deja de ser significativo, entonces la relación era indirecta.
library(ppcor)

#1) P valor base (correlaciones sig entre variables?)
cor.test(Datos$Var1, Datos$Var2, method = "spearman") # "pearson" relacion lineal (V continuas y norm), "spearman" no son normales o relaciones no lineales

#2) correlacion parcial
pcor.test(Datos$Variable1, Datos$Variable2, Datos$VariableControl)

#3) Modelos de Ecuaciones Estructurales (SEM) Permiten descomponer la correlación total en Efectos Directos e Efectos Indirectos y otorgan un $p$-valor para cada "flecha" de tu modelo
library(lavaan)
modelo <- ' VarRespuesta ~ b*VarIndependiente + c*VarMediadora
            VarMediadora ~ a*VarIndependiente 
            indirecto := a*c '



######
library(Hmisc)
DvitroInc
library(dplyr)

D_num <- DvitroInc[, sapply(DvitroInc, is.numeric)]
D_num <- D_num[, colSums(is.na(D_num)) < nrow(D_num)] # eliminar columnas completamente vacías
D_num <- subset(D_num, select = -c(`Lhif`,id))

res <- rcorr(as.matrix(D_num), type = "spearman")
res
cor_mat <- res$r      # correlaciones
p_mat   <- res$P      # p-values

threshold_r <- 0.2 #la correlacion minma necesaria para que se grafique en el mapa
threshold_p <- 0.05   # p valor marginal como minimo

valid <- abs(cor_mat) > threshold_r & p_mat < threshold_p
#Filtro la matriz
cor_mat[!valid] <- 0
cor_mat[is.na(cor_mat)] <- 0

p_mat
library(reshape2)

df_cor <- melt(cor_mat)

# limpiar tipos
df_cor$Var1 <- as.character(df_cor$Var1)
df_cor$Var2 <- as.character(df_cor$Var2)

# quedarte solo con relaciones reales
df_cor <- subset(df_cor, Var1 != Var2 & value != 0)

# eliminar duplicados
df_cor <- df_cor[df_cor$Var1 < df_cor$Var2, ]

library(igraph)

g <- graph_from_data_frame(df_cor, directed = FALSE)
g <- delete_vertices(g, degree(g) == 0) #Elimino nodos aislados
library(ggraph)


# 8. Agrupación 
V(g)$grupo <- ifelse(grepl("Suelo|S|Zn|Fe|gt|gfe", V(g)$name), "Suelo",
                      "raiz")

ggraph(g, layout = "fr") +
  geom_edge_link(aes(edge_width = abs(value), repel = TRUE,
                     color = value > 0),
                 alpha = 0.8) +
  geom_edge_link(aes(label = round(value, 2)),
                 angle_calc = 'along',
                 label_dodge = unit(2.5, 'mm')) +
  geom_node_point(aes(color = grupo), size = 6) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_edge_color_manual(values = c("red", "blue")) +
  scale_edge_width(range = c(0.5, 2)) +
  theme_void()

##### Repito grafo con correlaciones significativas pero in vivo

DvivoInc

library(dplyr)

D_VIVO <- DvivoInc[, sapply(DvivoInc, is.numeric)]
D_VIVO <- D_VIVO[, colSums(is.na(D_VIVO)) < nrow(D_VIVO)] # eliminar columnas completamente vacías
D_VIVO <- subset(D_VIVO, select = -c(id,PStotal,`largo vasta`, id, `pf raiz`, `pf aer`,`RAIZ+TIERRA`,PStotal))# eliminar columnas `largo vasta`, id, `pf raiz`, `pf aer`,`RAIZ+TIERRA`,PStotal 
colnames(D_VIVO) <- make.names(trimws(colnames(D_VIVO)))
res <- rcorr(as.matrix(D_VIVO), type = "spearman")
res
cor_mat <- res$r      # correlaciones
p_mat   <- res$P      # p-values

threshold_r <- 0.2
threshold_p <- 0.05   # p valor marginal como minimo

valid <- abs(cor_mat) > threshold_r & p_mat < threshold_p
#Filtro la matriz
cor_mat[!valid] <- 0
cor_mat[is.na(cor_mat)] <- 0


library(reshape2)

df_cor <- melt(cor_mat)

# limpiar tipos
df_cor$Var1 <- as.character(df_cor$Var1)
df_cor$Var2 <- as.character(df_cor$Var2)

# quedarte solo con relaciones reales
df_cor <- subset(df_cor, Var1 != Var2 & value != 0)

# eliminar duplicados
df_cor <- df_cor[df_cor$Var1 < df_cor$Var2, ]


library(igraph)

g <- graph_from_data_frame(df_cor, directed = FALSE)
g <- delete_vertices(g, degree(g) == 0) #Elimino nodos aislados
library(ggraph)


# 8. Agrupación 
V(g)$grupo <- ifelse(grepl("Suelo", V(g)$name), "Suelo",
                     ifelse(grepl("Raiz|raiz|FT", V(g)$name), "Raíz",
                            ifelse(grepl("Vastago|Tallo|aereo", V(g)$name), "Vástago",
                                   "Micorriza")))
ggraph(g, layout = "fr") +
  geom_edge_link(aes(edge_width = abs(value), repel = TRUE,
                     color = value > 0),
                 alpha = 0.8) +
  geom_edge_link(aes(label = round(value, 2)),
                 angle_calc = 'along',
                 label_dodge = unit(2.5, 'mm')) +
  
  geom_node_point(aes(color = grupo), size = 6) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  
  scale_edge_color_manual(values = c("red", "blue"),
                          labels = c("Negativa", "Positiva"),
                          name = "Relación") +
  
  scale_edge_width(range = c(0.5, 2)) +
  theme_void()
install.packages("ggraph")






###############
# Paquetes
# install.packages(c("igraph", "ggraph", "tidygraph", "reshape2"))
library(igraph)
library(ggraph)
library(tidygraph)
library(reshape2)

# 1. Matriz de correlación
cor_VRVivo
cor_VRVitro
# 1. Asegurar matriz numérica limpia
#cor_mat <- as.matrix(cor_VRVivo)
cor_mat <- as.matrix(cor_VRVitro)
# 2. Eliminar posibles NA (clave)
cor_mat[is.na(cor_mat)] <- 0

# 3. Formato largo
df_cor <- melt(cor_mat)

# 4. Convertir a character (evita error de factores)
df_cor$Var1 <- as.character(df_cor$Var1)
df_cor$Var2 <- as.character(df_cor$Var2)

# 5. Filtrar
threshold <- 0.6   # te recomiendo más estricto para este dataset

df_cor <- subset(df_cor, Var1 != Var2 & abs(value) > threshold)

# 6. Eliminar duplicados
df_cor <- df_cor[df_cor$Var1 < df_cor$Var2, ]

# 7. Crear grafo
g <- graph_from_data_frame(df_cor, directed = FALSE)

# 8. Agrupación ecológica
V(g)$grupo <- ifelse(grepl("Suelo", V(g)$name), "Suelo",
                     ifelse(grepl("Raiz", V(g)$name), "Raíz",
                            ifelse(grepl("Vastago|Tallo|aereo", V(g)$name), "Parte aérea",
                                   "Fisiológico")))

# 9. Plot
ggraph(g, layout = "fr") +
  geom_edge_link(aes(edge_alpha = abs(value),
                     edge_width = abs(value),
                     color = value > 0)) +
  geom_edge_link(aes(label = round(value, 2)),
                 angle_calc = 'along',
                 label_dodge = unit(2.5, 'mm')) +
  
  geom_node_point(aes(color = grupo), size = 6) +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  scale_edge_color_manual(values = c("red", "blue")) +
  scale_edge_width(range = c(0.5, 2)) +
  theme_void()

