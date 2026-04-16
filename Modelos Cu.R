library(DHARMa)
library(car)
library(emmeans)
library(ggplot2)
library(FSA)   
library(rcompanion)


library(glmmTMB)
library(emmeans)
library(ggplot2)


#########Modelado ##########
# 1. Ajuste del modelo con interacción (*)
# Esto analiza el efecto de Cu, de Inoculo y si el Inoculo cambia el efecto del Cu
modelo_interaccion <- glm( gt ~ Cu * Inoculo, 
                          data = Dvivo, 
                          family = Gamma(link = "log"))
# 2 Supuestos (dharma)
residuos <- simulateResiduals(fittedMod = modelo_interaccion)
plot(residuos)

#3 Summary
summary (modelo_interaccion)

#4. Obtención de Medias Estimadas (EMMeans)
# "type = response" vuelve a la escala original de bioacumulación
em_med <- emmeans(modelo_interaccion, ~ Cu|Inoculo, type = "response")
em_df <- as.data.frame(em_med)
print(em_df)
# 5 contrastes
em_contrastes <- emmeans(modelo_interaccion, ~ Inoculo | Cu, type = "response") # entre con y sin inoculo dentro de cada nivel de cobre
comparaciones <- contrast(em_contrastes, method = "pairwise")
comp_df <- as.data.frame(comparaciones)

# agrego sig al grafico
get_stars <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  if (p < 0.1)   return(".")
  return("ns")
}
comp_df$stars <- sapply(comp_df$p.value, get_stars)

em_df <- em_df %>%
  left_join(comp_df[, c("Cu", "stars")], by = "Cu")

# 6. Gráfico con geom_text para los asteriscos
ggplot(em_df, aes(x = Cu, y = response, group = Inoculo, color = Inoculo)) +
  # Usamos barras en lugar de puntos si prefieres (ya que mencionaste barras)
  # Si prefieres puntos, mantén geom_point
  geom_bar(aes(fill = Inoculo), stat = "identity", position = position_dodge(0.9), alpha = 0.7) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, position = position_dodge(0.9), color = "black") +
  # Añadir asteriscos
  # Calculamos y_pos para que flote sobre la barra de error más alta
  geom_text(aes(y = upper.CL + (max(upper.CL) * 0.05), label = stars), 
            position = position_dodge(0.9), color = "black", size = 5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Medias Estimadas de Bioacumulación",
    subtitle = "Significancia: *** < 0.001, ** < 0.01, * < 0.05, . < 0.1",
    y = "Media estimada (BCRaiz)",
    x = "Concentración de Cobre"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer


############ funcion para optimizar #########
library(ggplot2)
library(emmeans)
library(DHARMa)
library(dplyr)

analisis_interaccion_gamma <- function(data, x, y, grupo, titulo = "Análisis de Interacción") {
  
  # 1. Ajuste del modelo (usamos reformulate para manejar nombres de variables)
  formula_str <- reformulate(paste0(x, "*", grupo), response = y)
  modelo <- glm(formula_str, data = data, family = Gamma(link = "log"))
  
  # 2. Supuestos
  message("Generando diagnóstico de residuos...")
  residuos <- simulateResiduals(fittedMod = modelo)
  plot(residuos)
  
  # 3. Summary
  print(summary(modelo))
  
  # 4. Medias Estimadas
  # Reformulamos la especificación para emmeans
  specs_med <- as.formula(paste("~", x, "|", grupo))
  em_med <- emmeans(modelo, specs_med, type = "response")
  em_df <- as.data.frame(em_med)
  
  # 5. Contrastes (Inoculo dentro de cada nivel de Cu)
  specs_cont <- as.formula(paste("~", grupo, "|", x))
  em_contrastes <- emmeans(modelo, specs_cont, type = "response")
  comparaciones <- contrast(em_contrastes, method = "pairwise")
  comp_df <- as.data.frame(comparaciones)
  message("Contrastes")
  print(comparaciones)
  
  # Función interna para estrellas
  get_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    if (p < 0.1)   return(".")
    return("ns")
  }
  
  comp_df$stars <- sapply(comp_df$p.value, get_stars)
  
  # Unir estrellas al dataframe de medias
  # Unimos por la variable x (ej. Cu)
  em_df <- em_df %>%
    left_join(comp_df[, c(x, "stars")], by = x)
  
  # 6. Gráfico
  # Usamos .data[[variable]] para que ggplot reconozca los strings
  p <- ggplot(em_df, aes(x = .data[[x]], y = response, fill = .data[[grupo]])) +
    geom_bar(stat = "identity", position = position_dodge(0.9), alpha = 0.7) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                  width = 0.2, position = position_dodge(0.9), color = "black") +
    # ETIQUETA DE MEDIA (Dentro de la barra)
    geom_text(aes(label = round(response, 2)), 
              position = position_dodge(0.9), vjust = 1.5, color = "black", fontface = "bold") +
    geom_text(aes(y = upper.CL + (max(upper.CL, na.rm = TRUE) * 0.05), label = stars), 
              position = position_dodge(0.9), color = "black", size = 5, fontface = "bold") +
    theme_minimal() +
    labs(
      title = titulo,
      subtitle = "Significancia: *** < 0.001, ** < 0.01, * < 0.05, . < 0.1",
      y = paste("Media estimada de", y),
      x = x
    ) +
    scale_fill_brewer(palette = "Set2")
  
  return(list(modelo = modelo, medias = em_df, grafico = p))
}

DvivoInc <- Dvivo%>% filter(Inoculo == 1)
analisis_unifactorial_gamma <- function(data, x, y, titulo = "Análisis Unifactorial") {
  
  # 1. Ajuste del modelo (Efecto simple de X)
  formula_str <- reformulate(x, response = y)
  modelo <- glm(formula_str, data = data, family = Gamma(link = "log"))
  
  # 2. Supuestos
  message("--- Diagnóstico de residuos (DHARMa) ---")
  residuos <- simulateResiduals(fittedMod = modelo)
  plot(residuos)
  
  # 3. Resumen estadístico
  message("--- Resumen del Modelo (GLM) ---")
  print(summary(modelo))
  
  # 4. Medias Estimadas
  specs_med <- as.formula(paste("~", x))
  em_med <- emmeans(modelo, specs_med, type = "response")
  em_df <- as.data.frame(em_med)
  
  # 5. Contrastes (Comparaciones por pares entre niveles de X)
  message("--- Tabla de Contrastes ---")
  comparaciones <- contrast(em_med, method = "pairwise")
  comp_df <- as.data.frame(comparaciones)
  print(comp_df)
  
  # Función interna para estrellas
  get_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    if (p < 0.1)   return(".")
    return("ns")
  }
  
  comp_df$stars <- sapply(comp_df$p.value, get_stars)
  
  # Nota: En modelos unifactoriales, si X tiene más de 2 niveles,
  # los asteriscos sobre cada barra son complejos de interpretar.
  # Aquí se asume el contraste contra el primer nivel o control.
  
  # 6. Gráfico
  p <- ggplot(em_df, aes(x = .data[[x]], y = response, fill = .data[[x]])) +
    geom_bar(stat = "identity", alpha = 0.7, color = "black") +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                  width = 0.2, color = "black") +
    # ETIQUETA DE MEDIA (Dentro de la barra)
    geom_text(aes(label = round(response, 2)), 
              position = position_dodge(0.9), vjust = 1.5, color = "white", fontface = "bold") +
    theme_minimal() +
    labs(
      title = titulo,
      y = paste("Media estimada de", y),
      x = x
    ) +
    scale_fill_brewer(palette = "Set2")+
    theme(legend.position = "none") # Quitamos leyenda porque X ya está en el eje
  
  return(list(
    modelo = modelo, 
    medias = em_df, 
    contrastes = comp_df, 
    grafico = p
  ))
}

VivoFre<- analisis_unifactorial_gamma (data = DvivoInc, x = "Cu", y = "Frec",  titulo = "Frecuencia in vivo")
VivoFre$grafico
VivoFre$medias

VivoInt <- analisis_unifactorial_gamma (data = DvivoInc, x = "Cu", y = "Int",  titulo = "Intensidad in vivo")
VivoInt$grafico
VivoInt$medias

VivoInt <- analisis_unifactorial_gamma (data = DvivoInc, x = "Cu", y = "Lhif",  titulo = "Largo hifal in vivo")
VivoInt$grafico
VivoInt$medias

#Analisis con interaccion Cu Inoculo

#GT: Significativamente mayor concentracion de glomalina total en suelos Cu2 cuando estan inoculados
VivoGT <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "gt",  grupo = "Inoculo",  titulo = "Medias Estimadas de GT")
VivoGT$grafico
VivoGT$medias

#GFE: siempre sig mayor en suelo inoculado en contraste con suelo sin inoculo.
VivoGfe <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "gfe",  grupo = "Inoculo",  titulo = "Medias Estimadas de Gfe")
VivoGfe$grafico
VivoGfe$medias

#Pesos secos: No se obbservan efectos sobre el desarrollo de la planta
VivoPStotal <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "PStotal",  grupo = "Inoculo",  titulo = "Medias Estimadas de peso seco total")
VivoPStotal$grafico
VivoPStotal$medias

VivoPSR <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "p seco raiz",  grupo = "Inoculo",  titulo = "Medias Estimadas de peso seco raiz")
VivoPSR$grafico
VivoPSR$medias

VivoPSV <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "p s aereo",  grupo = "Inoculo",  titulo = "Medias Estimadas de peso seco vastago")
VivoPSV$grafico
VivoPSV$medias

VivoFt <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "FT",  grupo = "Inoculo",  titulo = "Medias Estimadas de traslocacion")
VivoFt$grafico
VivoFt$medias

Vivoad <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "adherido",  grupo = "Inoculo",  titulo = "Medias Estimadas de adherido")
Vivoad$grafico
Vivoad$medias

#Bioacumulacion: tendencia a menor bioacumulacion en Cu 2 inoculado en comparacion con sin inocular, estas tend tienen sig marginal en Tallo.
VivoBCr <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "BCRaiz",  grupo = "Inoculo",  titulo = "Medias Estimadas de bioacumulacion en raiz")
VivoBCr$grafico
VivoBCr$medias

VivoBCv <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "BCTallo",  grupo = "Inoculo",  titulo = "Medias Estimadas de bioacumulacion en vastago")
VivoBCv$grafico
VivoBCv$medias

#CU: Mas Cu en suelo (.) y mas cu en raiz (*) cuando la planta esta inoculada (en suelos con Cu 2)
VivoCv <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "CuVastago",  grupo = "Inoculo",  titulo = "Medias Estimadas de cobre en vastago")
VivoCv$grafico
VivoCv$medias

VivoCr <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "CuRaiz",  grupo = "Inoculo",  titulo = "Medias Estimadas de cobre en raiz")
VivoCr$grafico
VivoCr$medias

VivoCs <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "CuSuelo",  grupo = "Inoculo",  titulo = "Medias Estimadas de cobre en suelo")
VivoCs$grafico
VivoCs$medias

VivoEE <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "EE",  grupo = "Inoculo",  titulo = "Medias Estimadas de Eficiencia de extracion ")
VivoEE$grafico
VivoEE$medias # Tendencia a mayor EE en Cu con inoculo

## Fosfato: No se observan efectos sobre la absorcion del fosfato del suelo
VivoPs <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "PSuelo",  grupo = "Inoculo",  titulo = "Medias Estimadas fosfato en suelo")
VivoPStotal$grafico
VivoPStotal$medias

VivoPR <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "PRaiz",  grupo = "Inoculo",  titulo = "Medias Estimadas de fosfato en raiz")
VivoPR$grafico
VivoPR$medias

VivoPV <- analisis_interaccion_gamma(
  data = Dvivo, x = "Cu", y = "PVastago",  grupo = "Inoculo",  titulo = "Medias Estimadas de fosfato en vastago")
VivoPV$grafico
VivoPV$medias
