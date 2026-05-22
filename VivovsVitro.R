
library(DHARMa)
library(emmeans)
library(ggplot2)

setwd("~/cobre/Cobre")
Datos <- read_excel("BD Cobre.xlsx", sheet ="Tabla general",col_names = TRUE)
Datos$Inoculo <- as.factor(Datos$Inoculo)
Datos$Cu <- as.factor(Datos$Cu)
Datos <- Datos %>%
  mutate(across(-c(id, Medio, Inoculo, Cu), as.numeric))
DatosInc<- Datos%>% filter(Inoculo == 1)
DatosInc

### Modelos comparativos vivo y vitro###
analisis_medio_por_cu_gamma <- function(data, y, titulo = NULL) {
  if (is.null(titulo)) {
    titulo <- paste("Comparación de Medio (vivo vs vitro) según niveles de Cu para:", y)
  }
  
  # 1. Ajuste del modelo con INTERACCIÓN (Medio * Cu)
  # Usamos Gamma con link log tal como venías trabajando
  formula_str <- reformulate("Medio * Cu", response = y)
  modelo <- glm(formula_str, data = data, family = Gamma(link = "log"))
  
  # 2. Supuestos con DHARMa
  message("\n--- Diagnóstico de residuos (DHARMa) ---")
  residuos <- simulateResiduals(fittedMod = modelo)
  plot(residuos)
  
  # 3. Resumen estadístico del modelo
  message("\n--- Resumen del Modelo (GLM) ---")
  print(summary(modelo))
  
  # 4. Medias Estimadas para la combinación Medio y Cu
  em_med <- emmeans(modelo, ~ Medio * Cu, type = "response")
  em_df <- as.data.frame(em_med)
  
  # 5. Contrastes: Comparar Medio DENTRO de cada nivel de Cu
  message("\n--- Tabla de Contrastes (Medio dentro de cada nivel de Cu) ---")
  comparaciones <- contrast(em_med, method = "pairwise", by = "Cu")
  comp_df <- as.data.frame(comparaciones)
  print(comp_df)
  
  # Función interna para asignar estrellas de significancia
  get_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    if (p < 0.1)   return(".")
    return("ns")
  }
  comp_df$stars <- sapply(comp_df$p.value, get_stars)
  
  # Combinar las medias con las estrellas de los contrastes para el gráfico
  # Esto nos ayuda a posicionar las etiquetas de significancia arriba de las barras
  pos_anotaciones <- em_df %>% 
    group_by(Cu) %>% 
    summarise(y_text = max(upper.CL, na.rm = TRUE) * 1.15) %>% 
    left_join(comp_df, by = "Cu")
  
  # 6. Gráfico agrupado y facetado por nivel de Cu
  p <- ggplot(em_df, aes(x = Medio, y = response, fill = Medio)) +
    geom_bar(stat = "identity", alpha = 0.7, color = "black", width = 0.6) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                  width = 0.2, color = "black") +
    # Valor numérico estimado dentro de cada barra
    geom_text(aes(label = round(response, 2)), 
              vjust = 1.5, color = "black", fontface = "bold", size = 3.5) +
    # Facetado por niveles de Cobre
    facet_wrap(~ Cu, labeller = label_both) + 
    theme_minimal() +
    labs(
      title = titulo,
      y = paste("Media estimada de", y),
      x = "Medio de Cultivo"
    ) +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 13),
          strip.text = element_text(face = "bold", size = 11)) # Estilo de las etiquetas de Cu
  
  # Agregar las líneas de contraste y estrellas sobre cada panel de Cu
  p <- p + 
    geom_segment(data = pos_anotaciones, 
                 aes(x = 1, xend = 2, y = y_text * 0.92, yend = y_text * 0.92), 
                 inherit.aes = FALSE, col = "black", lwd = 0.4) +
    geom_text(data = pos_anotaciones, 
              aes(x = 1.5, y = y_text, label = stars), 
              inherit.aes = FALSE, size = 5, fontface = "bold")
  
  return(list(
    modelo = modelo, 
    medias = em_df, 
    contrastes = comp_df, 
    grafico = p
  ))
}

# =====================================================================
# EJECUCIÓN DEL ANÁLISIS POR VARIABLE (Modelo Gamma: Medio dentro de Cu)
# =====================================================================

# 1. Variable: Int
res_int <- analisis_medio_por_cu_gamma(data = DatosInc, y = "Int")
res_int$grafico

# 2. Variable: gfe
res_gfe <- analisis_medio_por_cu_gamma(data = DatosInc, y = "gfe")
res_gfe$grafico

# 3. Variable: gt
res_gt <- analisis_medio_por_cu_gamma(data = DatosInc, y = "gt")
res_gt$grafico

# 4. Variable: Lhif
res_lhif <- analisis_medio_por_cu_gamma(data = DatosInc, y = "Lhif")
res_lhif$grafico

# 5. Variable: CuSuelo
res_cusuelo <- analisis_medio_por_cu_gamma(data = DatosInc, y = "CuSuelo")
res_cusuelo$grafico

# 6. Variable: PSuelo
res_psuelo <- analisis_medio_por_cu_gamma(data = DatosInc, y = "PSuelo")
res_psuelo$grafico

# Para analizar la Frecuencia
res_frec_cu <- analisis_medio_por_cu_gamma(data = DatosInc, y = "Frec")
res_frec_cu$grafico
