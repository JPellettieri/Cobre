# =============================================================================
# 02_figuras_y_tablas.R -- figuras (TIFF 600 dpi, PDF, PNG) y tablas del manuscrito
# Correr DESPUES de 01_analisis_revisado.R
# Salida: claude/figuras/*  y  claude/analisis/tablas_manuscrito/*.csv
# =============================================================================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(emmeans); library(car); library(scales)
})
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
HERE <- if (length(script_path)) dirname(normalizePath(script_path)) else getwd()
ROOT <- normalizePath(file.path(HERE, "..", ".."))
FIG  <- file.path(HERE, "..", "figuras"); dir.create(FIG, showWarnings = FALSE)
TM   <- file.path(HERE, "tablas_manuscrito"); dir.create(TM, showWarnings = FALSE)
TB   <- file.path(HERE, "tablas")

D <- suppressMessages(read_excel(file.path(ROOT, "BD Cobre.xlsx"), sheet = "Tabla general")) %>%
  mutate(across(-c(id, Medio, Inoculo, Cu), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(Inoc = factor(ifelse(Inoculo == 1, "GC3", "NI"), levels = c("GC3", "NI")),
         Dose = factor(paste0("Cu", Cu), levels = c("Cu0", "Cu1", "Cu2")),
         System = factor(ifelse(Medio == "in vivo", "in vivo", "in vitro"), levels = c("in vitro", "in vivo")))
D$MicelioS[D$id %in% 1:3] <- NA; D$MicelioS[D$id == 12] <- 88.5   # ver 01_analisis
vitro <- filter(D, System == "in vitro"); vivo <- filter(D, System == "in vivo")

# ---- Estilo unico -----------------------------------------------------------
COL <- c(GC3 = "#0072B2", NI = "#D55E00")        # Okabe-Ito; forma doble codifica (B/N y daltonismo)
SHP <- c(GC3 = 16, NI = 2)
theme_ms <- function(base = 8.5) {
  theme_classic(base_size = base, base_family = "sans") +
    theme(axis.line = element_line(linewidth = .3, colour = "grey25"),
          axis.ticks = element_line(linewidth = .3, colour = "grey25"),
          axis.text = element_text(colour = "grey15"),
          strip.background = element_blank(), strip.text = element_text(face = "bold", size = base),
          plot.title = element_text(face = "plain", size = base, hjust = 0, margin = margin(b = 3)),
          plot.tag = element_text(face = "bold", size = base + 1.5),
          legend.title = element_blank(), legend.key.size = unit(3.2, "mm"),
          legend.text = element_text(size = base - .5),
          legend.background = element_blank(),
          panel.grid.major.y = element_line(linewidth = .2, colour = "grey90"),
          plot.margin = margin(3, 5, 3, 3))
}
save_fig <- function(p, name, w_mm, h_mm) {
  w <- w_mm / 25.4; h <- h_mm / 25.4
  tiff(file.path(FIG, paste0(name, ".tiff")), width = w, height = h, units = "in", res = 600, compression = "lzw", type = "cairo"); print(p); dev.off()
  png(file.path(FIG, paste0(name, ".png")), width = w, height = h, units = "in", res = 300, type = "cairo"); print(p); dev.off()
  cairo_pdf(file.path(FIG, paste0(name, ".pdf")), width = w, height = h); print(p); dev.off()
}
# Media geometrica +- IC95 (t) -- coherente con escala log/modelos con enlace log
gm_ci <- function(x) { x <- x[!is.na(x) & x > 0]; n <- length(x); m <- mean(log(x)); s <- sd(log(x))
  tc <- if (n > 1) qt(.975, n - 1) else NA; data.frame(y = exp(m), lo = exp(m - tc * s / sqrt(n)), hi = exp(m + tc * s / sqrt(n)), n = n) }
ar_ci <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x); s <- sd(x); tc <- if (n > 1) qt(.975, n - 1) else NA
  data.frame(y = m, lo = m - tc * s / sqrt(n), hi = m + tc * s / sqrt(n), n = n) }
stars <- function(p) ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "")))
lab_dose <- c(Cu0 = "Cu0", Cu1 = "Cu1", Cu2 = "Cu2")

# ---- Figura 2: desarrollo fungico in vitro por compartimiento ----------------
pan_vitro <- function(v, title, ylab, ann = NULL, ni = TRUE) {
  d <- vitro %>% transmute(Inoc, Dose, val = .data[[v]]) %>% filter(!is.na(val), val > 0)
  if (!ni) d <- filter(d, Inoc == "GC3")
  s <- d %>% group_by(Inoc, Dose) %>% group_modify(~ gm_ci(.x$val)) %>% ungroup()
  pd <- position_dodge(.45)
  p <- ggplot(d, aes(Dose, val, colour = Inoc, shape = Inoc)) +
    geom_errorbar(data = s, aes(y = y, ymin = lo, ymax = hi), width = .16, linewidth = .4, position = pd) +
    geom_point(data = s, aes(y = y), size = 2.4, position = pd) +
    geom_point(position = position_jitterdodge(jitter.width = .1, dodge.width = .45, seed = 2), size = .9, alpha = .45) +
    scale_y_log10(labels = label_number(accuracy = 1, big.mark = ",")) +
    scale_colour_manual(values = COL, drop = FALSE, labels = c(GC3 = "GC3", NI = "Non-inoculated")) +
    scale_shape_manual(values = SHP, drop = FALSE, labels = c(GC3 = "GC3", NI = "Non-inoculated")) +
    labs(x = NULL, y = ylab, title = title) + theme_ms()
  if (!is.null(ann)) p <- p + annotate("text", x = ann$x, y = ann$y, label = ann$lab, size = 2.6, colour = "grey15")
  p
}
fa <- pan_vitro("MicelioMM", "Hyphae, root compartment (RC)", "Hyphal length (cm)", ni = FALSE) + guides(colour = "none", shape = "none")
fb <- pan_vitro("MicelioS",  "Hyphae, hyphal compartment (HC, soil)", expression("Hyphal length (cm g"^-1*" soil)"),
                ann = list(x = 1.89, y = 420, lab = "×1.8*"))
fc <- pan_vitro("EsporasMM", "Spores, root compartment (RC)", "Spores (count)", ni = FALSE) + guides(colour = "none", shape = "none")
fd <- pan_vitro("EsporasS",  "Spores, hyphal compartment (HC, soil)", expression("Spores (g"^-1*" soil)"),
                ann = list(x = c(1.89, 2.89), y = c(3300, 36), lab = c("×2.8*", "×0.06***")))
f2 <- (fa + fb + fc + fd) + plot_layout(ncol = 2, guides = "collect") + plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")
save_fig(f2, "Fig2_in_vitro_fungal_development", 120, 110)

# ---- Figura 3: GRSP por sistema ---------------------------------------------
eff <- read.csv(file.path(TB, "T02_factorial_efectos_razones.csv"))
pan_grsp <- function(dat, y, sys, title, ylab) {
  d <- dat %>% transmute(Inoc, Dose, val = .data[[y]]) %>% filter(!is.na(val))
  s <- d %>% group_by(Inoc, Dose) %>% group_modify(~ ar_ci(.x$val)) %>% ungroup()
  e <- eff %>% filter(system == sys, response == y, contrast == "GC3/NI") %>% transmute(Dose = factor(level, levels(D$Dose)), lab = stars(p))
  top <- s %>% group_by(Dose) %>% summarise(ymax = max(hi, na.rm = TRUE), .groups = "drop") %>% left_join(e, by = "Dose")
  pd <- position_dodge(.5)
  ggplot(d, aes(Dose, val, colour = Inoc, shape = Inoc)) +
    geom_errorbar(data = s, aes(y = y, ymin = pmax(lo, 0), ymax = hi), width = .16, linewidth = .4, position = pd) +
    geom_point(data = s, aes(y = y), size = 2.4, position = pd) +
    geom_point(position = position_jitterdodge(jitter.width = .1, dodge.width = .5, seed = 2), size = .9, alpha = .45) +
    geom_text(data = top, aes(x = Dose, y = ymax * 1.05, label = lab), inherit.aes = FALSE, size = 3.2, vjust = 0) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .12))) +
    scale_colour_manual(values = COL, labels = c(GC3 = "GC3", NI = "Non-inoculated")) +
    scale_shape_manual(values = SHP, labels = c(GC3 = "GC3", NI = "Non-inoculated")) +
    labs(x = NULL, y = ylab, title = title) + theme_ms()
}
ug <- expression("µg g"^-1*" soil")
f3 <- (pan_grsp(vitro, "gt", "in vitro", "T-GRSP, in vitro", ug) + pan_grsp(vivo, "gt", "in vivo", "T-GRSP, in vivo", ug) +
       pan_grsp(vitro, "gfe", "in vitro", "EE-GRSP, in vitro", ug) + pan_grsp(vivo, "gfe", "in vivo", "EE-GRSP, in vivo", ug)) +
  plot_layout(ncol = 2, guides = "collect") + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")
save_fig(f3, "Fig3_GRSP", 120, 110)

# ---- Figura 4: Cu medido ---------------------------------------------------
pan_cu <- function(dat, y, title, doses, ylab = expression("Cu (mg kg"^-1*" DW)")) {
  d <- dat %>% filter(Dose %in% doses) %>% transmute(Inoc, Dose = droplevels(Dose), val = .data[[y]]) %>% filter(!is.na(val))
  s <- d %>% group_by(Inoc, Dose) %>% group_modify(~ ar_ci(.x$val)) %>% ungroup() %>% mutate(lo = ifelse(n < 2, NA, lo), hi = ifelse(n < 2, NA, hi))
  pd <- position_dodge(.5)
  ggplot(d, aes(Dose, val, colour = Inoc, shape = Inoc)) +
    geom_errorbar(data = s, aes(y = y, ymin = pmax(lo, 0), ymax = hi), width = .16, linewidth = .4, position = pd, na.rm = TRUE) +
    geom_point(data = s, aes(y = y), size = 2.4, position = pd) +
    geom_point(position = position_jitterdodge(jitter.width = .08, dodge.width = .5, seed = 3), size = .9, alpha = .45) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, .08))) +
    scale_colour_manual(values = COL, labels = c(GC3 = "GC3", NI = "Non-inoculated")) +
    scale_shape_manual(values = SHP, labels = c(GC3 = "GC3", NI = "Non-inoculated")) +
    labs(x = NULL, y = ylab, title = title) + theme_ms()
}
f4 <- (pan_cu(vitro, "CuSuelo", "Soil, in vitro", c("Cu0","Cu1","Cu2")) + pan_cu(vivo, "CuSuelo", "Soil, in vivo", c("Cu0","Cu2")) +
       pan_cu(vivo, "CuRaiz", "Root, in vivo", c("Cu0","Cu2")) + pan_cu(vivo, "CuVastago", "Shoot, in vivo", c("Cu0","Cu2"))) +
  plot_layout(ncol = 2, guides = "collect") + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")
save_fig(f4, "Fig4_Cu_concentrations", 120, 110)

# ---- Figura 5: razones de respuesta, libres de unidades ---------------------
rr <- read.csv(file.path(TB, "T28_razon_Cu2_Cu0_por_sistema.csv"))
ri <- read.csv(file.path(TB, "T29_razon_GC3_NI_GRSP_por_sistema.csv"))
ord <- rev(c("Root colonisation frequency (%F)", "Colonisation intensity (%I)", "Soil hyphal length", "T-GRSP", "EE-GRSP", "Soil Cu (XRF)", "Soil P (XRF)"))
rr$label <- factor(rr$label, levels = ord)
SYS_S <- c("in vitro" = 22, "in vivo" = 16); SYS_C <- c("in vitro" = "grey40", "in vivo" = "black")
g5a <- ggplot(rr, aes(ratio, label, colour = System, shape = System)) +
  geom_vline(xintercept = 1, linewidth = .3, colour = "grey50") +
  geom_errorbar(aes(xmin = lcl, xmax = ucl), orientation = "y", width = .2, linewidth = .4, position = position_dodge(.55)) +
  geom_point(size = 2, position = position_dodge(.55), fill = "white") +
  scale_x_log10(breaks = c(.25, .5, 1, 2, 4, 8), labels = c("0.25", "0.5", "1", "2", "4", "8")) +
  scale_shape_manual(values = SYS_S) + scale_colour_manual(values = SYS_C) +
  labs(x = "Ratio Cu2 / Cu0 in inoculated cultures (95% CI)", y = NULL, title = "Effect of Cu") + theme_ms() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(linewidth = .2, colour = "grey90"), legend.position = "bottom")
ri$lab <- factor(paste0(ifelse(ri$y == "gt", "T-GRSP", "EE-GRSP"), ", ", ri$Dose),
                 levels = rev(c("T-GRSP, Cu0","T-GRSP, Cu2","EE-GRSP, Cu0","EE-GRSP, Cu2")))
g5b <- ggplot(ri, aes(ratio, lab, colour = System, shape = System)) +
  geom_vline(xintercept = 1, linewidth = .3, colour = "grey50") +
  geom_errorbar(aes(xmin = lcl, xmax = ucl), orientation = "y", width = .2, linewidth = .4, position = position_dodge(.55)) +
  geom_point(size = 2, position = position_dodge(.55), fill = "white") +
  scale_x_log10(breaks = c(.5, 1, 2, 4), labels = c("0.5", "1", "2", "4")) +
  scale_shape_manual(values = SYS_S) + scale_colour_manual(values = SYS_C) +
  labs(x = "Ratio GC3 / non-inoculated (95% CI)", y = NULL, title = "Effect of inoculation on GRSP") + theme_ms() +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(linewidth = .2, colour = "grey90"), legend.position = "bottom")
f5 <- (g5a + g5b) + plot_layout(widths = c(1.15, 1), guides = "collect") + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")
save_fig(f5, "Fig5_response_ratios", 174, 75)

# ---- Figura para el INFORME: bimodalidad de P en tejido ----------------------
pp <- vivo %>% filter(!is.na(PVastago) | !is.na(PRaiz)) %>% select(id, Inoc, Dose, Shoot = PVastago, Root = PRaiz) %>%
  pivot_longer(c(Shoot, Root), names_to = "Tissue", values_to = "P") %>% filter(!is.na(P))
fr <- ggplot(pp, aes(P, Tissue, colour = Inoc, shape = Inoc)) + geom_blank() +
  annotate("rect", xmin = 192, xmax = 565, ymin = -Inf, ymax = Inf, fill = "grey93") +
  annotate("text", x = 378, y = 2.45, label = "ningún valor entre 192 y 565", size = 2.7, colour = "grey30") +
  geom_point(position = position_jitter(height = .17, width = 0, seed = 7), size = 2) +
  scale_colour_manual(values = COL, labels = c(GC3 = "GC3", NI = "No inoculado")) +
  scale_shape_manual(values = SHP, labels = c(GC3 = "GC3", NI = "No inoculado")) +
  scale_x_continuous(limits = c(0, 1050), breaks = seq(0, 1000, 200)) +
  labs(x = "P en tejido, tal como figura en la planilla (ppm)", y = NULL) + theme_ms(9) +
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom")
save_fig(fr, "Informe_P_tejido_bimodal", 130, 55)

# ---- PCA para el INFORME ---------------------------------------------------
sc <- read.csv(file.path(TB, "T26_PCA_scores.csv")); lo <- read.csv(file.path(TB, "T27_PCA_loadings.csv"))
names(lo)[1] <- "var"
lo$lab <- dplyr::recode(lo$var, "p seco raiz" = "Raíz PS", "p s aereo" = "Vástago PS", gt = "T-GRSP", gfe = "EE-GRSP", CuSuelo = "Cu suelo", CuRaiz = "Cu raíz", CuVastago = "Cu vástago")
sc$Inoc <- factor(sc$Inoc, levels = c("GC3","NI"))
fp <- ggplot(sc, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, linewidth = .2, colour = "grey80") + geom_vline(xintercept = 0, linewidth = .2, colour = "grey80") +
  geom_segment(data = lo, aes(x = 0, y = 0, xend = PC1 * 3.3, yend = PC2 * 3.3), colour = "grey55", linewidth = .3, arrow = arrow(length = unit(1.2, "mm"))) +
  geom_text(data = lo, aes(x = PC1 * 3.6, y = PC2 * 3.6, label = lab), size = 2.4, colour = "grey25") +
  geom_point(aes(colour = Inoc, shape = interaction(Inoc, Dose)), size = 2.2) +
  scale_colour_manual(values = COL, guide = "none") +
  scale_shape_manual(values = c("GC3.Cu0" = 1, "GC3.Cu2" = 16, "NI.Cu0" = 2, "NI.Cu2" = 17), labels = c("GC3-Cu0","GC3-Cu2","NI-Cu0","NI-Cu2")) +
  labs(x = "PC1 (37 %)", y = "PC2 (25 %)") + theme_ms(9) + theme(panel.grid.major.y = element_blank(), legend.position = "bottom")
save_fig(fp, "Informe_PCA_in_vivo", 110, 95)

# ---- Tablas formateadas ----------------------------------------------------
ms <- function(x, dig = 1) { x <- x[!is.na(x)]; n <- length(x)
  if (n == 0) return("—"); if (n == 1) return(paste0(formatC(x, format = "f", digits = dig), "†"))
  paste0(formatC(mean(x), format = "f", digits = dig), " ± ", formatC(sd(x) / sqrt(n), format = "f", digits = dig)) }
fp_ <- function(p) ifelse(is.na(p), "—", ifelse(p < .001, "<0.001", sprintf("%.3f", p)))
cell <- function(dat, v, i, d, dig) ms(dat[[v]][dat$Inoc == i & dat$Dose == d], dig)

# Tabla 1: in vitro (Cu0/Cu1/Cu2)
comp <- read.csv(file.path(TB, "T08c_compartimiento_loggauss.csv"))
gl1 <- read.csv(file.path(TB, "T05_unifactorial_global.csv"))
an  <- read.csv(file.path(TB, "T01_factorial_ANOVA_gamma.csv"))
pcomp <- function(v) fp_(unique(comp$p_global[comp$var == v]))
pgl <- function(v, s = "in vitro") fp_(gl1$p[gl1$response == v & gl1$system == s])
pan <- function(y, term, s) fp_(an$p[an$response == y & an$term == term & an$system == s])
rowv <- function(lbl, v, i, dig, pv) data.frame(Variable = lbl, Group = i,
    Cu0 = cell(vitro, v, i, "Cu0", dig), Cu1 = cell(vitro, v, i, "Cu1", dig), Cu2 = cell(vitro, v, i, "Cu2", dig), P = pv)
t1 <- bind_rows(
  rowv("Soil Cu, measured (mg kg-1)", "CuSuelo", "GC3", 0, pan("CuSuelo","Dose","in vitro")),
  rowv("Soil Cu, measured (mg kg-1)", "CuSuelo", "NI", 0, ""),
  rowv("Soil P, measured (mg kg-1)", "PSuelo", "GC3", 0, pan("PSuelo","Dose","in vitro")),
  rowv("Soil P, measured (mg kg-1)", "PSuelo", "NI", 0, ""),
  rowv("Root length, RC (cm)", "LargoRaizMM", "GC3", 0, pan("LargoRaizMM","Dose","in vitro")),
  rowv("Root length, RC (cm)", "LargoRaizMM", "NI", 0, ""),
  rowv("Hyphal length, RC (cm)", "MicelioMM", "GC3", 0, pcomp("MicelioMM")),
  rowv("Hyphal length, HC (cm g-1)", "MicelioS", "GC3", 1, pcomp("MicelioS")),
  rowv("Hyphal length, HC (cm g-1)", "MicelioS", "NI", 1, ""),
  rowv("Spores, RC (count)", "EsporasMM", "GC3", 0, pcomp("EsporasMM")),
  rowv("Spores, HC (g-1)", "EsporasS", "GC3", 0, pcomp("EsporasS")),
  rowv("Spores, HC (g-1)", "EsporasS", "NI", 0, ""),
  rowv("Frequency, %F", "Frec", "GC3", 1, pgl("Frec")),
  rowv("Intensity, %I", "Int", "GC3", 1, pgl("Int")),
  rowv("Arbuscules, %A", "Arb", "GC3", 1, pgl("Arb")),
  rowv("Vesicles, %V", "Vesic", "GC3", 1, pgl("Vesic")),
  rowv("Intraradical spores, %IS", "EsporasIntraRad", "GC3", 1, pgl("EsporasIntraRad")))
write.csv(t1, file.path(TM, "Tabla1_in_vitro.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Tabla 2: in vivo (3 dosis)
vivo <- mutate(vivo, ratio_sr = `p s aereo` / `p seco raiz`)
fit_sr <- glm(ratio_sr ~ Inoc * Dose, data = vivo, family = Gamma(link = "log")); a_sr <- Anova(fit_sr, type = 2, test.statistic = "F")
rowiv <- function(lbl, v, dig, pI, pC, pX) data.frame(Variable = lbl,
    GC3_Cu0 = cell(vivo, v, "GC3", "Cu0", dig), GC3_Cu1 = cell(vivo, v, "GC3", "Cu1", dig), GC3_Cu2 = cell(vivo, v, "GC3", "Cu2", dig),
    NI_Cu0 = cell(vivo, v, "NI", "Cu0", dig), NI_Cu1 = cell(vivo, v, "NI", "Cu1", dig), NI_Cu2 = cell(vivo, v, "NI", "Cu2", dig),
    P_Inoc = pI, P_Cu = pC, P_IxCu = pX)
rowan <- function(lbl, v, y, dig) rowiv(lbl, v, dig, pan(y,"Inoc","in vivo"), pan(y,"Dose","in vivo"), pan(y,"Inoc:Dose","in vivo"))
rowone <- function(lbl, v, dig, y) { z <- D %>% filter(System == "in vivo", Inoc == "NI"); r <- rowiv(lbl, v, dig, "—", pgl(y, "in vivo"), "—"); r$NI_Cu0 <- r$NI_Cu1 <- r$NI_Cu2 <- "—"; r }
t2 <- bind_rows(
  rowan("Root dry weight (g)", "p seco raiz", "p seco raiz", 2),
  rowan("Shoot dry weight (g)", "p s aereo", "p s aereo", 2),
  rowan("Total dry weight (g)", "PStotal", "PStotal", 2),
  rowiv("Shoot:root ratio", "ratio_sr", 1, fp_(a_sr["Inoc","Pr(>F)"]), fp_(a_sr["Dose","Pr(>F)"]), fp_(a_sr["Inoc:Dose","Pr(>F)"])),
  rowone("Frequency, %F", "Frec", 1, "Frec"), rowone("Intensity, %I", "Int", 1, "Int"),
  rowone("Soil hyphal length (cm g-1)", "Lhif", 1, "Lhif"),
  rowan("T-GRSP (ug g-1)", "gt", "gt", 1), rowan("EE-GRSP (ug g-1)", "gfe", "gfe", 1))
write.csv(t2, file.path(TM, "Tabla2_in_vivo.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Tabla 3: Cu in vivo y balance
vs <- vivo %>% filter(Dose != "Cu1") %>% mutate(Cu_plant = CuVastago * `p s aereo` + CuRaiz * `p seco raiz`)
up <- read.csv(file.path(TB, "T17_contenido_Cu_planta_ANOVA.csv"))
rowc <- function(lbl, v, dig, y) { pv <- if (y == "Cu_plant_ug") setNames(up$p[up$response == "Cu_plant_ug"], up$term[up$response == "Cu_plant_ug"]) else NULL
  data.frame(Variable = lbl,
    NI_Cu0 = ms(vs[[v]][vs$Inoc == "NI" & vs$Dose == "Cu0"], dig), GC3_Cu0 = ms(vs[[v]][vs$Inoc == "GC3" & vs$Dose == "Cu0"], dig),
    NI_Cu2 = ms(vs[[v]][vs$Inoc == "NI" & vs$Dose == "Cu2"], dig), GC3_Cu2 = ms(vs[[v]][vs$Inoc == "GC3" & vs$Dose == "Cu2"], dig),
    P_Inoc = if (!is.null(pv)) fp_(pv["Inoc"]) else pan(y,"Inoc","in vivo"), P_Cu = if (!is.null(pv)) fp_(pv["Dose"]) else pan(y,"Dose","in vivo"),
    P_IxCu = if (!is.null(pv)) fp_(pv["Inoc:Dose"]) else pan(y,"Inoc:Dose","in vivo")) }
t3 <- bind_rows(rowc("Soil Cu (mg kg-1)", "CuSuelo", 1, "CuSuelo"), rowc("Root Cu (mg kg-1)", "CuRaiz", 1, "CuRaiz"), rowc("Shoot Cu (mg kg-1)", "CuVastago", 1, "CuVastago"),
  rowc("BCF root", "BCRaiz", 2, "BCRaiz"), rowc("BCF shoot", "BCTallo", 2, "BCTallo"), rowc("TF", "FT", 2, "FT"),
  rowc("Plant Cu content (ug plant-1)", "Cu_plant", 0, "Cu_plant_ug"), rowc("Soil P (mg kg-1)", "PSuelo", 0, "PSuelo"))
write.csv(t3, file.path(TM, "Tabla3_Cu_in_vivo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
# Tabla S: P en tejido segun registro y segun hipotesis x10
ts <- read.csv(file.path(TB, "T11_sensibilidad_P_tejido_medias.csv"))
write.csv(ts, file.path(TM, "TablaS_P_tejido.csv"), row.names = FALSE)
print(t1); print(t2); print(t3)
