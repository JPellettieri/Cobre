# =============================================================================
# 04_figuras_alternativas.R -- otras formas de mostrar los mismos resultados
# Correr despues de 01_analisis_revisado.R (usa sus tablas de efectos).
# Salida: "claude/figuras alternativas/"  (paleta del equipo: naranja = in vitro,
# verde = in vivo, gris = W/I; tono mas oscuro con la dosis)
# =============================================================================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(emmeans); library(scales)
})
select <- dplyr::select; filter <- dplyr::filter
args <- commandArgs(trailingOnly = FALSE); sp <- sub("--file=", "", args[grep("--file=", args)])
HERE <- if (length(sp)) dirname(normalizePath(sp)) else getwd()
ROOT <- normalizePath(file.path(HERE, "..", "..")); TB <- file.path(HERE, "tablas")
OUT <- file.path(HERE, "..", "figuras alternativas"); dir.create(OUT, showWarnings = FALSE)

D <- suppressMessages(read_excel(file.path(ROOT, "BD Cobre.xlsx"), sheet = "Tabla general")) %>%
  mutate(across(-c(id, Medio, Inoculo, Cu), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(Inoc = factor(ifelse(Inoculo == 1, "GC3", "W/I"), levels = c("GC3", "W/I")),
         Dose = factor(paste0("Cu", Cu), levels = c("Cu0", "Cu1", "Cu2")),
         System = ifelse(Medio == "in vivo", "in vivo", "in vitro"))
D$MicelioS[D$id %in% 1:3] <- NA; D$MicelioS[D$id == 12] <- 88.5
vitro <- filter(D, System == "in vitro"); vivo <- filter(D, System == "in vivo")

INK <- "grey20"
RAMP <- list("in vitro" = c(Cu0 = "#FF9955", Cu1 = "#E86A17", Cu2 = "#B84A00"),
             "in vivo"  = c(Cu0 = "#66C2A5", Cu1 = "#3F9E81", Cu2 = "#2B6E59"),
             "W/I"      = c(Cu0 = "#DEDEDE", Cu1 = "#CFCFCF", Cu2 = "#BDBDBD"))
SYS <- c("in vitro" = "#E86A17", "in vivo" = "#3F9E81")
theme_ms <- function(b = 8.5) theme_classic(base_size = b, base_family = "sans") + theme(
  axis.line = element_line(linewidth = .35, colour = INK), axis.ticks = element_line(linewidth = .35, colour = INK),
  axis.text = element_text(colour = "black"), plot.title = element_text(size = b, hjust = 0, margin = margin(b = 4)),
  plot.tag = element_text(face = "bold", size = b + 2), legend.title = element_blank(), legend.position = "bottom",
  legend.key.size = unit(3.2, "mm"), legend.margin = margin(0, 0, 0, 0), plot.margin = margin(3, 6, 3, 3),
  strip.background = element_blank(), strip.text = element_text(size = b, face = "bold"))
save_fig <- function(p, name, w, h) { W <- w / 25.4; H <- h / 25.4
  png(file.path(OUT, paste0(name, ".png")), width = W, height = H, units = "in", res = 300, type = "cairo"); print(p); dev.off()
  cairo_pdf(file.path(OUT, paste0(name, ".pdf")), width = W, height = H); print(p); dev.off()
  tiff(file.path(OUT, paste0(name, ".tiff")), width = W, height = H, units = "in", res = 600, compression = "lzw", type = "cairo"); print(p); dev.off(); invisible() }
ci_t <- function(x) { x <- x[!is.na(x)]; n <- length(x); m <- mean(x); h <- if (n > 1) qt(.975, n - 1) * sd(x) / sqrt(n) else NA; data.frame(m = m, lo = m - h, hi = m + h, n = n) }
gci_t <- function(x) { x <- x[!is.na(x) & x > 0]; n <- length(x); l <- log(x); m <- mean(l); h <- if (n > 1) qt(.975, n - 1) * sd(l) / sqrt(n) else NA; data.frame(m = exp(m), lo = exp(m - h), hi = exp(m + h), n = n) }

# ---- ALT1: puntos + media e IC95 (en vez de barras +- EE), escala log -------------
dot1 <- function(v, title, ylab) {
  d <- vitro %>% filter(Inoc == "GC3") %>% transmute(Dose, val = .data[[v]]) %>% filter(!is.na(val), val > 0)
  s <- d %>% group_by(Dose) %>% group_modify(~ gci_t(.x$val)) %>% ungroup()
  ggplot(d, aes(Dose, val, colour = Dose)) +
    geom_point(position = position_jitter(width = .08, height = 0, seed = 1), size = 2, alpha = .85) +
    geom_errorbar(data = s, aes(y = m, ymin = lo, ymax = hi), width = 0, linewidth = .6, position = position_nudge(x = .25), na.rm = TRUE) +
    geom_point(data = s, aes(y = m), shape = 18, size = 3.4, position = position_nudge(x = .25)) +
    scale_colour_manual(values = RAMP[["in vitro"]], guide = "none") +
    scale_y_log10(labels = label_number(big.mark = ",")) + labs(x = NULL, y = ylab, title = title) + theme_ms()
}
a1 <- (dot1("MicelioMM", "Hyphal length, RC", "cm") + dot1("MicelioS", "Hyphal length, HC", expression("cm g"^-1)) +
       dot1("EsporasMM", "Spores, RC", "Number") + dot1("EsporasS", "Spores, HC", expression("Spores g"^-1))) + plot_layout(nrow = 1) + plot_annotation(tag_levels = "A")
save_fig(a1, "ALT1_puntos_IC_hongo_in_vitro", 174, 62)

# ---- ALT2: respuesta relativa por compartimiento (normas de reaccion) ----------
rel <- vitro %>% filter(Inoc == "GC3") %>% select(id, Dose, `Hyphae|RC` = MicelioMM, `Hyphae|HC` = MicelioS, `Spores|RC` = EsporasMM, `Spores|HC` = EsporasS) %>%
  pivot_longer(-c(id, Dose), names_to = c("Var", "Comp"), names_sep = "\\|", values_to = "val") %>% filter(!is.na(val), val > 0) %>%
  group_by(Var, Comp) %>% mutate(fc = val / exp(mean(log(val[Dose == "Cu0"])))) %>% ungroup()
rs <- rel %>% group_by(Var, Comp, Dose) %>% group_modify(~ gci_t(.x$fc)) %>% ungroup()
rel$Comp <- factor(rel$Comp, levels = c("RC", "HC"), labels = c("Root compartment (RC, no Cu)", "Hyphal compartment (HC, soil + Cu)"))
rs$Comp <- factor(rs$Comp, levels = c("RC", "HC"), labels = levels(rel$Comp))
a2 <- ggplot(rs, aes(Dose, m, colour = Comp, group = Comp)) +
  geom_hline(yintercept = 1, linewidth = .3, colour = "grey55") +
  geom_point(data = rel, aes(y = fc), position = position_jitterdodge(jitter.width = .08, dodge.width = .3, seed = 2), size = 1.3, alpha = .55) +
  geom_line(position = position_dodge(.3), linewidth = .7) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = .1, linewidth = .45, position = position_dodge(.3), na.rm = TRUE) +
  geom_point(position = position_dodge(.3), size = 2.6, shape = 18) +
  facet_wrap(~Var) + scale_y_log10(breaks = c(.03, .1, .3, 1, 3, 10), labels = c("0.03", "0.1", "0.3", "1", "3", "10")) +
  scale_colour_manual(values = c("#7A7A7A", "#B84A00")) +
  labs(x = NULL, y = "Fold change relative to Cu0 (log scale)") + theme_ms()
save_fig(a2, "ALT2_respuesta_relativa_por_compartimiento", 140, 80)

# ---- ALT3: grafico de estimacion (datos + efecto con IC) para GRSP in vivo ----
eff <- read.csv(file.path(TB, "T02_factorial_efectos_razones.csv"))
est_panel <- function(y, lab, ylim_top) {
  d <- vivo %>% transmute(Inoc, Dose, val = .data[[y]]) %>% filter(!is.na(val)) %>% mutate(key = paste(Inoc, Dose, sep = "."))
  s <- d %>% group_by(Inoc, Dose, key) %>% group_modify(~ ci_t(.x$val)) %>% ungroup()
  pal <- c(setNames(RAMP[["in vivo"]], paste0("GC3.", names(RAMP[["in vivo"]]))), setNames(RAMP[["W/I"]], paste0("W/I.", names(RAMP[["W/I"]]))))
  pd <- position_dodge(.6)
  top <- ggplot(d, aes(Dose, val, colour = key, group = Inoc)) +
    geom_point(position = position_jitterdodge(jitter.width = .08, dodge.width = .6, seed = 3), size = 1.9) +
    geom_errorbar(data = s, aes(y = m, ymin = lo, ymax = hi), width = 0, linewidth = .6, colour = INK, position = pd) +
    geom_point(data = s, aes(y = m), shape = 18, size = 3, colour = INK, position = pd) +
    scale_colour_manual(values = pal, breaks = c("GC3.Cu1", "W/I.Cu1"), labels = c("GC3", "W/I")) +
    scale_y_continuous(limits = c(0, ylim_top), expand = expansion(mult = c(0, .02))) +
    labs(x = NULL, y = expression("µg g"^-1*" soil"), title = lab) + theme_ms() + theme(legend.position = "top")
  e <- eff %>% filter(system == "in vivo", response == y, contrast == "GC3/NI") %>% mutate(Dose = factor(level, levels = levels(D$Dose)))
  bot <- ggplot(e, aes(Dose, ratio)) + geom_hline(yintercept = 1, linewidth = .3, colour = "grey55") +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, linewidth = .7, colour = RAMP[["in vivo"]][["Cu2"]]) +
    geom_point(shape = 18, size = 3.4, colour = RAMP[["in vivo"]][["Cu2"]]) +
    scale_y_log10(limits = c(.6, 3), breaks = c(.75, 1, 1.5, 2, 3)) +
    labs(x = NULL, y = "GC3 / W/I (95% CI)") + theme_ms()
  top / bot + plot_layout(heights = c(2, 1.15))
}
a3 <- wrap_elements(est_panel("gt", "T-GRSP", 80)) | wrap_elements(est_panel("gfe", "EE-GRSP", 130))
save_fig(a3 + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 10.5)), "ALT3_estimacion_GRSP_in_vivo", 174, 110)

# ---- ALT4: forest de todos los efectos in vivo ---------------------------------
lab_map <- c("p seco raiz" = "Root dry weight", "p s aereo" = "Shoot dry weight", "PStotal" = "Total dry weight", "gt" = "T-GRSP", "gfe" = "EE-GRSP",
             "adherido" = "Adhered soil", "CuSuelo" = "Cu, soil", "CuRaiz" = "Cu, root", "CuVastago" = "Cu, shoot", "BCRaiz" = "BCF, root", "BCTallo" = "BCF, shoot", "FT" = "TF (Cu)")
grp <- c("Root dry weight" = "Plant", "Shoot dry weight" = "Plant", "Total dry weight" = "Plant", "T-GRSP" = "Soil protein", "EE-GRSP" = "Soil protein", "Adhered soil" = "Soil protein",
         "Cu, soil" = "Copper", "Cu, root" = "Copper", "Cu, shoot" = "Copper", "BCF, root" = "Copper", "BCF, shoot" = "Copper", "TF (Cu)" = "Copper")
f <- eff %>% filter(system == "in vivo", response %in% names(lab_map)) %>%
  filter((contrast == "GC3/NI" & level == "Cu2") | (contrast == "Cu2 / Cu0")) %>%
  mutate(var = factor(lab_map[response], levels = rev(lab_map)), g = factor(grp[as.character(var)], levels = c("Plant", "Soil protein", "Copper")),
         panel = ifelse(contrast == "GC3/NI", "Inoculation effect at Cu2\n(GC3 / W/I)", "Copper effect\n(Cu2 / Cu0)"),
         who = ifelse(contrast == "GC3/NI", "GC3 vs W/I", ifelse(level == "GC3", "in GC3", "in W/I")),
         sig = ifelse(p < .05, "P < 0.05", "n.s."))
f$panel <- factor(f$panel, levels = c("Inoculation effect at Cu2\n(GC3 / W/I)", "Copper effect\n(Cu2 / Cu0)"))
a4 <- ggplot(f, aes(ratio, var, colour = who, shape = sig)) +
  geom_vline(xintercept = 1, linewidth = .3, colour = "grey55") +
  geom_errorbar(aes(xmin = lcl, xmax = ucl), orientation = "y", width = 0, linewidth = .55, position = position_dodge(.55)) +
  geom_point(size = 2.2, position = position_dodge(.55), fill = "white", stroke = .7) +
  facet_grid(g ~ panel, scales = "free_y", space = "free_y") +
  scale_x_log10(breaks = c(.25, .5, 1, 2, 4), labels = c("0.25", "0.5", "1", "2", "4")) +
  scale_colour_manual(values = c("GC3 vs W/I" = "#3F9E81", "in GC3" = "#2B6E59", "in W/I" = "#8C8C8C")) +
  scale_shape_manual(values = c("P < 0.05" = 16, "n.s." = 21)) +
  labs(x = "Ratio of means (95% CI, log scale)", y = NULL) + theme_ms() +
  theme(panel.grid.major.x = element_line(linewidth = .2, colour = "grey90"), strip.text.y = element_text(angle = 0, hjust = 0))
save_fig(a4, "ALT4_forest_efectos_in_vivo", 174, 120)

# ---- ALT5: in vitro vs in vivo en unidades libres (razones) ---------------------
rr <- read.csv(file.path(TB, "T28_razon_Cu2_Cu0_por_sistema.csv"))
ord <- rev(c("Root colonisation frequency (%F)", "Colonisation intensity (%I)", "Soil hyphal length", "T-GRSP", "EE-GRSP", "Soil Cu (XRF)", "Soil P (XRF)"))
rr$label <- factor(rr$label, levels = ord)
ri <- read.csv(file.path(TB, "T29_razon_GC3_NI_GRSP_por_sistema.csv"))
ri$lab <- factor(paste0(ifelse(ri$y == "gt", "T-GRSP", "EE-GRSP"), ", ", ri$Dose), levels = rev(c("T-GRSP, Cu0", "T-GRSP, Cu2", "EE-GRSP, Cu0", "EE-GRSP, Cu2")))
fp <- function(dat, yv, ttl, xl, br) ggplot(dat, aes(ratio, .data[[yv]], colour = System, shape = System)) +
  geom_vline(xintercept = 1, linewidth = .3, colour = "grey55") +
  geom_errorbar(aes(xmin = lcl, xmax = ucl), orientation = "y", width = 0, linewidth = .6, position = position_dodge(.55)) +
  geom_point(size = 2.3, position = position_dodge(.55)) + scale_colour_manual(values = SYS) + scale_shape_manual(values = c("in vitro" = 15, "in vivo" = 16)) +
  scale_x_log10(breaks = br, labels = as.character(br)) + labs(x = xl, y = NULL, title = ttl) + theme_ms() +
  theme(panel.grid.major.x = element_line(linewidth = .2, colour = "grey90"))
a5 <- (fp(rr, "label", "Effect of Cu in GC3", "Cu2 / Cu0 (95% CI)", c(.5, 1, 2, 4, 8)) + fp(ri, "lab", "Effect of inoculation on GRSP", "GC3 / W/I (95% CI)", c(.5, 1, 2))) +
  plot_layout(widths = c(1.2, 1), guides = "collect") + plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")
save_fig(a5, "ALT5_in_vitro_vs_in_vivo", 174, 75)

# ---- ALT6: balance de Cu por maceta (escala log) ------------------------------
mb <- vivo %>% filter(Dose != "Cu1") %>% mutate(soil_kg = (`RAIZ+TIERRA` - `pf raiz`) / 1000,
  `Soil pool` = CuSuelo * soil_kg, `Whole plant` = (CuVastago * `p s aereo` + coalesce(CuRaiz * `p seco raiz`, 0)) / 1000) %>%
  select(id, Inoc, Dose, `Soil pool`, `Whole plant`) %>% pivot_longer(c(`Soil pool`, `Whole plant`), names_to = "Pool", values_to = "mg") %>% filter(!is.na(mg), mg > 0)
mb$grp <- factor(paste(mb$Inoc, mb$Dose), levels = c("W/I Cu0", "GC3 Cu0", "W/I Cu2", "GC3 Cu2"))
pct <- mb %>% group_by(grp) %>% summarise(p = 100 * mean(mg[Pool == "Whole plant"]) / mean(mg[Pool == "Soil pool"]), .groups = "drop")
colk <- c("W/I Cu0" = RAMP[["W/I"]][["Cu0"]], "GC3 Cu0" = RAMP[["in vivo"]][["Cu0"]], "W/I Cu2" = RAMP[["W/I"]][["Cu2"]], "GC3 Cu2" = RAMP[["in vivo"]][["Cu2"]])
a6 <- ggplot(mb, aes(grp, mg, fill = grp, shape = Pool)) +
  geom_point(position = position_jitter(width = .1, height = 0, seed = 4), size = 2.4, colour = INK, stroke = .3) +
  geom_text(data = pct, aes(x = grp, y = .03, label = sprintf("plant = %.2f%%", p)), inherit.aes = FALSE, size = 2.6, colour = "grey25") +
  scale_y_log10(breaks = c(.03, .1, .3, 1, 3, 10, 30, 100, 300), labels = c("0.03", "0.1", "0.3", "1", "3", "10", "30", "100", "300")) +
  scale_fill_manual(values = colk, guide = "none") + scale_shape_manual(values = c("Soil pool" = 22, "Whole plant" = 21)) +
  labs(x = NULL, y = "Cu per pot (mg, log scale)") + theme_ms()
save_fig(a6, "ALT6_balance_de_cobre", 110, 80)

# ---- ALT7: dispersion de las relaciones clave (en vez de matriz de correlacion) --
rel_panel <- function(dat, x, y, xl, yl, sys = "in vivo") {
  d <- dat %>% transmute(Inoc, Dose, x = .data[[x]], y = .data[[y]]) %>% filter(!is.na(x), !is.na(y)) %>% mutate(key = paste(Inoc, Dose, sep = "."))
  r <- cor(d$x, d$y)
  pal <- c(setNames(RAMP[[sys]], paste0("GC3.", names(RAMP[[sys]]))), setNames(RAMP[["W/I"]], paste0("W/I.", names(RAMP[["W/I"]]))))
  ggplot(d, aes(x, y, fill = key, shape = Inoc)) + geom_point(size = 2.4, colour = INK, stroke = .35) +
    scale_fill_manual(values = pal, guide = "none") + scale_shape_manual(values = c(GC3 = 21, "W/I" = 24), guide = "none") +
    labs(x = xl, y = yl, title = sprintf("r = %.2f (n = %d)", r, nrow(d))) + theme_ms()
}
v02 <- vivo %>% filter(Dose != "Cu1")
a7 <- (rel_panel(filter(vivo, Inoc == "GC3"), "Lhif", "gfe", expression("Hyphal length (cm g"^-1*")"), "EE-GRSP") +
       rel_panel(vivo, "Frec", "gfe", "Colonization frequency (%F)", "EE-GRSP") +
       rel_panel(v02, "CuSuelo", "gt", expression("Soil Cu (mg kg"^-1*")"), "T-GRSP") +
       rel_panel(v02, "CuSuelo", "CuRaiz", expression("Soil Cu (mg kg"^-1*")"), expression("Root Cu (mg kg"^-1*")"))) +
  plot_layout(ncol = 2) + plot_annotation(tag_levels = "A", caption = "Shade = Cu level (Cu0 light, Cu2 dark). Circles = GC3; triangles = W/I.") &
  theme(plot.caption = element_text(size = 7.5, hjust = 0))
save_fig(a7, "ALT7_relaciones_clave", 130, 115)

# ---- ALT8: perfil de colonizacion in vitro --------------------------------------
cl <- vitro %>% filter(Inoc == "GC3") %>% select(id, Dose, `%F` = Frec, `%I` = Int, `%A` = Arb, `%V` = Vesic, `%IS` = EsporasIntraRad) %>%
  pivot_longer(-c(id, Dose), names_to = "Str", values_to = "val") %>% filter(!is.na(val)) %>% mutate(Str = factor(Str, levels = c("%F", "%I", "%A", "%V", "%IS")))
cs <- cl %>% group_by(Str, Dose) %>% group_modify(~ ci_t(.x$val)) %>% ungroup()
a8 <- ggplot(cs, aes(Str, m, colour = Dose, group = Dose)) +
  geom_point(data = cl, aes(y = val), position = position_jitterdodge(jitter.width = .05, dodge.width = .55, seed = 5), size = 1.2, alpha = .55) +
  geom_errorbar(aes(ymin = pmax(lo, 0), ymax = pmin(hi, 100)), width = 0, linewidth = .5, position = position_dodge(.55), na.rm = TRUE) +
  geom_point(position = position_dodge(.55), size = 2.6, shape = 18) +
  scale_colour_manual(values = RAMP[["in vitro"]]) + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  geom_vline(xintercept = 1.5:4.5, linewidth = .2, colour = "grey88") +
  labs(x = NULL, y = "Percentage (%)") + theme_ms()
save_fig(a8, "ALT8_perfil_colonizacion_in_vitro", 110, 75)
cat("OK\n"); print(pct)
