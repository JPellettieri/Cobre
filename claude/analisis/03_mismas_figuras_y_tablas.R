# =============================================================================
# 03_mismas_figuras_y_tablas.R
# Rehace EL MISMO conjunto de figuras y tablas del borrador "Manuscrito cobre 2",
# con los valores recalculados desde BD Cobre.xlsx y un unico diseno.
# Analisis: el de "Modelos Cu.R" (GLM Gamma log; emmeans; Tukey entre dosis en GC3;
# contraste GC3 vs W/I dentro de cada dosis).
# Salida: claude/figuras_mismas/  y  claude/analisis/tablas_mismas/
# =============================================================================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
  library(emmeans); library(car)
})
select <- dplyr::select; filter <- dplyr::filter
args <- commandArgs(trailingOnly = FALSE)
sp <- sub("--file=", "", args[grep("--file=", args)])
HERE <- if (length(sp)) dirname(normalizePath(sp)) else getwd()
ROOT <- normalizePath(file.path(HERE, "..", ".."))
FIG <- file.path(HERE, "..", "figuras_mismas"); dir.create(FIG, showWarnings = FALSE)
TB  <- file.path(HERE, "tablas_mismas"); dir.create(TB, showWarnings = FALSE)
emm_options(rg.limit = 50000)

D <- suppressMessages(read_excel(file.path(ROOT, "BD Cobre.xlsx"), sheet = "Tabla general")) %>%
  mutate(across(-c(id, Medio, Inoculo, Cu), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(Inoc = factor(ifelse(Inoculo == 1, "GC3", "W/I"), levels = c("GC3", "W/I")),
         Dose = factor(paste0("Cu", Cu), levels = c("Cu0", "Cu1", "Cu2")),
         System = ifelse(Medio == "in vivo", "in vivo", "in vitro"))
D$MicelioS[D$id %in% 1:3] <- NA   # copia exacta de ids 10-12 (ver Informe)
D$MicelioS[D$id == 12] <- 88.5    # valor en "TABLA TOTAL"; faltaba en MicelioS
D <- D %>% mutate(ratio_sr = `p s aereo` / `p seco raiz`,
                  FT_P = PVastago / PRaiz, BCP_root = PRaiz / PSuelo, BCP_shoot = PVastago / PSuelo)
vitro <- filter(D, System == "in vitro"); vivo <- filter(D, System == "in vivo")

# ---- Diseno unico -----------------------------------------------------------
# Paleta de las figuras del equipo ("Figuras INGLES.svg", Set2): naranja = in vitro, verde = in vivo,
# gris = W/I. El tono se oscurece con la dosis (Cu0 claro -> Cu2 oscuro). Cu0 = color claro del equipo;
# los pasos se ampliaron y el gris se aclaro para que GC3 y W/I se distingan con daltonismo
# (verde oscuro vs gris original: dE protan 3.0; con estos valores >= 12, validate_palette.js).
INK <- "grey20"
RAMP <- list("in vitro" = c(Cu0 = "#FF9955", Cu1 = "#E86A17", Cu2 = "#B84A00"),
             "in vivo"  = c(Cu0 = "#66C2A5", Cu1 = "#3F9E81", Cu2 = "#2B6E59"),
             "W/I"      = c(Cu0 = "#DEDEDE", Cu1 = "#CFCFCF", Cu2 = "#BDBDBD"))
pal2 <- function(sys) { v <- c(setNames(RAMP[[sys]], paste0("GC3.", names(RAMP[[sys]]))), setNames(RAMP[["W/I"]], paste0("W/I.", names(RAMP[["W/I"]])))); v }
theme_ms <- function(b = 8.5) theme_classic(base_size = b, base_family = "sans") + theme(
  axis.line = element_line(linewidth = .35, colour = INK), axis.ticks = element_line(linewidth = .35, colour = INK),
  axis.text = element_text(colour = "black"), axis.title = element_text(colour = "black"),
  plot.title = element_text(size = b, hjust = 0, margin = margin(b = 4)), plot.tag = element_text(face = "bold", size = b + 2),
  legend.title = element_blank(), legend.key.size = unit(3.2, "mm"), legend.text = element_text(size = b),
  legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), plot.margin = margin(3, 6, 3, 3),
  strip.background = element_blank(), strip.text = element_text(size = b))
save_fig <- function(p, name, w, h) { W <- w / 25.4; H <- h / 25.4
  tiff(file.path(FIG, paste0(name, ".tiff")), width = W, height = H, units = "in", res = 600, compression = "lzw", type = "cairo"); print(p); dev.off()
  png(file.path(FIG, paste0(name, ".png")), width = W, height = H, units = "in", res = 300, type = "cairo"); print(p); dev.off()
  cairo_pdf(file.path(FIG, paste0(name, ".pdf")), width = W, height = H); print(p); dev.off(); invisible() }
se <- function(x) { x <- x[!is.na(x)]; if (length(x) > 1) sd(x) / sqrt(length(x)) else NA_real_ }
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))

# ---- Modelos del borrador ----------------------------------------------------
letters_tukey <- function(dat, y) { d <- dat %>% filter(Inoc == "GC3") %>% transmute(Dose = droplevels(Dose), val = .data[[y]]) %>% filter(!is.na(val), val > 0)
  m <- glm(val ~ Dose, data = d, family = Gamma(link = "log"))
  a <- as.data.frame(Anova(m, type = 2, test.statistic = "F"))
  pr <- as.data.frame(pairs(emmeans(m, ~Dose, type = "response"), adjust = "tukey")) %>% mutate(response = y)
  ml <- multcompView::multcompLetters(setNames(pr$p.value, gsub(" / ", "-", pr$contrast)), threshold = .05)$Letters
  list(let = data.frame(Dose = factor(names(ml), levels = levels(d$Dose)), lab = unname(ml)), F = a[1, "F value"], df2 = df.residual(m), p = a[1, "Pr(>F)"], pairs = pr) }
contr_inoc <- function(dat, y) { d <- dat %>% transmute(Inoc, Dose, val = .data[[y]]) %>% filter(!is.na(val), val > 0)
  n <- d %>% count(Inoc, Dose) %>% filter(n >= 2); d <- semi_join(d, n, by = c("Inoc", "Dose")) %>% droplevels()
  m <- glm(val ~ Inoc * Dose, data = d, family = Gamma(link = "log"))
  a <- as.data.frame(Anova(m, type = 2, test.statistic = "F"))
  ct <- as.data.frame(contrast(emmeans(m, ~Inoc | Dose, type = "response"), "pairwise"))
  ct <- ct %>% filter(!is.na(p.value))
  list(ct = ct %>% transmute(Dose, ratio, p = p.value, response = y), anova = data.frame(response = y, Inoc = a["Inoc", "Pr(>F)"], Cu = a["Dose", "Pr(>F)"], IxCu = a["Inoc:Dose", "Pr(>F)"], df2 = df.residual(m), F_Inoc = a["Inoc", "F value"], F_Cu = a["Dose", "F value"])) }

# ---- Paneles ---------------------------------------------------------------
pt <- function() list(position = position_jitter(width = .12, height = 0, seed = 3))
bar1 <- function(dat, y, title, ylab, lims = NULL, scale = 1) {   # solo GC3: letras Tukey
  d <- dat %>% filter(Inoc == "GC3") %>% transmute(Dose = droplevels(Dose), val = .data[[y]]) %>% filter(!is.na(val))
  s <- d %>% group_by(Dose) %>% summarise(m = mean(val), e = se(val), top = max(c(m + ifelse(is.na(e), 0, e), val)), .groups = "drop")
  L <- letters_tukey(dat, y)
  s <- left_join(s, L$let, by = "Dose")
  sys <- unique(dat$System)
  ggplot(s, aes(Dose, m)) +
    geom_col(aes(fill = Dose), colour = INK, linewidth = .3, width = .62, show.legend = FALSE) +
    scale_fill_manual(values = RAMP[[sys]]) +
    geom_errorbar(aes(ymin = m - e, ymax = m + e), width = .18, linewidth = .4, colour = INK, na.rm = TRUE) +
    geom_point(data = d, aes(Dose, val), shape = 21, size = 1.2, stroke = .3, fill = "white", colour = INK, position = position_jitter(width = .12, height = 0, seed = 3)) +
    geom_text(aes(y = top, label = lab), vjust = -.5, size = 2.9) +
    scale_y_continuous(expand = expansion(mult = c(0, .13)), limits = c(0, NA)) +
    labs(x = NULL, y = ylab, title = title) + theme_ms()
}
bar2 <- function(dat, y, title, ylab, doses = c("Cu0", "Cu1", "Cu2")) {  # factorial: * GC3 vs W/I
  d <- dat %>% filter(Dose %in% doses) %>% transmute(Inoc, Dose = factor(Dose, levels = doses), val = .data[[y]]) %>% filter(!is.na(val))
  s <- d %>% group_by(Inoc, Dose) %>% summarise(m = mean(val), e = se(val), top = max(c(m + ifelse(is.na(e), 0, e), val)), .groups = "drop")
  ct <- contr_inoc(dat %>% filter(Dose %in% doses), y)$ct %>% transmute(Dose = factor(as.character(Dose), levels = doses), lab = stars(p))
  tp <- s %>% group_by(Dose) %>% summarise(top = max(top), .groups = "drop") %>% left_join(ct, by = "Dose")
  pd <- position_dodge(.72)
  sys <- unique(dat$System); s$key <- paste(s$Inoc, s$Dose, sep = ".")
  top_d <- tail(doses, 1)
  ggplot(s, aes(Dose, m, fill = key, group = Inoc)) +
    geom_col(colour = INK, linewidth = .3, width = .66, position = pd) +
    geom_errorbar(aes(ymin = m - e, ymax = m + e), width = .18, linewidth = .4, colour = INK, position = pd, na.rm = TRUE) +
    geom_point(data = d, aes(Dose, val, group = Inoc), shape = 21, size = 1.2, stroke = .3, fill = "white", colour = INK,
               position = position_jitterdodge(jitter.width = .1, dodge.width = .72, seed = 3), inherit.aes = FALSE) +
    geom_text(data = tp, aes(x = Dose, y = top, label = lab), inherit.aes = FALSE, vjust = -.15, size = 3.4) +
    scale_fill_manual(values = pal2(sys), breaks = c(paste0("GC3.", top_d), paste0("W/I.", top_d)), labels = c(paste0("GC3 (", sys, ")"), "W/I")) +
    scale_y_continuous(expand = expansion(mult = c(0, .13)), limits = c(0, NA)) +
    labs(x = NULL, y = ylab, title = title) + theme_ms()
}
TAG <- plot_annotation(tag_levels = "A")
UG <- expression("µg g"^-1*" soil"); MG <- expression("mg kg"^-1*" DW")

# Fig 2: micelio y esporas in vitro (RC, HC), como en el borrador
f2 <- (bar1(vitro, "MicelioMM", "Hyphal length, RC", "cm") + bar1(vitro, "MicelioS", "Hyphal length, HC", expression("cm g"^-1*" soil")) +
       bar1(vitro, "EsporasMM", "Spores, RC", "Number of spores") + bar1(vitro, "EsporasS", "Spores, HC", expression("Spores g"^-1*" soil"))) +
  plot_layout(ncol = 2) + TAG
save_fig(f2, "Fig2_micelio_esporas_in_vitro", 130, 112)

# Fig 3: colonizacion in vitro
f3 <- (bar1(vitro, "Frec", "%F", "Percentage (%)") + bar1(vitro, "Int", "%I", NULL) + bar1(vitro, "Arb", "%A", NULL) + bar1(vitro, "Vesic", "%V", NULL) + bar1(vitro, "EsporasIntraRad", "%IS", NULL)) +
  plot_layout(nrow = 1) + TAG
save_fig(f3, "Fig3_colonizacion_in_vitro", 174, 58)

# Fig 4: colonizacion y micelio in vivo
f4 <- (bar1(vivo, "Frec", "Frequency (%F)", "Percentage (%)") + bar1(vivo, "Int", "Intensity (%I)", "Percentage (%)") + bar1(vivo, "Lhif", "Hyphal length", expression("cm g"^-1*" soil"))) +
  plot_layout(nrow = 1) + TAG
save_fig(f4, "Fig4_colonizacion_micelio_in_vivo", 174, 58)

# Fig 5: GRSP. Inserto: in vitro sin Cu, 10 meses (hoja "glomalina tesina" de datos paper Cu.xlsx)
tes <- data.frame(Inoc = factor(rep(c("GC3", "W/I"), each = 4), levels = c("GC3", "W/I")),
  gt = c(49.432, 60.227, 59.659, 64.205, 75, 59.091, 67.045, 60.227), gfe = c(73.159, 77.519, 77.519, 69.767, 103.2, 94.961, 74.612, 79.942))
inset <- function(y) { s <- tes %>% group_by(Inoc) %>% summarise(m = mean(.data[[y]]), e = se(.data[[y]]), .groups = "drop")
  ggplot(s, aes(Inoc, m, fill = Inoc)) + geom_col(colour = INK, linewidth = .25, width = .6) +
    geom_errorbar(aes(ymin = m - e, ymax = m + e), width = .2, linewidth = .3, colour = INK) +
    scale_fill_manual(values = c(GC3 = RAMP[["in vitro"]][["Cu0"]], "W/I" = RAMP[["W/I"]][["Cu0"]]), guide = "none") + scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0, NA), breaks = c(0, 50, 100)) +
    labs(x = NULL, y = NULL, title = "In vitro, no Cu") + theme_ms(6.5) + theme(plot.margin = margin(0, 0, 0, 0), plot.background = element_rect(fill = "white", colour = NA), plot.title = element_text(size = 6.5, margin = margin(b = 1))) }
gtv <- suppressMessages(bar2(vivo, "gt", "T-GRSP, in vivo", UG) + scale_y_continuous(limits = c(0, 118), expand = expansion(mult = c(0, 0)))) + inset_element(inset("gt"), .02, .68, .34, 1.0, align_to = "panel")
gev <- suppressMessages(bar2(vivo, "gfe", "EE-GRSP, in vivo", UG) + scale_y_continuous(limits = c(0, 170), expand = expansion(mult = c(0, 0)))) + inset_element(inset("gfe"), .02, .68, .34, 1.0, align_to = "panel")
f5 <- (gtv | gev) / (bar2(vitro, "gt", "T-GRSP, in vitro", UG) | bar2(vitro, "gfe", "EE-GRSP, in vitro", UG)) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = list(c("A", "", "B", "", "C", "D"))) & theme(legend.position = "bottom")
save_fig(f5, "Fig5_GRSP", 150, 125)

# Fig 6: suelo adherido
f6 <- bar2(vivo, "adherido", "Soil adhered to roots", expression("g soil g"^-1*" fresh root"))
save_fig(f6, "Fig6_suelo_adherido", 90, 68)

# Fig 10: Cu in vivo
d02 <- c("Cu0", "Cu2")
f7 <- (bar2(vivo, "CuSuelo", "Soil", MG, d02) + bar2(vivo, "CuRaiz", "Root", MG, d02) + bar2(vivo, "CuVastago", "Shoot", MG, d02)) +
  plot_layout(nrow = 1, guides = "collect") + TAG & theme(legend.position = "bottom")
save_fig(f7, "Fig10_Cu_in_vivo", 174, 66)

# Fig 11: bioacumulacion de Cu; Fig 12: translocacion P y Cu
f8 <- (bar2(vivo, "BCTallo", "Shoot", "Bioaccumulation factor (Cu)", d02) + bar2(vivo, "BCRaiz", "Root", "Bioaccumulation factor (Cu)", d02)) +
  plot_layout(nrow = 1, guides = "collect") + TAG & theme(legend.position = "bottom")
save_fig(f8, "Fig11_bioacumulacion_Cu", 120, 66)
f9 <- (bar2(vivo, "FT_P", "Phosphorus", "Translocation factor", d02) + bar2(vivo, "FT", "Copper", "Translocation factor", d02)) +
  plot_layout(nrow = 1, guides = "collect") + TAG & theme(legend.position = "bottom")
save_fig(f9, "Fig12_translocacion", 120, 66)

# Fig 9 y 13: correlaciones con el metodo del borrador: Pearson, complete.obs
corr_fig <- function(dat, vars, labs, size = 2.2, name, w, h) {
  X <- as.matrix(dat[, vars]); X <- X[complete.cases(X), , drop = FALSE]
  R <- cor(X, method = "pearson")
  P <- matrix(NA, ncol(X), ncol(X)); for (i in seq_len(ncol(X))) for (j in seq_len(ncol(X))) if (i != j) P[i, j] <- cor.test(X[, i], X[, j])$p.value
  dimnames(R) <- dimnames(P) <- list(labs, labs)
  long <- expand.grid(a = seq_along(labs), b = seq_along(labs)) %>% filter(a <= b) %>% mutate(r = R[cbind(a, b)], p = P[cbind(a, b)])
  p <- ggplot(long, aes(b, -a, fill = r)) + geom_tile(colour = "white", linewidth = .4) +
    geom_text(aes(label = sub("^-0\\.00$", "0.00", sprintf("%.2f", r)), fontface = ifelse(!is.na(p) & p < .05, "bold", "plain"), colour = abs(r) > .62), size = size) +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none") +
    scale_fill_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC", midpoint = 0, limits = c(-1, 1), name = "r") +
    scale_x_continuous(breaks = seq_along(labs), labels = labs, position = "top", expand = c(0, 0)) +
    scale_y_continuous(breaks = -seq_along(labs), labels = labs, expand = c(0, 0)) + coord_equal() +
    labs(x = NULL, y = NULL) + theme_ms() + theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text.x.top = element_text(angle = 45, hjust = 0),
      legend.position = "right", legend.title = element_text(size = 8.5), legend.key.height = unit(14, "mm"), legend.key.width = unit(3, "mm"))
  save_fig(p, name, w, h); attr(R, "n") <- nrow(X); R }
vi <- vitro %>% filter(Inoc == "GC3") %>% mutate(Cu_num = as.numeric(sub("Cu", "", as.character(Dose))))
Rvt <- corr_fig(vi, c("Cu_num","gfe","gt","Fe","CuSuelo","PSuelo","EsporasS","Lhif","Frec","Int"),
  c("Cu dose","EE-GRSP","T-GRSP","Fe (soil)","Cu (soil)","P (soil)","Spores (HC)","Hyphal length (HC)","%F","%I"), 2.3, "Fig9_correlaciones_in_vitro", 130, 110)
vars_v <- c("PVastago","Frec","Int","PRaiz","CuVastago","CuRaiz","PSuelo","BCTallo","FT","CuSuelo","BCRaiz","Lhif","gt","gfe","p seco raiz","p s aereo","adherido","EE")
labs_v <- c("P (shoot)","%F","%I","P (root)","Cu (shoot)","Cu (root)","P (soil)","BCF shoot","TF","Cu (soil)","BCF root","Hyphal length","T-GRSP","EE-GRSP","Root DW","Shoot DW","Adhered soil","EE")
Rvv_all <- corr_fig(vivo %>% filter(Dose != "Cu1"), vars_v, labs_v, 1.9, "Fig13_correlaciones_in_vivo", 174, 150)

# ---- Tablas -----------------------------------------------------------------
ms <- function(x, dig) { x <- x[!is.na(x)]; n <- length(x); if (!n) return("—")
  if (n == 1) return(paste0(formatC(x, format = "f", digits = dig), "†"))
  paste0(formatC(mean(x), format = "f", digits = dig), " ± ", formatC(se(x), format = "f", digits = dig)) }
fp <- function(p) ifelse(is.na(p), "—", ifelse(p < .001, "<0.001", sprintf("%.3f", p)))
cellv <- function(dat, v, tr, dig) { i <- sub("-.*", "", tr); dd <- sub(".*-", "", tr); ms(dat[[v]][dat$Inoc == i & dat$Dose == dd], dig) }
mk <- function(dat, trs, spec, doses) {
  rows <- lapply(trs, function(tr) c(Treatment = tr, sapply(spec, function(s) cellv(dat, s$v, tr, s$d))))
  an <- lapply(spec, function(s) { if (isTRUE(s$one)) { l <- letters_tukey(dat, s$v); c(NA, l$p, NA) } else { a <- contr_inoc(dat %>% filter(Dose %in% doses), s$v)$anova; c(a$Inoc, a$Cu, a$IxCu) } })
  P <- do.call(cbind, an)
  extra <- lapply(1:3, function(k) c(Treatment = c("P Inoculation", "P Cu", "P Inoc. × Cu")[k], fp(P[k, ])))
  o <- as.data.frame(do.call(rbind, c(rows, extra)), check.names = FALSE); names(o) <- c("Treatment", sapply(spec, `[[`, "n")); o }
tr6 <- c("GC3-Cu0","W/I-Cu0","GC3-Cu1","W/I-Cu1","GC3-Cu2","W/I-Cu2"); tr4 <- c("GC3-Cu0","W/I-Cu0","GC3-Cu2","W/I-Cu2")
t1 <- mk(vivo, tr6, list(list(v="p seco raiz",d=2,n="Root DW (g)"), list(v="p s aereo",d=2,n="Shoot DW (g)"), list(v="PStotal",d=2,n="Total DW (g)"), list(v="ratio_sr",d=1,n="Shoot:root"),
  list(v="Frec",d=1,n="%F",one=TRUE), list(v="Int",d=1,n="%I",one=TRUE), list(v="adherido",d=0,n="Soil aggregation")), c("Cu0","Cu1","Cu2"))
t1[t1$Treatment %in% c("W/I-Cu0","W/I-Cu1","W/I-Cu2"), c("%F","%I")] <- "—"
write.csv(t1, file.path(TB, "Tabla1.csv"), row.names = FALSE, fileEncoding = "UTF-8")
t2 <- mk(vitro, tr4, list(list(v="PSuelo",d=0,n="Total P"), list(v="CuSuelo",d=0,n="Total Cu")), c("Cu0","Cu1","Cu2"))
write.csv(t2, file.path(TB, "Tabla2.csv"), row.names = FALSE, fileEncoding = "UTF-8")
t3 <- mk(vivo, tr4, list(list(v="PVastago",d=0,n="P shoot"), list(v="PRaiz",d=0,n="P root"), list(v="PSuelo",d=0,n="P soil"), list(v="CuVastago",d=1,n="Cu shoot"), list(v="CuRaiz",d=1,n="Cu root"), list(v="CuSuelo",d=1,n="Cu soil")), d02)
write.csv(t3, file.path(TB, "Tabla3.csv"), row.names = FALSE, fileEncoding = "UTF-8")
t4 <- mk(vivo, tr4, list(list(v="BCTallo",d=2,n="BCF Cu shoot"), list(v="BCRaiz",d=2,n="BCF Cu root"), list(v="FT",d=2,n="TF Cu"), list(v="BCP_shoot",d=2,n="BCF P shoot"), list(v="BCP_root",d=2,n="BCF P root"), list(v="FT_P",d=2,n="TF P")), d02)
write.csv(t4, file.path(TB, "TablaS3.csv"), row.names = FALSE, fileEncoding = "UTF-8")
t5 <- mk(vitro, tr6, list(list(v="MicelioMM",d=1,n="Hyphal RC",one=TRUE), list(v="MicelioS",d=1,n="Hyphal HC",one=TRUE), list(v="EsporasMM",d=0,n="Spores RC",one=TRUE), list(v="EsporasS",d=0,n="Spores HC",one=TRUE),
  list(v="LargoRaizMM",d=1,n="Root length"), list(v="Frec",d=1,n="%F",one=TRUE), list(v="Int",d=1,n="%I",one=TRUE), list(v="Arb",d=1,n="%A",one=TRUE), list(v="Vesic",d=1,n="%V",one=TRUE), list(v="EsporasIntraRad",d=1,n="%IS",one=TRUE)), c("Cu0","Cu1","Cu2"))
t5[t5$Treatment %in% c("W/I-Cu0","W/I-Cu1","W/I-Cu2"), c("Hyphal RC","Spores RC","%F","%I","%A","%V","%IS")] <- "—"
t5[t5$Treatment == "W/I-Cu0", "Hyphal HC"] <- "10.8 ± 3.0‡"; t5[t5$Treatment == "W/I-Cu0", "Spores HC"] <- "7 ± 1‡"
write.csv(t5, file.path(TB, "TablaS1.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---- Consola: numeros para el texto y control de reproduccion -----------------
options(width = 170, digits = 3)
for (v in list(list(vitro,"MicelioMM"),list(vitro,"MicelioS"),list(vitro,"EsporasMM"),list(vitro,"EsporasS"),list(vitro,"Frec"),list(vitro,"Int"),list(vitro,"Arb"),list(vitro,"Vesic"),list(vitro,"EsporasIntraRad"),list(vivo,"Frec"),list(vivo,"Int"),list(vivo,"Lhif"))) {
  l <- letters_tukey(v[[1]], v[[2]]); cat(sprintf("%-16s %-8s F=%.2f df2=%d p=%.4f  letras: %s\n", v[[2]], unique(v[[1]]$System), l$F, l$df2, l$p, paste(l$let$Dose, l$let$lab, collapse = " "))); print(l$pairs[,c("contrast","ratio","p.value")]) }
for (v in list(list(vivo,"gt",c("Cu0","Cu1","Cu2")),list(vivo,"gfe",c("Cu0","Cu1","Cu2")),list(vivo,"adherido",c("Cu0","Cu1","Cu2")),list(vitro,"gt",c("Cu0","Cu1","Cu2")),list(vitro,"gfe",c("Cu0","Cu1","Cu2")),list(vitro,"LargoRaizMM",c("Cu0","Cu1","Cu2")),list(vitro,"CuSuelo",c("Cu0","Cu1","Cu2")),list(vitro,"PSuelo",c("Cu0","Cu1","Cu2")),list(vivo,"CuSuelo",d02),list(vivo,"CuRaiz",d02),list(vivo,"CuVastago",d02),list(vivo,"PRaiz",d02),list(vivo,"PVastago",d02),list(vivo,"FT",d02),list(vivo,"FT_P",d02),list(vivo,"BCTallo",d02),list(vivo,"BCRaiz",d02))) {
  r <- contr_inoc(v[[1]] %>% filter(Dose %in% v[[3]]), v[[2]]); cat(v[[2]], unique(v[[1]]$System), "\n"); print(r$anova[,c("Inoc","Cu","IxCu","df2")]); print(r$ct[,c("Dose","ratio","p")]) }
tes_t <- sapply(c("gt","gfe"), function(y) { m <- glm(as.formula(paste(y, "~ Inoc")), data = tes, family = Gamma(link = "log")); summary(m)$coefficients[2,4] }); print(tes_t)
print(tes %>% group_by(Inoc) %>% summarise(across(c(gt, gfe), list(m = mean, se = se))))
cat("\nReproduccion corrplots (borrador -> recalculado)\n")
cat("In vitro n =", attr(Rvt, "n"), " Cu-CuSuelo 0.95 ->", round(Rvt[1,5],2), "; Esporas-Lhif 0.94 ->", round(Rvt[7,8],2), "; gfe-gt -0.59 ->", round(Rvt[2,3],2), "; Fe-PSuelo 0.86 ->", round(Rvt[4,6],2), "\n")
cat("In vivo n =", attr(Rvv_all, "n"), " Frec-Int 0.98 ->", round(Rvv_all[2,3],2), "; PRaiz-BCRaiz -0.89 ->", round(Rvv_all[4,11],2), "; BCTallo-gt -0.73 ->", round(Rvv_all[8,13],2), "; gt-adherido 0.70 ->", round(Rvv_all[13,17],2), "\n")
print(t1); print(t2); print(t3); print(t4); print(t5)
