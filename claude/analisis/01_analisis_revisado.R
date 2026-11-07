# =============================================================================
# 01_analisis_revisado.R
# Re-analisis reproducible del ensayo Cu x R. intraradices GC3 (in vitro / in vivo)
#
# Correr desde cualquier lado:
#   Rscript claude/analisis/01_analisis_revisado.R
# Lee  : BD Cobre.xlsx (raiz del proyecto) -- NO se modifica el original
# Genera: claude/analisis/tablas/*.csv
#
# Convencion: GC3 = inoculado; NI = no inoculado ("W/I" en el borrador)
# Modelo principal: el mismo que ya usaban en "Modelos Cu.R" (GLM Gamma, log),
# pero con (i) todas las dosis medidas, (ii) tablas de efectos con IC95 %,
# (iii) correccion BH por familia, (iv) analisis de sensibilidad.
# =============================================================================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(emmeans)
  library(car); library(glmmTMB); library(vegan); library(DHARMa)
})
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
HERE <- if (length(script_path)) dirname(normalizePath(script_path)) else getwd()
ROOT <- normalizePath(file.path(HERE, "..", ".."))
OUT  <- file.path(HERE, "tablas"); dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
emm_options(rg.limit = 50000)
set.seed(20261106)

# ---- 1. Datos --------------------------------------------------------------
raw <- suppressMessages(read_excel(file.path(ROOT, "BD Cobre.xlsx"), sheet = "Tabla general"))
D <- raw %>%
  mutate(across(-c(id, Medio, Inoculo, Cu), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(Inoc = factor(ifelse(Inoculo == 1, "GC3", "NI"), levels = c("NI", "GC3")),
         Dose = factor(paste0("Cu", Cu), levels = c("Cu0", "Cu1", "Cu2")),
         System = factor(ifelse(Medio == "in vivo", "in vivo", "in vitro"),
                         levels = c("in vitro", "in vivo")))

# Correcciones DOCUMENTADAS (ver informe, seccion "Auditoria de datos"):
# - ids 1-3 (NI-Cu0 in vitro): MicelioS es copia exacta de ids 10-12 (GC3-Cu0).
#   El borrador anterior reportaba 10.8 +/- 3.0 para NI-Cu0; se anula el dato copiado.
D$MicelioS[D$id %in% 1:3] <- NA
# - ids 10-18: Lhif (in vitro) duplica MicelioS; id 12 tiene Lhif=88.5 y MicelioS=NA.
#   Se recupera el valor (planilla original: 88.5 en "TABLA TOTAL").
D$MicelioS[D$id == 12] <- 88.5
# - EsporasS: la columna de BD para NI-Cu0 esta vacia (ok, el manuscrito viejo decia 7 +/- 1).

vitro <- filter(D, System == "in vitro")
vivo  <- filter(D, System == "in vivo")

fmt_p <- function(p) ifelse(is.na(p), NA_character_,
                            ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# ---- 2. Descriptivos (media, DE, EE, n) por tratamiento ---------------------
desc_vars <- c("Frec","Int","Arb","Vesic","EsporasIntraRad","Entrada","MicelioMM","MicelioS",
               "EsporasMM","EsporasS","LargoRaizMM","gt","gfe","Lhif","CuSuelo","PSuelo",
               "CuVastago","CuRaiz","PVastago","PRaiz","BCRaiz","BCTallo","FT",
               "p seco raiz","p s aereo","PStotal","adherido")
descr <- D %>%
  pivot_longer(all_of(desc_vars), names_to = "variable", values_to = "valor") %>%
  group_by(System, Inoc, Dose, variable) %>%
  summarise(n = sum(!is.na(valor)), media = mean(valor, na.rm = TRUE),
            de = sd(valor, na.rm = TRUE), ee = de / sqrt(n),
            min = suppressWarnings(min(valor, na.rm = TRUE)),
            max = suppressWarnings(max(valor, na.rm = TRUE)), .groups = "drop") %>%
  filter(n > 0)
write.csv(descr, file.path(OUT, "T00_descriptivos_por_tratamiento.csv"), row.names = FALSE)

# ---- 3. Modelos factoriales GLM Gamma(log): Inoculo x Cu --------------------
# Devuelve: ANOVA tipo II (F), medias, razones GC3/NI dentro de dosis y Cu/Cu0 dentro de inoculo
fit_factorial <- function(dat, y, system, doses = c("Cu0","Cu1","Cu2")) {
  d <- dat %>% filter(Dose %in% doses) %>% select(Inoc, Dose, id, val = all_of(y)) %>%
    filter(!is.na(val), val > 0) %>% droplevels()
  n_ok <- d %>% count(Inoc, Dose)
  # se excluyen celdas con n < 2 (no estiman dispersion)
  keep <- n_ok %>% filter(n >= 2)
  d <- semi_join(d, keep, by = c("Inoc","Dose")) %>% droplevels()
  m <- glm(val ~ Inoc * Dose, data = d, family = Gamma(link = "log"))
  a <- as.data.frame(Anova(m, type = 2, test.statistic = "F"))
  a$term <- rownames(a)
  a <- a %>% filter(term != "Residuals") %>%
    transmute(system, response = y, term, F = `F value`, df1 = Df,
              df2 = df.residual(m), p = `Pr(>F)`)
  emm <- emmeans(m, ~ Inoc | Dose, type = "response")
  by_dose <- as.data.frame(confint(contrast(emm, "revpairwise", type = "response"))) %>%
    left_join(as.data.frame(contrast(emm, "revpairwise", type = "response")) %>%
                select(Dose, p.value), by = "Dose") %>%
    transmute(system, response = y, contrast = "GC3/NI", level = as.character(Dose),
              ratio, lcl = lower.CL, ucl = upper.CL, p = p.value)
  emm2 <- emmeans(m, ~ Dose | Inoc, type = "response")
  ct2 <- contrast(emm2, "trt.vs.ctrl", ref = 1, type = "response", adjust = "none")
  by_inoc <- as.data.frame(confint(ct2)) %>%
    left_join(as.data.frame(ct2) %>% select(contrast, Inoc, p.value), by = c("contrast","Inoc")) %>%
    transmute(system, response = y, contrast = as.character(contrast),
              level = as.character(Inoc), ratio, lcl = lower.CL, ucl = upper.CL, p = p.value)
  means <- as.data.frame(emmeans(m, ~ Inoc * Dose, type = "response")) %>%
    transmute(system, response = y, Inoc, Dose, mean = response, lcl = lower.CL, ucl = upper.CL)
  sim <- tryCatch(simulateResiduals(m, n = 500, plot = FALSE), error = function(e) NULL)
  diag <- data.frame(system, response = y, n = nrow(d),
                     cells = paste(nrow(keep), "celdas"),
                     dispersion_gamma = summary(m)$dispersion,
                     ks_p = if (!is.null(sim)) suppressWarnings(testUniformity(sim, plot = FALSE)$p.value) else NA,
                     levene_p = tryCatch(leveneTest(log(val) ~ Inoc * Dose, data = d)$`Pr(>F)`[1],
                                         error = function(e) NA))
  list(anova = a, effects = bind_rows(by_dose, by_inoc), means = means, diag = diag, model = m)
}

resp_vivo <- c("gt","gfe","p seco raiz","p s aereo","PStotal","adherido")
resp_vivo_chem <- c("CuSuelo","CuRaiz","CuVastago","PSuelo","PRaiz","PVastago","BCRaiz","BCTallo","FT")
resp_vitro <- c("gt","gfe","LargoRaizMM","CuSuelo","PSuelo")

res <- list()
for (y in resp_vivo)      res[[paste("vivo", y)]]  <- fit_factorial(vivo,  y, "in vivo")
for (y in resp_vivo_chem) res[[paste("vivo", y)]]  <- fit_factorial(vivo,  y, "in vivo", c("Cu0","Cu2"))
for (y in resp_vitro)     res[[paste("vitro", y)]] <- fit_factorial(vitro, y, "in vitro")

anova_tab <- bind_rows(lapply(res, `[[`, "anova"))
eff_tab   <- bind_rows(lapply(res, `[[`, "effects"))
mean_tab  <- bind_rows(lapply(res, `[[`, "means"))
diag_tab  <- bind_rows(lapply(res, `[[`, "diag"))

# BH por familia: una familia = cada sistema, sobre las pruebas F del modelo
anova_tab <- anova_tab %>% group_by(system) %>% mutate(p_BH = p.adjust(p, "BH")) %>% ungroup()
eff_tab <- eff_tab %>% group_by(system) %>% mutate(p_BH = p.adjust(p, "BH")) %>% ungroup()
# Familia PRIMARIA in vivo (hipotesis del trabajo): T-GRSP, EE-GRSP, Cu suelo, Cu raiz, Cu vastago
prim <- anova_tab %>% filter(system == "in vivo", response %in% c("gt","gfe","CuSuelo","CuRaiz","CuVastago")) %>%
  mutate(p_BH_primaria = p.adjust(p, "BH"))
write.csv(prim, file.path(OUT, "T01b_familia_primaria_in_vivo.csv"), row.names = FALSE)
write.csv(anova_tab, file.path(OUT, "T01_factorial_ANOVA_gamma.csv"), row.names = FALSE)
write.csv(eff_tab,   file.path(OUT, "T02_factorial_efectos_razones.csv"), row.names = FALSE)
write.csv(mean_tab,  file.path(OUT, "T03_factorial_medias_estimadas.csv"), row.names = FALSE)
write.csv(diag_tab,  file.path(OUT, "T04_diagnosticos.csv"), row.names = FALSE)

# ---- 4. Variables solo en inoculados (un factor: Cu) ------------------------
# %F como conteo binomial (quasi), %I/%A/%V/%IS como proporcion (quasi-binomial),
# largo de micelio Gamma, esporas binomial negativa (con quasi-Poisson si no converge).
# Denominadores de %F inferidos de los decimales: ver seccion 9.
one_factor <- function(dat, y, system, family_type) {
  d <- dat %>% filter(Inoc == "GC3") %>% select(Dose, val = all_of(y)) %>% filter(!is.na(val)) %>% droplevels()
  if (family_type == "quasibinomial") {
    d$prop <- d$val / 100
    m <- glm(prop ~ Dose, data = d, family = quasibinomial(link = "logit"))
    a <- Anova(m, type = 2, test.statistic = "F")
    F_ <- a[1,"F value"]; p_ <- a[1,"Pr(>F)"]; df2 <- df.residual(m); tp <- "response"
  } else if (family_type == "gamma") {
    m <- glm(val ~ Dose, data = d, family = Gamma(link = "log"))
    a <- Anova(m, type = 2, test.statistic = "F")
    F_ <- a[1,"F value"]; p_ <- a[1,"Pr(>F)"]; df2 <- df.residual(m)
  } else {
    m <- MASS::glm.nb(val ~ Dose, data = d)
    a <- Anova(m, type = 2)
    F_ <- a[1,"LR Chisq"]; p_ <- a[1,"Pr(>Chisq)"]; df2 <- NA
  }
  emm <- emmeans(m, ~ Dose, type = "response")
  ct <- contrast(emm, "trt.vs.ctrl", ref = 1, adjust = "none", type = "response")
  ctd <- as.data.frame(confint(ct)) %>% left_join(as.data.frame(ct)[,c("contrast","p.value")], by = "contrast")
  names(ctd)[names(ctd) %in% c("asymp.LCL")] <- "lower.CL"; names(ctd)[names(ctd) %in% c("asymp.UCL")] <- "upper.CL"
  est_col <- intersect(c("ratio","odds.ratio"), names(ctd))[1]
  list(global = data.frame(system, response = y, model = family_type, n = nrow(d), stat = F_, df2 = df2, p = p_),
       contr = data.frame(system, response = y, model = family_type, contrast = ctd$contrast,
                          est = ctd[[est_col]], lcl = ctd$lower.CL, ucl = ctd$upper.CL, p = ctd$p.value),
       means = { x <- as.data.frame(emm); data.frame(system, response = y, Dose = x$Dose,
                  mean = x[[intersect(c("response","prob"), names(x))[1]]] ) })
}
spec <- data.frame(
  sys = c(rep("in vitro", 11), rep("in vivo", 3)),
  y   = c("Frec","Int","Arb","Vesic","EsporasIntraRad","Entrada","MicelioMM","MicelioS","EsporasMM","EsporasS","MicelioExtraTincion",
          "Frec","Int","Lhif"),
  fam = c("quasibinomial","quasibinomial","quasibinomial","quasibinomial","quasibinomial","quasibinomial","gamma","gamma","nb","nb","quasibinomial",
          "quasibinomial","quasibinomial","gamma"))
res1 <- lapply(seq_len(nrow(spec)), function(i) {
  dat <- if (spec$sys[i] == "in vitro") vitro else vivo
  tryCatch(one_factor(dat, spec$y[i], spec$sys[i], spec$fam[i]),
           error = function(e) { message("FALLO ", spec$y[i], ": ", e$message); NULL })
})
res1 <- res1[!sapply(res1, is.null)]
glob1 <- bind_rows(lapply(res1, `[[`, "global")) %>% group_by(system) %>% mutate(p_BH = p.adjust(p, "BH")) %>% ungroup()
cont1 <- bind_rows(lapply(res1, `[[`, "contr"))
write.csv(glob1, file.path(OUT, "T05_unifactorial_global.csv"), row.names = FALSE)
write.csv(cont1, file.path(OUT, "T06_unifactorial_contrastes.csv"), row.names = FALSE)

# ---- 5. Compartimiento x Cu in vitro (asignacion hifal y de esporas) ---------
# Modelo en escala log: la interaccion Compartimiento x Cu no depende de las unidades
# (cm placa vs cm/g), por eso permite comparar la respuesta relativa de cada compartimiento.
long_h <- vitro %>% filter(Inoc == "GC3") %>%
  select(id, Dose, RC = MicelioMM, HC = MicelioS) %>%
  pivot_longer(c(RC, HC), names_to = "Comp", values_to = "val") %>% filter(!is.na(val))
long_s <- vitro %>% filter(Inoc == "GC3") %>%
  select(id, Dose, RC = EsporasMM, HC = EsporasS) %>%
  pivot_longer(c(RC, HC), names_to = "Comp", values_to = "val") %>% filter(!is.na(val))
long_h$Comp <- factor(long_h$Comp, levels = c("RC","HC")); long_s$Comp <- factor(long_s$Comp, levels = c("RC","HC"))
mh <- glmmTMB(log(val) ~ Comp * Dose + (1 | id), data = long_h, REML = TRUE)
ms <- glmmTMB(val ~ Comp * Dose + (1 | id), data = long_s, family = nbinom2)
ms0 <- update(ms, . ~ Comp + Dose + (1 | id))
mh0 <- glmmTMB(log(val) ~ Comp + Dose + (1 | id), data = long_h, REML = FALSE)
mh_ml <- update(mh, REML = FALSE)
comp_tests <- data.frame(
  response = c("Hifas","Esporas"),
  LRT_chisq = c(2*(logLik(mh_ml)-logLik(mh0)), 2*(logLik(ms)-logLik(ms0))),
  df = 2)
comp_tests$p <- pchisq(comp_tests$LRT_chisq, comp_tests$df, lower.tail = FALSE)
write.csv(comp_tests, file.path(OUT, "T07_compartimiento_x_Cu_LRT.csv"), row.names = FALSE)
ratio_comp <- function(m, is_log) {
  e <- emmeans(m, ~ Dose | Comp, type = if (is_log) "link" else "response")
  ct <- contrast(e, "trt.vs.ctrl", ref = 1, adjust = "none")
  x <- as.data.frame(summary(ct, infer = TRUE))
  lo <- intersect(c("lower.CL","asymp.LCL"), names(x))[1]; up <- intersect(c("upper.CL","asymp.UCL"), names(x))[1]
  if (is_log) { x$ratio <- exp(x$estimate); x$lcl <- exp(x[[lo]]); x$ucl <- exp(x[[up]]) }
  else { x$lcl <- x[[lo]]; x$ucl <- x[[up]] }
  x %>% transmute(contrast, Comp, ratio, lcl, ucl, p = p.value)
}
comp_rat <- bind_rows(mutate(ratio_comp(mh, TRUE), response = "Hifas"),
                      mutate(ratio_comp(ms, FALSE), response = "Esporas"))
write.csv(comp_rat, file.path(OUT, "T08_compartimiento_razones.csv"), row.names = FALSE)


# 5b. Indice de asignacion por placa: log(HC/RC). Equivale exactamente a la interaccion
# Compartimiento x Cu en escala log, pero con inferencia t (mas honesta con n = 3).
alloc <- vitro %>% filter(Inoc == "GC3") %>%
  transmute(id, Dose, hyph = log(MicelioS / MicelioMM), spor = log(EsporasS / EsporasMM))
alloc_fit <- function(v) { d <- alloc[!is.na(alloc[[v]]), c("Dose", v)]; names(d)[2] <- "y"
  m <- lm(y ~ Dose, data = d); a <- anova(m); ct <- contrast(emmeans(m, ~Dose), "trt.vs.ctrl", ref = 1, adjust = "none")
  x <- as.data.frame(summary(ct, infer = TRUE))
  data.frame(var = v, n = nrow(d), F = a[1,"F value"], df1 = a[1,"Df"], df2 = a[2,"Df"], p_global = a[1,"Pr(>F)"],
             contrast = x$contrast, fold = exp(x$estimate), lcl = exp(x$lower.CL), ucl = exp(x$upper.CL), p = x$p.value) }
alloc_tab <- bind_rows(alloc_fit("hyph"), alloc_fit("spor"))
write.csv(alloc_tab, file.path(OUT, "T08b_indice_asignacion_HC_RC.csv"), row.names = FALSE)
# Por compartimiento, log-gaussiano (t con gl residuales), para las razones del texto
comp_lm <- bind_rows(lapply(c("MicelioMM","MicelioS","EsporasMM","EsporasS"), function(v) {
  d <- vitro %>% filter(Inoc == "GC3") %>% transmute(Dose, y = log(.data[[v]])) %>% filter(!is.na(y))
  m <- lm(y ~ Dose, data = d); a <- anova(m); ct <- contrast(emmeans(m, ~Dose), "trt.vs.ctrl", ref = 1, adjust = "none")
  x <- as.data.frame(summary(ct, infer = TRUE))
  data.frame(var = v, n = nrow(d), F = a[1,"F value"], df2 = a[2,"Df"], p_global = a[1,"Pr(>F)"], contrast = x$contrast,
             fold = exp(x$estimate), lcl = exp(x$lower.CL), ucl = exp(x$upper.CL), p = x$p.value) }))
write.csv(comp_lm, file.path(OUT, "T08c_compartimiento_loggauss.csv"), row.names = FALSE)
# Esporas de suelo en Cu2 vs fondo sin inoculo en Cu2
bg <- vitro %>% filter(Dose == "Cu2", !is.na(EsporasS)) %>% group_by(Inoc) %>% summarise(m = mean(EsporasS), de = sd(EsporasS), n = n())
write.csv(bg, file.path(OUT, "T08d_esporas_HC_fondo_Cu2.csv"), row.names = FALSE)

# Sensibilidad: plaza id 15 (Cu de suelo = 94, parece una placa sin Cu) y id 13 (nota "contaminada")
sens_spore <- vitro %>% filter(Inoc == "GC3", !id %in% c(13, 15))
sens_tab <- bind_rows(
  data.frame(escenario = "todas", as.data.frame(pairs(emmeans(glm(EsporasS ~ Dose, data = filter(vitro, Inoc == "GC3"), family = quasipoisson), ~Dose, type = "response"), adjust = "none"))),
  data.frame(escenario = "sin ids 13, 15", as.data.frame(pairs(emmeans(glm(EsporasS ~ Dose, data = sens_spore, family = quasipoisson), ~Dose, type = "response"), adjust = "none"))))
write.csv(sens_tab, file.path(OUT, "T09_sensibilidad_esporas_HC.csv"), row.names = FALSE)

# ---- 6. Analisis de sensibilidad de datos sospechosos (P y Cu in vivo) ------
# 6a. P en tejido: valores 100-191 vs 566-993 (vacio entre 191 y 566).
# Hipotesis a verificar en CNEA: perdida de un cero/exponente al transcribir
# (p.ej. 1.04E3 -> 104). Se compara lo registrado vs lo corregido.
fix10 <- function(x) ifelse(!is.na(x) & x < 200, x * 10, x)
vivo_s <- vivo %>% filter(Dose != "Cu1") %>%
  mutate(PVastago_x10 = fix10(PVastago), PRaiz_x10 = fix10(PRaiz),
         FTP = PVastago / PRaiz, FTP_x10 = PVastago_x10 / PRaiz_x10)
p_sens <- bind_rows(lapply(c("PVastago","PVastago_x10","PRaiz","PRaiz_x10","FTP","FTP_x10"), function(y) {
  r <- fit_factorial(vivo_s, y, "in vivo", c("Cu0","Cu2"))
  r$anova %>% select(response, term, F, df1, df2, p) }))
p_sens_means <- vivo_s %>% group_by(Inoc, Dose) %>%
  summarise(across(c(PVastago, PVastago_x10, PRaiz, PRaiz_x10), list(m = ~mean(.x, na.rm = TRUE), n = ~sum(!is.na(.x)))), .groups = "drop")
write.csv(p_sens, file.path(OUT, "T10_sensibilidad_P_tejido_ANOVA.csv"), row.names = FALSE)
write.csv(p_sens_means, file.path(OUT, "T11_sensibilidad_P_tejido_medias.csv"), row.names = FALSE)

# 6b. Cu suelo: id 34 (128.5 en GC3-Cu0, resto 43-52)
cs_sens <- bind_rows(
  fit_factorial(vivo, "CuSuelo", "todas", c("Cu0","Cu2"))$anova,
  fit_factorial(filter(vivo, id != 34), "CuSuelo", "sin id34", c("Cu0","Cu2"))$anova,
  fit_factorial(filter(vivo, !id %in% c(34, 27)), "CuSuelo", "sin id34,id27", c("Cu0","Cu2"))$anova)
cs_eff <- bind_rows(
  fit_factorial(vivo, "CuSuelo", "todas", c("Cu0","Cu2"))$effects,
  fit_factorial(filter(vivo, id != 34), "CuSuelo", "sin id34", c("Cu0","Cu2"))$effects)
write.csv(cs_sens, file.path(OUT, "T12_sensibilidad_CuSuelo_ANOVA.csv"), row.names = FALSE)
write.csv(cs_eff,  file.path(OUT, "T13_sensibilidad_CuSuelo_efectos.csv"), row.names = FALSE)

# 6c. Permutacion exacta para los contrastes clave (no depende de la familia)
perm_exact <- function(a, b) {
  a <- log(a[!is.na(a)]); b <- log(b[!is.na(b)]); all <- c(a, b)
  obs <- mean(b) - mean(a); idx <- combn(length(all), length(b))
  d <- apply(idx, 2, function(i) mean(all[i]) - mean(all[-i]))
  c(n1 = length(a), n2 = length(b), ratio = exp(obs), p = mean(abs(d) >= abs(obs) - 1e-12), n_perm = ncol(idx))
}
gg <- function(dat, y, i, d) dat[[y]][dat$Inoc == i & dat$Dose == d]
key <- list(
  c("in vivo","gt","GC3/NI @Cu0"), c("in vivo","gt","GC3/NI @Cu1"), c("in vivo","gt","GC3/NI @Cu2"),
  c("in vivo","gfe","GC3/NI @Cu0"), c("in vivo","gfe","GC3/NI @Cu1"), c("in vivo","gfe","GC3/NI @Cu2"),
  c("in vivo","CuSuelo","GC3/NI @Cu2"), c("in vivo","CuRaiz","GC3/NI @Cu2"), c("in vivo","CuSuelo","GC3/NI @Cu0"))
perm <- bind_rows(lapply(key, function(k) {
  dat <- if (k[1] == "in vivo") vivo else vitro; d <- sub(".*@", "", k[3])
  r <- perm_exact(gg(dat, k[2], "NI", d), gg(dat, k[2], "GC3", d))
  data.frame(system = k[1], response = k[2], contrast = k[3], t(r)) }))
perm2 <- bind_rows(lapply(list(c("gt","Cu2 vs Cu0 en GC3"), c("gfe","Cu2 vs Cu0 en GC3")), function(k) {
  r <- perm_exact(gg(vivo, k[1], "GC3", "Cu0"), gg(vivo, k[1], "GC3", "Cu2"))
  data.frame(system = "in vivo", response = k[1], contrast = k[2], t(r)) }))
write.csv(bind_rows(perm, perm2), file.path(OUT, "T14_permutacion_exacta.csv"), row.names = FALSE)

# ---- 7. Balance de masa de Cu in vivo --------------------------------------
xl <- suppressMessages(read_excel(file.path(ROOT, "BD Cobre.xlsx"), sheet = "Tabla general"))
mb <- vivo %>% filter(Dose != "Cu1") %>%
  mutate(soil_kg = (`RAIZ+TIERRA` - `pf raiz`) / 1000,
         Cu_shoot_ug = CuVastago * `p s aereo`,
         Cu_root_ug  = CuRaiz * `p seco raiz`,
         Cu_plant_ug = Cu_shoot_ug + Cu_root_ug,
         Cu_soil_mg  = CuSuelo * soil_kg)
mb_s <- mb %>% group_by(Inoc, Dose) %>%
  summarise(across(c(soil_kg, Cu_shoot_ug, Cu_root_ug, Cu_plant_ug, Cu_soil_mg, CuSuelo),
                   list(m = ~mean(.x, na.rm = TRUE), de = ~sd(.x, na.rm = TRUE), n = ~sum(!is.na(.x)))), .groups = "drop")
write.csv(mb_s, file.path(OUT, "T15_balance_masa_Cu.csv"), row.names = FALSE)
# Cu agregado por maceta segun el texto (420 mL de solucion):
added <- expand.grid(dose = c(150, 300), sal = c("Cu elemental", "CuSO4 anhidro (39.8 % Cu)", "CuSO4.5H2O (25.5 % Cu)"), stringsAsFactors = FALSE)
added$frac <- c(Cu = 1, an = .398, pen = .2545)[match(added$sal, unique(added$sal))]
added$mg_Cu_maceta <- added$dose * 0.42 * added$frac
soil_mean <- mean(mb$soil_kg, na.rm = TRUE)
added$incremento_mg_kg_esperado <- added$mg_Cu_maceta / soil_mean
added$soil_kg_medio <- soil_mean
# Escenario alternativo: la leyenda vieja de las figuras decia "0.320 g SO4Cu / kg suelo"
leg <- data.frame(dose = 320, sal = c("leyenda 0.32 g/kg, CuSO4 anhidro", "leyenda 0.32 g/kg, CuSO4.5H2O"), frac = c(.398, .2545))
leg$mg_Cu_maceta <- 0.32 * soil_mean * leg$frac * 1000; leg$incremento_mg_kg_esperado <- 0.32 * leg$frac * 1000; leg$soil_kg_medio <- soil_mean
added <- bind_rows(added, leg)
write.csv(added, file.path(OUT, "T16_Cu_agregado_esperado.csv"), row.names = FALSE)
# Modelo del contenido de Cu en planta (extraccion real, sensibilidad a dilucion)
mod_up <- fit_factorial(mb, "Cu_plant_ug", "in vivo", c("Cu0","Cu2"))
mod_sh <- fit_factorial(mb, "Cu_shoot_ug", "in vivo", c("Cu0","Cu2"))
write.csv(bind_rows(mod_up$anova, mod_sh$anova), file.path(OUT, "T17_contenido_Cu_planta_ANOVA.csv"), row.names = FALSE)
write.csv(bind_rows(mod_up$effects, mod_sh$effects), file.path(OUT, "T18_contenido_Cu_planta_efectos.csv"), row.names = FALSE)

# ---- 8. Correlaciones: cruda vs intra-tratamiento (residual), con FDR -------
corr_audit <- function(dat, vars, label) {
  X <- as.matrix(dat[, vars]); out <- list(); pairs <- combn(vars, 2)
  # residuos por tratamiento (log): quitan el efecto del diseno
  R <- apply(X, 2, function(v) { lv <- ifelse(v > 0, log(v), NA); lv - ave(lv, interaction(dat$Inoc, dat$Dose), FUN = function(z) mean(z, na.rm = TRUE)) })
  for (j in seq_len(ncol(pairs))) {
    a <- pairs[1,j]; b <- pairs[2,j]
    ok <- complete.cases(X[,a], X[,b]); n <- sum(ok)
    if (n < 5 || sd(X[ok,a]) == 0 || sd(X[ok,b]) == 0) next
    c1 <- suppressWarnings(cor.test(X[ok,a], X[ok,b], method = "spearman", exact = FALSE))
    ok2 <- complete.cases(R[,a], R[,b])
    c2 <- if (sum(ok2) >= 5 && sd(R[ok2,a]) > 0 && sd(R[ok2,b]) > 0) suppressWarnings(cor.test(R[ok2,a], R[ok2,b], method = "spearman", exact = FALSE)) else list(estimate = NA, p.value = NA)
    out[[j]] <- data.frame(set = label, a, b, n, rho_raw = c1$estimate, p_raw = c1$p.value,
                           n_res = sum(ok2), rho_within = c2$estimate, p_within = c2$p.value)
  }
  o <- bind_rows(out); o$p_raw_BH <- p.adjust(o$p_raw, "BH"); o$p_within_BH <- p.adjust(o$p_within, "BH"); o
}
vivo_inoc <- vivo %>% filter(Inoc == "GC3", Dose != "Cu1")
v_vars <- c("CuSuelo","PSuelo","CuRaiz","PRaiz","CuVastago","PVastago","BCRaiz","BCTallo","FT","Frec","Int","Lhif","gt","gfe","adherido","p seco raiz","p s aereo")
vitro_inoc <- vitro %>% filter(Inoc == "GC3", Dose != "Cu1")
vt_vars <- c("Frec","Int","Arb","Vesic","EsporasS","EsporasMM","EsporasIntraRad","MicelioMM","MicelioS","PSuelo","CuSuelo","Fe","Zn","gt","gfe","LargoRaizMM")
ca <- bind_rows(corr_audit(vivo_inoc, v_vars, "in vivo GC3 (Cu0+Cu2)"),
                corr_audit(vitro_inoc, vt_vars, "in vitro GC3 (Cu0+Cu2)"))
write.csv(ca, file.path(OUT, "T19_correlaciones_auditoria.csv"), row.names = FALSE)
ca_sum <- ca %>% group_by(set) %>%
  summarise(pares = n(), n_medio = mean(n), sig_raw_05 = sum(p_raw < .05, na.rm = TRUE),
            esperados_por_azar = 0.05 * n(), sig_raw_BH = sum(p_raw_BH < .05, na.rm = TRUE),
            sig_within_05 = sum(p_within < .05, na.rm = TRUE), sig_within_BH = sum(p_within_BH < .05, na.rm = TRUE), .groups = "drop")
write.csv(ca_sum, file.path(OUT, "T20_correlaciones_resumen.csv"), row.names = FALSE)

# ---- 9. Comprobaciones de integridad ----------------------------------------
# 9a. Denominadores de %F implicitos: ?coinciden con los 50 fragmentos del metodo?
infer_den <- function(p) { if (is.na(p) || p == 0) return(NA)
  for (d in 5:120) { k <- round(p / 100 * d); if (abs(100 * k / d - p) < 0.011) return(d) }; NA }
dn <- D %>% filter(Inoc == "GC3") %>% rowwise() %>% mutate(den_F = infer_den(Frec)) %>% ungroup() %>% select(id, System, Dose, Frec, den_F)
write.csv(dn, file.path(OUT, "T21_denominadores_F.csv"), row.names = FALSE)
# 9b. "adherido" = (raiz+tierra - pf raiz)/pf raiz ?
adh <- vivo %>% mutate(calc = (`RAIZ+TIERRA` - `pf raiz`) / `pf raiz`, dif = adherido - calc) %>%
  select(id, Inoc, Dose, `RAIZ+TIERRA`, `pf raiz`, adherido, calc, dif)
write.csv(adh, file.path(OUT, "T22_adherido_chequeo.csv"), row.names = FALSE)
adh_r <- cor(vivo$adherido, 1 / vivo$`pf raiz`, use = "complete.obs", method = "pearson")
# 9c. Reconstruccion de EE
ee <- vivo %>% filter(!is.na(EE)) %>% mutate(e1 = (CuVastago * `p s aereo`) / CuSuelo,
    e2 = (CuVastago * PStotal) / CuSuelo, e3 = (CuVastago * `p s aereo` + CuRaiz * `p seco raiz`) / CuSuelo) %>%
  select(id, EE, e1, e2, e3)
write.csv(ee, file.path(OUT, "T23_EE_reconstruccion.csv"), row.names = FALSE)
# 9d. P en tejido: ordenados, para ver el vacio
p_sorted <- list(shoot = sort(vivo$PVastago), root = sort(vivo$PRaiz))
# 9e. PERMANOVA + PCA: respuesta multivariada in vivo (sin P, pendiente de verificar)
mv_vars <- c("p seco raiz","p s aereo","gt","gfe","CuSuelo","CuRaiz","CuVastago")
mvd <- vivo %>% filter(Dose != "Cu1") %>% select(id, Inoc, Dose, all_of(mv_vars)) %>% drop_na()
Z <- scale(log(as.matrix(mvd[, mv_vars])))
pm <- adonis2(dist(Z) ~ Inoc * Dose, data = mvd, permutations = 9999, by = "margin")
pm_terms <- adonis2(dist(Z) ~ Inoc + Dose, data = mvd, permutations = 9999, by = "margin")
pc <- prcomp(Z)
write.csv(as.data.frame(pm), file.path(OUT, "T24_PERMANOVA_interaccion.csv"))
write.csv(as.data.frame(pm_terms), file.path(OUT, "T25_PERMANOVA_principales.csv"))
write.csv(cbind(mvd[,c("id","Inoc","Dose")], pc$x[,1:2]), file.path(OUT, "T26_PCA_scores.csv"), row.names = FALSE)
write.csv(pc$rotation[,1:2], file.path(OUT, "T27_PCA_loadings.csv"))
var_exp <- summary(pc)$importance[2,1:2]

# ---- 10. Razon Cu2/Cu0 por sistema (unidades libres) -------------------------
# Solo inoculados. Responde a: "?el ensayo in vitro predice la direccion del efecto in vivo?"
rr_fun <- function(y, label, fam = "gamma") {
  d <- D %>% filter(Inoc == "GC3", Dose %in% c("Cu0","Cu2")) %>%
    transmute(System, Dose = droplevels(Dose), val = .data[[y]]) %>% filter(!is.na(val), val > 0)
  m <- glm(val ~ System * Dose, data = d, family = Gamma(link = "log"))
  e <- emmeans(m, ~ Dose | System, type = "response")
  ct <- contrast(e, "revpairwise", type = "response")
  x <- as.data.frame(confint(ct)); x$p <- as.data.frame(ct)$p.value
  a <- as.data.frame(Anova(m, type = 2, test.statistic = "F"))
  data.frame(label, System = x$System, ratio = x$ratio, lcl = x$lower.CL, ucl = x$upper.CL, p = x$p,
             p_interaction = a["System:Dose","Pr(>F)"], n = nrow(d))
}
# En in vitro "Lhif" = micelio en suelo (compartimiento HC); in vivo = micelio en suelo
rr <- bind_rows(rr_fun("Frec","Root colonisation frequency (%F)"), rr_fun("Int","Colonisation intensity (%I)"),
                rr_fun("Lhif","Soil hyphal length"), rr_fun("gt","T-GRSP"), rr_fun("gfe","EE-GRSP"),
                rr_fun("CuSuelo","Soil Cu (XRF)"), rr_fun("PSuelo","Soil P (XRF)"))
rr_inoc <- D %>% filter(Dose %in% c("Cu0","Cu2")) %>% select(System, Inoc, Dose, gt, gfe) %>%
  pivot_longer(c(gt, gfe), names_to = "y", values_to = "val") %>% group_split(System, Dose, y) %>%
  lapply(function(g) { g <- filter(g, !is.na(val)); m <- glm(val ~ Inoc, data = g, family = Gamma(link = "log"))
    ct <- contrast(emmeans(m, ~Inoc, type = "response"), "revpairwise", type = "response")
    ci <- as.data.frame(confint(ct)); data.frame(System = g$System[1], Dose = g$Dose[1], y = g$y[1], ratio = ci$ratio, lcl = ci$lower.CL, ucl = ci$upper.CL, p = as.data.frame(ct)$p.value) }) %>% bind_rows()
# ?El efecto de inocular sobre GRSP difiere entre sistemas? (interaccion en escala log: libre de unidades)
inoc_sys <- bind_rows(lapply(c("gt","gfe"), function(y) {
  d <- D %>% filter(Dose %in% c("Cu0","Cu2")) %>% transmute(System, Inoc, Dose = droplevels(Dose), val = .data[[y]]) %>% filter(!is.na(val))
  m <- glm(val ~ Inoc * System * Dose, data = d, family = Gamma(link = "log")); a <- as.data.frame(Anova(m, type = 2, test.statistic = "F"))
  data.frame(y, term = rownames(a), F = a$`F value`, df1 = a$Df, p = a$`Pr(>F)`, n = nrow(d)) })) %>% filter(term != "Residuals")
write.csv(inoc_sys, file.path(OUT, "T30_GRSP_inoculo_x_sistema.csv"), row.names = FALSE)
write.csv(rr, file.path(OUT, "T28_razon_Cu2_Cu0_por_sistema.csv"), row.names = FALSE)
write.csv(rr_inoc, file.path(OUT, "T29_razon_GC3_NI_GRSP_por_sistema.csv"), row.names = FALSE)

# ---- 11. Consola: resumen ----------------------------------------------------
options(width = 200, digits = 3, pillar.sigfig = 3)
cat("\n=== ANOVA factorial ===\n"); print(as.data.frame(anova_tab %>% mutate(p = fmt_p(p), p_BH = fmt_p(p_BH))))
cat("\n=== Efectos (razones) ===\n"); print(as.data.frame(eff_tab %>% mutate(p = fmt_p(p), p_BH = fmt_p(p_BH))))
cat("\n=== Diagnosticos ===\n"); print(diag_tab)
cat("\n=== Unifactorial global ===\n"); print(as.data.frame(glob1))
cat("\n=== Unifactorial contrastes ===\n"); print(cont1)
cat("\n=== Compartimiento x Cu ===\n"); print(comp_tests); print(comp_rat)
cat("\n=== Sens esporas HC ===\n"); print(sens_tab)
cat("\n=== P sens ===\n"); print(p_sens); print(as.data.frame(p_sens_means))
cat("\nP vastago ordenado:", p_sorted$shoot, "\nP raiz ordenado:", p_sorted$root, "\n")
cat("\n=== CuSuelo sens ===\n"); print(cs_sens); print(cs_eff)
cat("\n=== Permutacion ===\n"); print(bind_rows(perm, perm2))
cat("\n=== Balance masa ===\n"); print(as.data.frame(mb_s)); print(added)
print(bind_rows(mod_up$anova, mod_sh$anova)); print(bind_rows(mod_up$effects, mod_sh$effects))
cat("\n=== Correlaciones ===\n"); print(as.data.frame(ca_sum))
print(ca %>% filter(p_within_BH < .1 | p_raw_BH < .05) %>% arrange(set, p_raw_BH) %>% as.data.frame() %>% head(40))
cat("\n=== Denominadores F ===\n"); print(as.data.frame(dn))
cat("\n=== Adherido ===\n"); print(as.data.frame(head(adh, 8))); cat("max |dif| =", max(abs(adh$dif), na.rm = TRUE), " r(adherido, 1/pf raiz) =", adh_r, "\n")
cat("\n=== EE ===\n"); print(as.data.frame(ee))
cat("\n=== PERMANOVA ===\n"); print(pm); print(pm_terms); cat("PC var:", var_exp, " n=", nrow(mvd), "\n")
print(round(pc$rotation[,1:2], 2))
cat("\n=== Razon Cu2/Cu0 ===\n"); print(rr); print(rr_inoc)
cat("\nSesion:\n"); print(sessionInfo()$R.version$version.string)
