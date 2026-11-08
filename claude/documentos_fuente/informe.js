// Genera claude/Informe_de_revision.docx
const fs = require('fs'); const path = require('path');
const { Ctx, tableGrid, image, D } = require('./lib.js');
const { Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType, Footer, PageNumber, LevelFormat, ExternalHyperlink } = D;
const ROOT = path.join(__dirname, '..'); const FIG = path.join(ROOT, 'figuras');
const FONT = 'Calibri';
const ctx = new Ctx(FONT, {}, 'Claude');
const body = [];
const add = (...x) => body.push(...x);
const P = t => new Paragraph({ spacing: { after: 110, line: 276 }, children: ctx.runs(t, { size: 22 }) });
const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, children: ctx.runs(t, { size: 30, bold: true, color: '1F3B4D' }) });
const H2 = t => new Paragraph({ heading: HeadingLevel.HEADING_2, children: ctx.runs(t, { size: 24, bold: true, color: '1F3B4D' }) });
const B = t => new Paragraph({ numbering: { reference: 'b', level: 0 }, spacing: { after: 60, line: 268 }, children: ctx.runs(t, { size: 22 }) });
const SP = () => new Paragraph({ spacing: { after: 120 }, children: [] });
const W = 9638;

add(new Paragraph({ spacing: { after: 60 }, children: ctx.runs('Informe de revisión del manuscrito sobre cobre', { size: 38, bold: true, color: '1F3B4D' }) }));
add(new Paragraph({ spacing: { after: 240 }, children: ctx.runs('Acompaña a *Manuscrito_cobre_3.docx*. Fecha: 6 de noviembre de 2026.', { size: 21, color: '555555' }) }));

add(H1('Lo esencial'));
add(P('**El trabajo tiene un resultado publicable, pero no es el que proponía el borrador.** Los datos no sostienen que el Cu aumente la glomalina, ni que GC3 inmovilice Cu, ni que el girasol sea hiperacumulador. Sí muestran algo más original: con Cu, *R. intraradices* GC3 mantiene la colonización, pero su esporulación en suelo sube ×2,8 en la dosis intermedia y cae ×0,06 en la alta, hasta el nivel de fondo. Es un patrón compatible con evitación del metal.'));
add(P('**No enviar todavía.** Hay ocho puntos que requieren revisar planillas, cuadernos o consultar a CNEA (sección 1). El primero es de integridad editorial: los mapas de la "Figure 5" ya están publicados. Varios otros pueden cambiar resultados, en especial el P en tejido.'));
add(P('La versión revisada está redactada de nuevo en inglés. Tiene 35 comentarios de Word dirigidos a ustedes y lo pendiente resaltado en amarillo. Las hipótesis y las decisiones de encuadre son propuestas mías y deben validarlas los autores.'));

add(H1('1. Puntos a resolver antes de enviar'));
add(tableGrid(ctx, ['#', 'Problema', 'Evidencia', 'Qué hacer'], [
  ['1', '**Figura ya publicada**', 'Comparé los mapas de la "Figure 5" con la figura de Statello et al. 2024 (Rev Argent Microbiol 56:198–199): son las mismas imágenes de micelio, vesículas y raíz. Licencia CC BY-NC-ND.', 'No republicarlos como nuevos. La versión revisada los cita y no los incluye. Si son indispensables, pedir permiso y declararlo. Verificar también que la foto elegida como Fig. 1 no esté en Silvani et al. 2019.'],
  ['2', '**P en tejido posiblemente mal transcripto**', 'Vástago y raíz son bimodales: 14 valores de 100–191 y 14 de 566–993, nada entre medio (figura abajo). 100 ppm = 0,01 % de P, implausible en tejido vegetal.', 'Pedir a CNEA los valores crudos. Es solo una hipótesis: que los valores ≥1000 perdieron un cero. Los resultados de P quedaron retenidos.'],
  ['3', '**EE-GRSP > T-GRSP y factores de volumen**', 'In vivo EE = 81–93 y T = 45–56 µg/g, imposible si T incluye EE. Hoja "in vivo eeg gt": ambas lecturas × 50 mL, pero EE se extrae en 8 mL y T en hasta 40 mL. In vitro no se aplicó ese factor (solo abs/pendiente).', 'Rehacer el cálculo desde absorbancias. Mientras tanto, no comparar valores absolutos entre sistemas: la figura "Glomalina VITROvsVIVO" y su contraste no son válidos.'],
  ['4', '**La dosis de Cu no cierra**', 'In vivo: 420 mL × 300 ppm (126 mg) vs "0,320 g/kg" en leyendas (≈420 mg). In vitro: 1,2 mg en 30 mL de suelo daría +40 mg/kg, pero se miden +190; Statello 2024 dice 300 mg/kg. ¿"ppm" es de Cu o de sal?', 'Definir dosis, sal (¿anhidra o pentahidratada?) y masa de suelo. Ver tabla de balance en la sección 2.'],
  ['5', '**"Agregación" no mide agregación**', 'adherido = (raíz + tierra − raíz fresca)/raíz fresca, con ≈1,2 kg de tierra: toda la maceta. r = 0,96 con 1/peso de raíz. El texto dice µg/g y 121–129; la tabla, g/g y 257–540.', 'Buscar el dato real. Si no existe, sacar agregación de objetivos y resultados (ya está fuera de la versión revisada).'],
  ['6', '**Apareamiento de muestras de vástago**', 'Suelo y raíz siguen la numeración 1–4, 6–9, 10–13, 14–16; los vástagos 4V y 9V quedaron en otro tratamiento, y FT y BC combinan números distintos (raíz 6 con vástago 4V).', 'Confirmar si el número identifica la maceta. Si es así, recalcular Cu en vástago, FT y BC.'],
  ['7', '**Tabla de microPIXE no confiable**', 'Raíz con 25 % de Si (¿suelo adherido?), P = 0 en raíces, Cu mayor sin Cu agregado, sin réplicas. El mapa de P en raíz coincide con Si y Fe. Los espectros de la "Figure 4" llegan a 7 keV, sin Cu Kα (8,05 keV); los de "MICRO PIXE con Cu" llegan a 9,5 keV aprox., pero el pico de Cu es apenas superior al ruido.', 'Revisar con Emmanuel, como ya anotaron en el borrador. Excluida de la versión revisada.'],
  ['8', '**Sin control de calidad en XRF de planta**', 'No hay LOD/LOQ, material certificado ni masa de pellet. Cu en raíz: 10–22 mg/kg, valores enteros. Mismo suelo Cu0: 49 mg/kg in vivo vs 100 in vitro; P total 650 vs 410.', 'Pedir a CNEA. Aclarar si es el mismo suelo. Es probable que un revisor lo exija.'],
], [350, 2100, 3900, 3288]));
add(SP());
add(new Paragraph({ alignment: AlignmentType.CENTER, children: [image(path.join(FIG, 'Informe_P_tejido_bimodal.png'), 500, 212, 'png')] }));
add(P('**Por qué importa el punto 2.** Si la hipótesis fuera cierta, el resultado del borrador se invierte. Medias de P en tejido (mg/kg), según planilla → con hipótesis ×10:'));
add(tableGrid(ctx, ['Tratamiento', 'P vástago', 'P raíz', 'Lectura'], [
  ['NI-Cu0', '502 → 814', '242 → 1144', ''],
  ['GC3-Cu0', '306 → 1078', '147 → 1467', 'GC3/NI en vástago: de 0,61 a 1,32'],
  ['NI-Cu2', '554 → 848', '632 → 924', ''],
  ['GC3-Cu2', '405 → 1074', '714 → 714', 'Cu2/Cu0 en raíz de GC3: de ×4,9 a ×0,49'],
], [1800, 1900, 1900, 4038]));
add(SP());
add(P('**Otros datos a corregir:**'));
[
  'Hifas de NI-Cu0 in vitro (ids 1–3) son copia exacta de GC3-Cu0 (62,7; 101; 88,5). El borrador viejo decía 10,8 ± 3,0. Anulé el dato; hay que recuperar el original.',
  'Notas en la planilla: id 13 "estaba contaminada", id 12 "no se midió pastilla?". El id 15 (Cu1) tiene Cu 94 y Zn 58, igual que una placa sin Cu. Sacarlos no cambia el patrón de esporas.',
  'id 34 (GC3-Cu0): Cu en suelo 128,5 frente a 43–52 en el resto de la celda. Sin él, GC3/NI en Cu2 pasa a P = 0,025 con la misma razón (T12–T13).',
  '%F no es compatible con "50 fragmentos": implicaría solo valores pares. Los decimales implican denominadores de 29–43.',
  'Placas sin inóculo muestran hifas (7–15 cm/g) y esporas (7/g), aunque el texto decía que se comprobó la ausencia de esporas (y de Cu, cuando se miden 47–100 mg/kg).',
  'EE (eficiencia de extracción) no se puede reconstruir con ninguna fórmula evidente (T23). No la usé.',
].forEach(t => add(B(t)));
add(P('**Valores del texto y las tablas que no coinciden con la base:**'));
add(tableGrid(ctx, ['Dónde', 'Borrador', 'Base de datos'], [
  ['Tabla 1, GC3-Cu0: vástago / total / razón', '6,08 / 4,56 (total < vástago) / 8,11', '9,37 / 10,12 / 12,5'],
  ['Tabla 1, W/I-Cu1: raíz / vástago / total', '0,47 / 8,62 / 9,09', '0,56 / 8,89 / 9,46'],
  ['Tabla 1, W/I-Cu2: raíz', '0,87', '0,80'],
  ['Tabla 1, agregación W/I-Cu1', '"38.,8 ± 37.7"', '386,8 (hoja "tabla grande")'],
  ['Texto: %F y %I in vivo', '63,7–66,8 y 15,1–17,8', '76,6–79,8 y 17,6–22,0'],
  ['Texto: T y EE in vitro', '58,4–65,3 y 74,5–88,1 ("10 meses")', '20–24 y 3–10 (otra hoja: "glomalina tesina")'],
  ['Figura ERM: etiqueta in vivo Cu0', '89,3 (la barra llega a ≈80)', '80,3'],
  ['Figura ERM: etiqueta in vitro Cu0', '81,9', '84,1 (omite el id 12)'],
  ['Figura GT: etiqueta', '"56."', '56,3'],
  ['Texto: TF de P', '"sin Cu trasloca (TF < 1)"', 'Sentido invertido: TF sin Cu = 2,5–3,7'],
  ['Texto: Cu en raíz (microPIXE)', '63,33 en "no colonizadas"', 'En la tabla, 63,33 es GC3-Cu0'],
], [3700, 3100, 2838]));

add(H1('2. Qué cambió en el análisis y qué dicen los datos'));
add(P('Mantuve su base: GLM Gamma con enlace log y emmeans. Agregué todas las dosis medidas, pruebas F por modelo, razones con IC 95 %, corrección BH, modelos adecuados para porcentajes y conteos, permutación exacta y análisis de sensibilidad. El texto del borrador describía ANOVA/Fisher en INFOSTAT, que no es el análisis actual.'));
add(tableGrid(ctx, ['Afirmación del borrador', '¿Se sostiene?', 'Dato'], [
  ['La GRSP aumenta con el Cu', '**No**', 'Cu2/Cu0 en GC3: T-GRSP 1,20 (0,91–1,59), P = 0,17; EE 1,10 (0,82–1,49). Interacción n.s.'],
  ['Inocular aumenta EE-GRSP in vivo', '**Sí, robusto**', 'GC3/NI 1,45–1,98; P < 0,001, también con BH. Permutación P = 0,029 (el mínimo posible con 4 vs 4).'],
  ['Inocular aumenta T-GRSP con Cu2', 'Parcial', '1,46 (1,11–1,92), P = 0,010. Efecto principal P = 0,009; con BH primaria P = 0,065. Interacción n.s.'],
  ['GC3 produce GRSP in vitro', '**No detectado**', 'GC3/NI ≈ 1. Interacción inóculo × sistema: EE P < 0,001; T P = 0,038. Sugiere que la señal in vivo no es producción fúngica directa.'],
  ['GC3 inmoviliza Cu en suelo', '**No demostrable**', 'Solo hay Cu total. In vivo 1,87 (0,90–3,88), P = 0,087; in vitro, sin efecto. Falta Cu disponible y unido a GRSP.'],
  ['Más Cu en raíz micorrizada', 'Tendencia', '1,58 (1,05–2,36), P = 0,031; permutación P = 0,11.'],
  ['Girasol hiperacumulador / fitoextracción', '**No**', 'Vástago 22–32 mg/kg (umbral 300); BCF < 1; la planta contiene < 0,5 % del Cu del suelo.'],
  ['Cu no afecta colonización', '**Sí**', 'Ambos sistemas (P ≥ 0,13).'],
  ['Cu afecta el micelio y las esporas', '**Más fuerte de lo que se reportaba**', 'Esporas HC: F~2,6~ = 54, P < 0,001. Cu2 = fondo. Se perdía al filtrar Cu != 1.'],
  ['Red de correlaciones', '**No publicable así**', 'In vivo: 136 pares con n ≈ 7; 16 con P < 0,05 (6,8 esperables por azar). Muchos son aritmética (BC contiene Cu en suelo) o efecto del tratamiento. Dentro de tratamiento, con BH, solo sobrevive 1, y es matemático.'],
], [3300, 1500, 4838]));
add(SP());
add(P('**Balance de masa in vivo.** El Cu de suelo sube +31 mg/kg en NI y +100 en GC3 (o +82 usando GC3-Cu0 con el atípico). Inocular no crea Cu, así que el valor depende de la dosis real:'));
add(tableGrid(ctx, ['Lectura de la dosis (masa media de suelo 1,31 kg)', 'mg Cu por maceta', 'Aumento esperado (mg/kg)', 'Coincide con'], [
  ['420 mL × 300 mg/L de Cu', '126', '+96', 'GC3'],
  ['420 mL × 300 mg/L de CuSO₄ anhidro', '50', '+38', 'NI'],
  ['420 mL × 300 mg/L de CuSO₄·5H₂O', '32', '+25', 'NI'],
  ['0,32 g CuSO₄ anhidro/kg (leyenda)', '167', '+127', 'GC3, aprox.'],
  ['0,32 g CuSO₄·5H₂O/kg (leyenda)', '107', '+81', 'GC3, aprox.'],
], [4700, 1500, 1800, 1638]));
add(SP());
add(P('Con las dos lecturas menores, la diferencia GC3 − NI (≈69 mg/kg, unos 85 mg por maceta) superaría todo el Cu agregado. Con las mayores, lo llamativo es que falte Cu en NI. La planta no lo explica (0,3–0,4 mg). Lo más probable es heterogeneidad espacial por muestreo o pérdidas; no es evidencia de inmovilización.'));

add(H1('3. Otros enfoques posibles'));
add(H2('Encuadres para el paper'));
add(tableGrid(ctx, ['Encuadre', 'A favor', 'En contra', 'Mi opinión'], [
  ['**A. Ecotoxicología in vitro–in vivo en suelo levemente contaminado** (nota de la primera página)', 'Es lo que los datos pueden sostener. Destaca esporulación como indicador sensible y evitación del metal.', 'Bajo n; hospedantes distintos.', '**Recomendado**; así está redactada la versión.'],
  ['**B. Seguridad alimentaria** (Julieta y Alicia)', 'Tema actual; EFSA 2023 da marco.', 'Sin Cu en semilla no hay dato comestible.', 'Solo si encuentran las semillas y miden Cu y P (idealmente por ICP).'],
  ['**C. Inmovilización / biorremediación** (borrador)', 'Continuidad con el grupo.', 'No hay fracciones de Cu; el girasol apenas extrae.', 'No, salvo que midan DTPA, BCR y Cu unido a GRSP.'],
  ['**D. Nota corta: esporulación y evitación in vitro**', 'Mensaje claro, para Mycorrhiza o Symbiosis.', 'Requiere repetir con más réplicas y dosis.', 'Buena opción si pueden repetir el ensayo.'],
], [3100, 2500, 2100, 1938]));
add(SP());
add(H2('Enfoques de análisis (ya ejecutados en claude/analisis/, salvo aclaración)'));
[
  '**Asignación entre compartimientos:** índice log(HC/RC) por placa, equivalente a la interacción compartimiento × Cu y libre de unidades. Espórulas: F~2,5~ = 39, P < 0,001. Hifas: tendencia, P = 0,06.',
  '**Comparación in vitro–in vivo por razones:** sustituye al contraste "Medio × Cu" de VivovsVitro.R, que está confundido con hospedante, tiempo, extracción y unidades. Las razones Cu2/Cu0 y GC3/NI son libres de unidades (Fig. 5).',
  '**Contenido además de concentración:** Cu total por planta, para separar captación de dilución. El Cu en planta se duplica con Cu solo en GC3 (1,96; 1,01–3,80), mientras que NI queda igual (1,01).',
  '**Correlaciones intra-tratamiento con FDR**, en lugar de la red cruda (T19–T20). Con estos n, recomiendo dejarlas fuera del paper.',
  '**Multivariado:** PCA y PERMANOVA in vivo sobre 7 variables sin P (n = 13): inóculo R² = 0,20, P = 0,011; Cu R² = 0,15, P = 0,039. Resume sin multiplicar pruebas.',
  '**Modelos según tipo de dato:** cuasi-binomial para porcentajes (Gamma no respeta el límite de 100 %), binomial negativa y log-normal para conteos. Las conclusiones sobre esporas se mantienen, pero los valores P dependen del modelo: reporté el más conservador.',
  '**SEM / lavaan** (planteado en Correlaciones.R): no lo recomiendo con n = 8. Sería aceptable con un ensayo más grande y un modelo definido a priori.',
  '**No ejecutado, requiere mediciones nuevas:** Cu extraíble con DTPA y secuencial BCR, Cu en GRSP purificada, ELISA (MAb32B11) como contraste de Bradford, esporas verdeazuladas (Cornejo 2013), más dosis entre Cu1 y Cu2 y más réplicas.',
].forEach(t => add(B(t)));
add(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 120 }, children: [image(path.join(FIG, 'Informe_PCA_in_vivo.png'), 390, 337, 'png')] }));

add(H1('4. Bibliografía'));
add(P('Crucé con Crossref las 54 referencias de la lista (incluida la sección de descartes) y las citas del texto. También leí los abstracts disponibles de las fuentes clave para comprobar lo que se les atribuye. Detalle: bibliografia/auditoria_referencias.csv y bibliografia/citas_sin_referencia_o_con_problemas.csv. La columna "citada en el cuerpo" es una búsqueda automática aproximada.'));
[
  '**20 referencias con errores de datos.** Por ejemplo: Newman es 1966, 3:139–145 (no 1965, 2); Duffus está en *Pure Appl Chem* (no "Clinical Chem"); "Rilling" es Rillig; Mendoza es 2015; Colombo es 2020; varias de AJCN llevan páginas S.',
  '**Mala atribución:** Statello et al. 2024 se cita por "GRSP secuestra metales", pero es una imagen de microPIXE. Lo había marcado Alicia con "revisar".',
  '**Citas no identificables:** Collins 2014, Wang 2023, Jeremias 2023, Alshammari 2023, Sytar 2021, Khalilvand-Behrouzyar 2007, Francis & Ndukwn 2019. Liu & Beckerman 2022 existe pero trata de biosorbentes en aguas residuales. Eliminadas.',
  '**Riesgo de plagio:** las páginas finales del borrador tienen párrafos textuales de otros artículos. No se reutilizaron, pero conviene borrarlos del documento de trabajo.',
  '**Trabajos propios recientes para citar y evitar superposición:** Statello 2024, Benavidez 2024 (Fungal Biol) y Colombo 2024 (Int J Phytoremediation, también con GC3).',
  '**Sumé referencias verificadas**, entre ellas Pawlowska & Charvat 2004 (ya estaba en la lista, sin usar), Gómez-Gallego 2024, Rosier 2006, Gillespie 2011, Holátko 2021, Driver 2005, van der Ent 2013, EFSA 2023, Meier 2012 y Cáceres-Mago 2025.',
  '**Sin verificar:** Castañón-Silva 2013 existe (sumario de la revista), pero no leí el texto. La cifra de 5–30 mg/kg de Adriano viene del borrador y no pude comprobarla en el libro. No leí textos completos de pago: las atribuciones se basan en abstracts.',
  '**Nomenclatura:** "*R. intraradices*" tiene historia taxonómica confusa (Stockinger et al. 2009) y Glomeromycota se ha reubicado (Spatafora et al. 2016). Conviene dar accesión molecular de GC3 si existe.',
].forEach(t => add(B(t)));

add(H1('5. Redacción, tablas y figuras'));
[
  '**Estructura:** eliminé la metodología duplicada, los extractos y las notas. Orden único en todo el texto: in vitro → in vivo → comparación (pedido de Roxana).',
  '**Terminología:** "heavy metal" → elemento traza; SO4Cu → CuSO₄; ppm → mg kg⁻¹; W/I → NI; TG/EEG → T-GRSP/EE-GRSP; SC y HC → HC; "ROC" unificado; MER/ERM → ERM.',
  '**Tablas:** tres líneas, sin bordes verticales, media ± EE con n declarado, valores P exactos, y recalculadas desde la base. Letras reemplazadas por P y razones.',
  '**Figuras:** 300/600 dpi en TIFF, PNG y PDF. Eliminé las barras enfrentadas (ejes invertidos, sin unidades, con errores de etiqueta). Ahora se ven los datos individuales, medias e IC 95 %, con paleta apta para daltonismo y forma distinta por tratamiento, de modo que también funcionan en blanco y negro. La Fig. 5 es nueva.',
  '**Formato:** numeración de líneas, referencias estilo Springer con DOI, y declaraciones. Adaptar a la revista elegida.',
].forEach(t => add(B(t)));
add(P('**Comentarios del equipo en el borrador:**'));
add(tableGrid(ctx, ['Pedido', 'Qué hice'], [
  ['Cambiar el análisis estadístico (Alicia, 24/04)', 'Reescrito; refleja lo que se hizo en R.'],
  ['Reducir microPIXE y XRF (Alicia, Roxana)', 'Queda una mención con cita. Explicación técnica eliminada.'],
  ['Enfoque de seguridad alimentaria (Julieta, Alicia)', 'Sale la fitoextracción. Mención con EFSA; se profundiza solo si hay semillas (sección 3).'],
  ['Valores de micelio en texto, no en gráfico (Alicia)', 'ERM in vivo va en texto y Tabla 2.'],
  ['Solo translocación; bioacumulación a suplemento (Marina)', 'No lo seguí del todo: BCF y TF van en Tabla 3 porque sostienen la conclusión de exclusión. Es fácil moverlos.'],
  ['"¿El Cu estimula la glomalina y entonces no entra?" (Alicia)', 'Con estos datos, no. Ver sección 2.'],
  ['Valores iniciales de P y Cu (Alicia, Marina)', 'Se reportan los medidos en Cu0 y el suelo; falta el análisis de la pila.'],
  ['Diferencias de especie y de extracción (Julieta)', 'Incorporadas en la Discusión 4.2 y 4.4.'],
  ['Ectomicorrizas en Colpaert (Alicia)', 'Eliminado.'],
  ['Qué foto usar (nota final)', 'Elegí la de Cu1/Cu2; decisión final de ustedes.'],
], [4800, 4838]));

add(H1('6. Revistas posibles'));
add(P('Los factores de impacto varían entre fuentes y deben confirmarse en JCR. Los indico solo como orientación.'));
add(tableGrid(ctx, ['Revista', 'FI aprox.', 'Encaje', 'Comentario'], [
  ['**J Soil Sci Plant Nutr** (Springer)', '3,4–3,6', 'Alto', 'Primera opción: tema y región. Cornejo y colaboradores publican ahí.'],
  ['**Rhizosphere** (Elsevier)', '≈3,9', 'Alto', 'Primera opción alternativa: interacciones raíz-hongo-suelo.'],
  ['**Symbiosis** (Springer)', '≈2,1–2,9', 'Alto', 'Publicó el sistema (Silvani 2019). Opción segura.'],
  ['**Mycorrhiza** (Springer)', '3,3–3,9', 'Medio-alto', 'Ideal para el resultado de esporulación, pero exigirá más réplicas y resolver GRSP.'],
  ['**Int J Phytoremediation** (T&F)', '3,1', 'Medio', 'Los datos no apoyan fitorremediación: solo con encuadre de fitoestabilización.'],
  ['**Annals of Applied Biology** (Wiley)', 'verificar', 'Medio', 'Considerada por ustedes. Excluye trabajos solo in vitro; aquí hay in vivo, así que es elegible.'],
  ['**J Environmental Sciences** (Elsevier)', '5,9–6,1', 'Bajo', 'Considerada por ustedes. Con n = 3–4 y Cu total, no la recomiendo sin mediciones nuevas.'],
], [3200, 900, 1100, 4438]));
add(SP());
add(P('Fuentes: páginas de [Annals of Applied Biology], [Springer J Soil Sci Plant Nutr], [Symbiosis], [Int J Phytoremediation], Journal Metrics para Rhizosphere y Mycorrhiza, y Elsevier/Journal Metrics para J Environ Sci:'.replace(/\[|\]/g, '')));
[
  ['https://link.springer.com/journal/13199/aims-and-scope', 'Symbiosis, aims and scope'],
  ['https://springer.com/journal/42729/aims-and-scope', 'J Soil Sci Plant Nutr, aims and scope'],
  ['https://www.tandfonline.com/journals/bijp20/about-this-journal', 'Int J Phytoremediation'],
  ['https://www.journalmetrics.org/journal/rhizosphere', 'Rhizosphere (Journal Metrics)'],
  ['https://www.journalmetrics.org/journal/mycorrhiza', 'Mycorrhiza (Journal Metrics)'],
  ['https://www.journalmetrics.org/journal/journal-of-environmental-sciences', 'J Environ Sci (Journal Metrics)'],
  ['https://peeref.com/journals/591', 'Annals of Applied Biology (ficha)'],
  ['https://www.elsevier.es/en-revista-revista-argentina-microbiologia-372-articulo-the-micropixe-technique-understand-distribution-S0325754124000415', 'Statello et al. 2024, texto y figura'],
].forEach(([u, t]) => add(new Paragraph({ numbering: { reference: 'b', level: 0 }, spacing: { after: 40 }, children: [new ExternalHyperlink({ link: u, children: [new TextRun({ text: t, font: FONT, size: 20, style: 'Hyperlink', color: '0563C1', underline: {} })] })] })));

add(H1('7. Cómo reproducir'));
add(P('Desde la raíz del proyecto (no modifican sus archivos): Rscript claude/analisis/01_analisis_revisado.R y luego Rscript claude/analisis/02_figuras_y_tablas.R. Las tablas T00–T30 quedan en claude/analisis/tablas/. Si corrigen BD Cobre.xlsx, vuelvan a correrlos; los números del texto del manuscrito hay que actualizarlos a mano.'));

const doc = new Document({
  creator: 'Claude', title: 'Informe de revisión del manuscrito sobre cobre',
  styles: {
    default: { document: { run: { font: FONT, size: 22 } } },
    paragraphStyles: [
      { id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true, run: { font: FONT, size: 30, bold: true, color: '1F3B4D' }, paragraph: { spacing: { before: 320, after: 120 }, keepNext: true, outlineLevel: 0 } },
      { id: 'Heading2', name: 'Heading 2', basedOn: 'Normal', next: 'Normal', quickFormat: true, run: { font: FONT, size: 24, bold: true, color: '1F3B4D' }, paragraph: { spacing: { before: 200, after: 80 }, keepNext: true, outlineLevel: 1 } },
    ],
  },
  numbering: { config: [{ reference: 'b', levels: [{ level: 0, format: LevelFormat.BULLET, text: '•', alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 360, hanging: 240 } } } }] }] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1134, bottom: 1134, left: 1134, right: 1134 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER, children: [new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 18 })] })] }) }, children: body }],
});
Packer.toBuffer(doc).then(b => { const o = path.join(ROOT, 'Informe_de_revision.docx'); fs.writeFileSync(o, b); console.log('OK', o, b.length); });
