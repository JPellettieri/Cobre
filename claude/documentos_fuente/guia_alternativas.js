// Genera "claude/figuras alternativas/Guia_figuras_alternativas.docx"
const fs = require('fs'); const path = require('path');
const { Ctx, tableGrid, image, D } = require('./lib.js');
const { Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType, Footer, PageNumber, LevelFormat, ExternalHyperlink } = D;
const DIR = path.join(__dirname, '..', 'figuras alternativas'); const FONT = 'Calibri';
const ctx = new Ctx(FONT, {}, 'Claude');
const b = [];
const P = t => new Paragraph({ spacing: { after: 110, line: 276 }, children: ctx.runs(t, { size: 22 }) });
const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, children: ctx.runs(t, { size: 30, bold: true, color: '1F3B4D' }) });
const H2 = t => new Paragraph({ heading: HeadingLevel.HEADING_2, children: ctx.runs(t, { size: 24, bold: true, color: '1F3B4D' }) });
const BL = t => new Paragraph({ numbering: { reference: 'b', level: 0 }, spacing: { after: 60, line: 268 }, children: ctx.runs(t, { size: 22 }) });
const IMG = (f, wmm, hmm) => { const w = Math.min(640, Math.round(wmm / 25.4 * 96)); const h = Math.round(w * hmm / wmm);
  return new Paragraph({ alignment: AlignmentType.CENTER, keepNext: true, spacing: { before: 120, after: 80 }, children: [image(path.join(DIR, f + '.png'), w, h, 'png')] }); };

b.push(new Paragraph({ spacing: { after: 60 }, children: ctx.runs('Figuras alternativas', { size: 38, bold: true, color: '1F3B4D' }) }));
b.push(new Paragraph({ spacing: { after: 240 }, children: ctx.runs('Otras formas de mostrar los mismos resultados, con la paleta del equipo. Cada figura está en PNG (300 dpi), PDF y TIFF (600 dpi). Script: *claude/analisis/04_figuras_alternativas.R*.', { size: 21, color: '555555' }) }));

b.push(H1('Qué recomienda la literatura'));
[
  '**Mostrar los datos individuales cuando n es chico.** Con n = 3–4, una barra con su error estándar puede representar distribuciones muy distintas. Los puntos individuales, con o sin media e intervalo, muestran la dispersión real y los valores atípicos (Drummond y Vowler 2011; Weissgerber et al. 2015, 2019).',
  '**Intervalos de confianza en vez de error estándar.** Con n = 3, el IC 95 % es unas 4,3 veces el EE; mostrar solo ±EE hace parecer las diferencias más seguras de lo que son (Krzywinski y Altman 2013; Cumming 2014).',
  '**Mostrar el tamaño del efecto, no solo si es significativo.** Los gráficos de estimación muestran cuánto difieren los tratamientos y con qué incertidumbre. Con muestras chicas, el valor P es muy inestable entre repeticiones (Nakagawa y Cuthill 2007; Halsey et al. 2015; Ho et al. 2019).',
  '**Escala logarítmica para efectos multiplicativos.** Esporas o micelio que se multiplican o dividen por 3 se comparan mejor como razones en escala log, que además no dependen de las unidades (Hedges et al. 1999).',
  '**Graficar las relaciones, no solo el coeficiente.** El mismo r puede salir de patrones muy distintos (Anscombe 1973). Una matriz de correlación con n chico y muchas variables no deja ver de dónde sale cada valor.',
  '**Colores aptos para daltonismo y redundancia.** El color no debería ser la única pista (Wong 2011). Aquí la dosis está en el eje y el tono la repite; GC3 y W/I también se distinguen por la forma de los puntos.',
].forEach(t => b.push(BL(t)));

b.push(H1('Resumen'));
b.push(tableGrid(ctx, ['Figura', 'Reemplaza o complementa', 'Qué mejora', 'Uso sugerido'], [
  ['ALT1', 'Fig. 2 (barras de hifas y esporas in vitro)', 'Muestra cada placa e IC 95 %; la escala log deja ver la caída ×0,06 de esporas en HC', 'Paper'],
  ['ALT2', 'Nueva (combina Fig. 2 A–D)', 'Muestra en un solo panel la inversión entre compartimientos: las esporas del suelo colapsan en Cu2 mientras las de raíz no bajan', 'Paper (es el resultado más claro)'],
  ['ALT3', 'Fig. 5 A–B (glomalina in vivo)', 'Datos y efecto (GC3/W/I con IC) en la misma figura, sin asteriscos', 'Paper'],
  ['ALT4', 'Tablas 1 y 3, Figs. 7–12', 'Todos los efectos in vivo en una sola figura, como razones con IC', 'Paper o resumen gráfico'],
  ['ALT5', 'Figuras "INGLES" con barras enfrentadas', 'Compara in vitro e in vivo sin mezclar unidades', 'Paper, si se mantiene la comparación'],
  ['ALT6', 'Nueva', 'Balance de Cu por maceta: la planta tiene 0,2–0,5 % del Cu del suelo', 'Suplemento'],
  ['ALT7', 'Corrplots (Figs. 9 y 13)', 'Muestra si una correlación es real o solo refleja tratamientos', 'Suplemento o discusión'],
  ['ALT8', 'Fig. 3 (colonización in vitro)', 'Más compacta; muestra placas e IC', 'Paper'],
], [800, 2500, 4300, 2038]));

const F = [
  ['ALT1_puntos_IC_hongo_in_vitro', 174, 62, 'ALT1. Puntos con media e IC 95 % (escala log)',
   'Cada círculo es una placa; el rombo y la línea, la media geométrica con IC 95 %. Los intervalos muy largos (p. ej. esporas RC en Cu0, n = 2) son la incertidumbre real con tan pocas placas, que las barras de EE ocultaban. La escala log hace visible la caída de esporas en HC con Cu2 (de ~140 a ~9 por gramo), que en barras queda casi en cero.'],
  ['ALT2_respuesta_relativa_por_compartimiento', 140, 80, 'ALT2. Respuesta relativa de cada compartimiento',
   'Cada compartimiento se expresa respecto de su propio Cu0, así se pueden comparar aunque las unidades sean distintas. La figura muestra el hallazgo central de una sola vez. En HC (suelo con Cu), las esporas suben ×2,8 con Cu1 y caen a ×0,06 con Cu2. En RC (sin Cu), siguen el camino inverso: bajan con Cu1 y no disminuyen con Cu2. Es compatible con evitación del metal (Pawlowska y Charvat 2004).'],
  ['ALT3_estimacion_GRSP_in_vivo', 174, 110, 'ALT3. Gráfico de estimación para glomalina in vivo',
   'Arriba, todos los datos con media e IC; abajo, la razón GC3/W/I con IC 95 % para cada dosis (Ho et al. 2019). Se ve de un vistazo que EE-GRSP aumenta con la inoculación en todas las dosis (×1,5 a ×2,0) y que T-GRSP lo hace sobre todo en Cu2 (×1,46). Reemplaza los asteriscos por un tamaño de efecto interpretable.'],
  ['ALT4_forest_efectos_in_vivo', 174, 120, 'ALT4. Todos los efectos in vivo en una figura',
   'Izquierda: efecto de inocular en Cu2. Derecha: efecto del Cu dentro de GC3 y de W/I. Círculos llenos: P < 0,05 (sin corregir por multiplicidad). Resume en una figura lo que hoy está repartido en dos tablas y seis figuras: la planta no responde, la glomalina responde a la inoculación y no al Cu, y en cobre los efectos son pocos e inciertos.'],
  ['ALT5_in_vitro_vs_in_vivo', 174, 75, 'ALT5. In vitro frente a in vivo en unidades comparables',
   'Las barras enfrentadas comparaban valores absolutos de dos sistemas con unidades y métodos distintos. Aquí se comparan razones: la dirección del efecto del Cu coincide en colonización y micelio, pero la inoculación aumenta la glomalina solo in vivo.'],
  ['ALT6_balance_de_cobre', 110, 80, 'ALT6. Balance de Cu por maceta',
   'Cu total en el suelo de la maceta frente al Cu en la planta entera (escala log). La planta contiene entre 0,2 y 0,5 % del Cu del suelo, lo que sostiene en una imagen que no hay fitoextracción. También deja ver el valor atípico de GC3-Cu0 (id 34).'],
  ['ALT7_relaciones_clave', 130, 115, 'ALT7. Relaciones clave, en dispersión',
   'Reemplaza la matriz por las relaciones que importan. B: el r = 0,85 entre %F y EE-GRSP solo separa inoculadas de no inoculadas: dentro de GC3 la relación desaparece (r = 0,08; P = 0,80). A: dentro de GC3, con todas las dosis, EE-GRSP no se relaciona con el micelio (r = −0,24), aunque la matriz mostraba 0,69 con Cu0 y Cu2. C y D: las relaciones con Cu en suelo se apoyan en pocos puntos extremos (en C, Spearman ρ = 0,41; P = 0,13). Paneles: A solo GC3, todas las dosis; B todas las macetas; C y D, Cu0 y Cu2.'],
  ['ALT8_perfil_colonizacion_in_vitro', 110, 75, 'ALT8. Colonización in vitro en un solo panel',
   'Las cinco variables de colonización, con cada placa y su IC 95 %. Ocupa la mitad que la Fig. 3 y deja ver que las tendencias en vesículas y esporas intrarradicales se apoyan en dos o tres placas con mucha variación.'],
];
b.push(H1('Las figuras'));
F.forEach(([f, w, h, t, txt]) => { b.push(H2(t)); b.push(IMG(f, w, h)); b.push(P(txt)); });

b.push(H1('Referencias'));
const R = [
  ['Anscombe FJ (1973) Graphs in statistical analysis. Am Stat 27:17–21', 'https://doi.org/10.1080/00031305.1973.10478966'],
  ['Cumming G (2014) The new statistics: why and how. Psychol Sci 25:7–29', 'https://doi.org/10.1177/0956797613504966'],
  ['Drummond GB, Vowler SL (2011) Show the data, don\'t conceal them. Exp Physiol 96:483–485', 'https://doi.org/10.1113/expphysiol.2011.057323'],
  ['Halsey LG, Curran-Everett D, Vowler SL, Drummond GB (2015) The fickle P value generates irreproducible results. Nat Methods 12:179–185', 'https://doi.org/10.1038/nmeth.3288'],
  ['Hedges LV, Gurevitch J, Curtis PS (1999) The meta-analysis of response ratios in experimental ecology. Ecology 80:1150–1156', 'https://doi.org/10.1890/0012-9658(1999)080[1150:TMAORR]2.0.CO;2'],
  ['Ho J, Tumkaya T, Aryal S, Choi H, Claridge-Chang A (2019) Moving beyond P values: data analysis with estimation graphics. Nat Methods 16:565–566', 'https://doi.org/10.1038/s41592-019-0470-3'],
  ['Krzywinski M, Altman N (2013) Error bars. Nat Methods 10:921–922', 'https://doi.org/10.1038/nmeth.2659'],
  ['Nakagawa S, Cuthill IC (2007) Effect size, confidence interval and statistical significance: a practical guide for biologists. Biol Rev 82:591–605', 'https://doi.org/10.1111/j.1469-185X.2007.00027.x'],
  ['Pawlowska TE, Charvat I (2004) Heavy-metal stress and developmental patterns of arbuscular mycorrhizal fungi. Appl Environ Microbiol 70:6643–6649', 'https://doi.org/10.1128/AEM.70.11.6643-6649.2004'],
  ['Weissgerber TL, Milic NM, Winham SJ, Garovic VD (2015) Beyond bar and line graphs: time for a new data presentation paradigm. PLoS Biol 13:e1002128', 'https://doi.org/10.1371/journal.pbio.1002128'],
  ['Weissgerber TL, Winham SJ, Heinzen EP, et al (2019) Reveal, don\'t conceal: transforming data visualization to improve transparency. Circulation 140:1506–1518', 'https://doi.org/10.1161/CIRCULATIONAHA.118.037777'],
  ['Wong B (2011) Points of view: Color blindness. Nat Methods 8:441', 'https://doi.org/10.1038/nmeth.1618'],
];
R.forEach(([t, u]) => b.push(new Paragraph({ spacing: { after: 70 }, indent: { left: 360, hanging: 360 }, children: [new TextRun({ text: t + '. ', font: FONT, size: 20 }), new ExternalHyperlink({ link: u, children: [new TextRun({ text: u, font: FONT, size: 20, color: '0563C1', underline: {} })] })] })));

const doc = new Document({ creator: 'Claude', title: 'Figuras alternativas',
  styles: { default: { document: { run: { font: FONT, size: 22 } } }, paragraphStyles: [
    { id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true, run: { font: FONT, size: 30, bold: true, color: '1F3B4D' }, paragraph: { spacing: { before: 300, after: 120 }, keepNext: true, outlineLevel: 0 } },
    { id: 'Heading2', name: 'Heading 2', basedOn: 'Normal', next: 'Normal', quickFormat: true, run: { font: FONT, size: 24, bold: true, color: '1F3B4D' }, paragraph: { spacing: { before: 240, after: 60 }, keepNext: true, outlineLevel: 1 } }] },
  numbering: { config: [{ reference: 'b', levels: [{ level: 0, format: LevelFormat.BULLET, text: '•', alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 360, hanging: 240 } } } }] }] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1134, bottom: 1134, left: 1134, right: 1134 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER, children: [new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 18 })] })] }) }, children: b }] });
Packer.toBuffer(doc).then(x => { const o = path.join(DIR, 'Guia_figuras_alternativas.docx'); fs.writeFileSync(o, x); console.log('OK', o, x.length); });
