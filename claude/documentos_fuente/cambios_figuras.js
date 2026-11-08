// Genera claude/Cambios_figuras_y_tablas.docx: correspondencia borrador -> version y que se corrigio
const fs = require('fs'); const path = require('path');
const { Ctx, tableGrid, D } = require('./lib.js');
const { Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType, Footer, PageNumber, PageOrientation } = D;
const ROOT = path.join(__dirname, '..'); const FONT = 'Calibri';
const ctx = new Ctx(FONT, {}, 'Claude');
const P = t => new Paragraph({ spacing: { after: 110, line: 276 }, children: ctx.runs(t, { size: 22 }) });
const H = t => new Paragraph({ heading: HeadingLevel.HEADING_1, spacing: { before: 260, after: 100 }, keepNext: true, children: ctx.runs(t, { size: 28, bold: true, color: '1F3B4D' }) });
const b = [];
b.push(new Paragraph({ spacing: { after: 80 }, children: ctx.runs('Cambios en figuras y tablas', { size: 36, bold: true, color: '1F3B4D' }) }));
b.push(new Paragraph({ spacing: { after: 200 }, children: ctx.runs('Acompaña a *Manuscrito_cobre_2_mismas_figuras.docx*: mismo conjunto de figuras y tablas del borrador, con valores corregidos y un único diseño.', { size: 21, color: '555555' }) }));
b.push(H('Diseño común'));
b.push(P('Paleta de sus figuras ("Figuras INGLES"): naranja = in vitro, verde = in vivo, gris = W/I; el tono se oscurece con la dosis de Cu (Cu0 claro, Cu1 intermedio, Cu2 oscuro). Cu0 usa su color claro y Cu1 en vivo su verde oscuro. Oscurecí Cu2 y aclaré el gris, porque con sus valores el verde oscuro y el gris eran casi indistinguibles con daltonismo protán. Barras con media ± EE y puntos individuales. Eje y desde cero. Dosis siempre Cu0, Cu1, Cu2. Paneles A, B, C... Numeración por orden de aparición en el texto. Misma fuente, grosores y leyenda abajo. Letras: Tukey entre dosis en GC3. Asteriscos: GC3 vs W/I dentro de cada dosis (como en sus gráficos de R). Tablas: tres líneas, media ± EE y filas de *P*. Términos únicos: T-GRSP, EE-GRSP, HC, W/I, CuSO₄. Como el tono varía con la dosis y la dosis también está en el eje, la figura se lee en blanco y negro y con daltonismo.'));
b.push(H('Figuras'));
b.push(tableGrid(ctx, ['Borrador', 'Esta versión', 'Qué se corrigió'], [
  ['Image 2 (fotos)', 'Fig. 1', 'Sin cambios. Queda pendiente decidir cuál de las dos fotos usar.'],
  ['Fig. 1: hifas y esporas, RC y SC', 'Fig. 2', 'Letras recalculadas. Hifas RC: Cu2 pasa de "b" a "a" (ya no es significativo). Esporas RC: Cu0 pasa de "a" a "ab". SC pasa a HC.'],
  ['Figure 2: colonización in vitro', 'Fig. 3', 'Letras recalculadas: %V y %IS ya no difieren (P = 0,118 y 0,075; antes Fisher).'],
  ['Fig. 3 y gráficos "Frecuencia", "Intensidad", "Largo hifal" in vivo', 'Fig. 4', 'Unificados en una figura con 3 paneles. Títulos en inglés.'],
  ['Fig. 4 (dos versiones), gráficos "Gfe" y "GT"; también GT-INGLES y GFE-INGLES', 'Fig. 5', 'Unificados en una figura con 4 paneles. Conserva los insertos. Corrige "56." y las comparaciones. Valores como figuran en la planilla.'],
  ['Gráfico "adherido"', 'Fig. 6', 'Rediseño; datos dudosos marcados.'],
  ['"Figure 4": espectros', 'Fig. 7', 'Uso los espectros de "MICRO PIXE con Cu", hasta 9,5 keV aprox., que muestran Cu. Los originales llegaban a 7 keV.'],
  ['"Figure 5": mapas', 'Fig. 8', 'Sin cambios. **Ya publicada en Statello et al. 2024: resolver antes de enviar.**'],
  ['Corrplot in vitro', 'Fig. 9', 'Mismos valores, verificados contra el original. Nombres de variables en inglés; negrita para P < 0,05.'],
  ['Gráficos "cobre en suelo / raíz / vástago"', 'Fig. 10', 'Unificados en 3 paneles.'],
  ['Gráficos "bioacumulación vástago / raíz"', 'Fig. 11', 'Unificados en 2 paneles.'],
  ['"Figure 3": translocación', 'Fig. 12', 'Rediseño. En P se usan los valores de la planilla, sin verificar.'],
  ['Corrplot in vivo', 'Fig. 13', 'Ídem Fig. 9. Corrige "-0.00".'],
  ['FREC-, ERM-INGLES', 'Figs. 2–4', 'Sus datos están en esas figuras. Corrige etiquetas: ERM in vivo 89,3 → 80,3; in vitro 81,9 → 84,1.'],
], [3400, 1100, 5138]));
b.push(H('Tablas'));
b.push(tableGrid(ctx, ['Borrador', 'Esta versión', 'Qué se corrigió'], [
  ['Table 1', 'Table 1', 'GC3-Cu0: vástago 6,08 → 9,37; total 4,56 → 10,12; razón 8,11 → 12,5. W/I-Cu1: raíz 0,47 → 0,56; vástago 8,62 → 8,89; total 9,09 → 9,46. W/I-Cu2: raíz 0,87 → 0,80. Razón recalculada en todas las filas. Agregación: promedio de los cocientes por maceta (antes, cociente de promedios) y "38.,8" corregido.'],
  ['Table 2', 'Table 2', 'EE corregidos (P en GC3-Cu0: 10 → 20; en GC3-Cu2: 18,8 → 26,7; Cu en GC3-Cu2: 23,7 → 27,4; en W/I-Cu2: 4 → 5). Decía "W/I-Cu01". Notas en español pasadas a inglés.'],
  ['Table 3', 'Table 3', 'P en raíz W/I-Cu0: 133,7 → 242 (excluía el 566). P en vástago: EE de W/I-Cu0 179 → 208; de GC3-Cu2 254,5 → 294. Cu en suelo GC3-Cu2: EE 27,8 → 32,1. Letras y asteriscos heterogéneos reemplazados por filas de *P*.'],
  ['Dos tablas in vitro al final del borrador', 'Table S1', 'Unidas en una y recalculadas; completa las celdas "±" vacías. %F 91,6 → 91,1; %I 50,6 → 49,6.'],
  ['Table 5', 'Table S2', 'Valores sin cambios: no tengo el dato crudo. Solo formato; marcada como dudosa.'],
  ['Table 4', 'Table S3', 'Misma información. Decimales unificados. Añade *P*.'],
], [3400, 1100, 5138]));
b.push(H('Texto'));
b.push(P('Conservé el texto del borrador, con corrección de estilo y sin cambiar el enfoque. Usé la versión más nueva de Materiales y métodos y actualicé los números que contradecían a las tablas. Reescribí Data analysis para que refleje lo que se hizo en R. Lo que no se sostiene con los datos sigue ahí, en amarillo y con un comentario.'));
b.push(P('**No incluí** (siguen en el original): la metodología vieja, los extractos de otros artículos, el esquema de la discusión y las notas sueltas. Incluí 36 de los 42 comentarios del equipo, en su lugar y con su autor. Faltan 6 porque el texto que comentaban ya no está: tres de Alicia pedían sacarlo o decían que estaba reescrito ("Todo esto lo sacaría", dos veces, y "Todo esto lo escribí de otra forma"), y otros tres estaban sobre extractos y notas no incluidos. Siguen en el original.'));
b.push(H('Valores que no pude corregir'));
b.push(P('P en tejido, T-GRSP y EE-GRSP, agregación y microPIXE se muestran tal como están en la planilla, porque corregirlos requiere datos crudos o consultas (CNEA, cuadernos). Están marcados con comentarios y explicados en *Informe_de_revision.docx*, sección 1.'));
b.push(P('Reproducir: Rscript claude/analisis/03_mismas_figuras_y_tablas.R'));
const doc = new Document({ creator: 'Claude', title: 'Cambios en figuras y tablas',
  styles: { default: { document: { run: { font: FONT, size: 22 } } }, paragraphStyles: [{ id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true, run: { font: FONT, size: 28, bold: true, color: '1F3B4D' }, paragraph: { spacing: { before: 260, after: 100 }, keepNext: true, outlineLevel: 0 } }] },
  sections: [{ properties: { page: { size: { width: 11906, height: 16838 }, margin: { top: 1134, bottom: 1134, left: 1134, right: 1134 } } }, children: b }] });
Packer.toBuffer(doc).then(x => { const o = path.join(ROOT, 'Cambios_figuras_y_tablas.docx'); fs.writeFileSync(o, x); console.log('OK', o, x.length); });
