// Utilidades comunes para generar los .docx (marcado minimo + tablas de tres lineas)
const fs = require('fs');
const path = require('path');
const D = require('docx');
const { Paragraph, TextRun, Table, TableRow, TableCell, WidthType, BorderStyle, AlignmentType, ImageRun,
  CommentRangeStart, CommentRangeEnd, CommentReference, VerticalAlign } = D;

const NOB = { style: BorderStyle.NONE, size: 0, color: 'FFFFFF' };
const LINE = (sz = 8) => ({ style: BorderStyle.SINGLE, size: sz, color: '000000' });

// Marcado: *italica*  **negrita**  ^sup^  ~sub~  ==resaltado==  {{c:KEY}}texto{{/c}}
class Ctx {
  constructor(font, comments, author) {
    this.font = font; this.defs = comments || {}; this.author = author || 'Claude'; this.used = []; this.ids = {};
  }
  commentId(key) {
    if (!(key in this.defs)) throw new Error('Comentario sin definir: ' + key);
    if (key in this.ids) throw new Error('Comentario anclado dos veces: ' + key);
    const id = this.used.length; this.ids[key] = id; this.used.push(key); return id;
  }
  commentsConfig() {
    return {
      children: this.used.map((k, id) => ({
        id, author: this.author, initials: 'CL', date: new Date('2026-11-06T22:00:00Z'),
        children: String(this.defs[k]).split('\n').map(t => new Paragraph({ children: [new TextRun({ text: t, font: this.font, size: 20 })] })),
      })),
    };
  }
  runs(text, base = {}) {
    const out = []; const st = { i: false, b: false, sup: false, sub: false, hl: false }; let buf = '';
    const flush = () => {
      if (!buf) return;
      const o = { text: buf, font: this.font, ...base };
      if (st.i) o.italics = !base.italics; if (st.b) o.bold = true; if (st.sup) o.superScript = true; if (st.sub) o.subScript = true; if (st.hl) o.highlight = 'yellow';
      out.push(new TextRun(o)); buf = '';
    };
    const open = [];
    for (let k = 0; k < text.length;) {
      if (text.startsWith('{{c:', k)) {
        flush(); const e = text.indexOf('}}', k); const key = text.slice(k + 4, e); const id = this.commentId(key);
        out.push(new CommentRangeStart(id)); open.push(id); k = e + 2; continue;
      }
      if (text.startsWith('{{/c}}', k)) {
        flush(); const id = open.pop(); out.push(new CommentRangeEnd(id));
        out.push(new TextRun({ children: [new CommentReference(id)] })); k += 6; continue;
      }
      if (text.startsWith('**', k)) { flush(); st.b = !st.b; k += 2; continue; }
      if (text.startsWith('==', k)) { flush(); st.hl = !st.hl; k += 2; continue; }
      const ch = text[k];
      if (ch === '*') { flush(); st.i = !st.i; k++; continue; }
      if (ch === '^') { flush(); st.sup = !st.sup; k++; continue; }
      if (ch === '~') { flush(); st.sub = !st.sub; k++; continue; }
      buf += ch; k++;
    }
    flush();
    if (open.length) throw new Error('Comentario sin cerrar en: ' + text.slice(0, 60));
    if (st.i || st.b || st.sup || st.sub || st.hl) throw new Error('Marcado sin cerrar en: ' + text.slice(0, 80));
    return out;
  }
}

// Tabla de tres lineas. rows: array de arrays de strings con marcado. widths en DXA.
function table3(ctx, header, rows, widths, opts = {}) {
  const size = opts.size || 17; const firstLeft = opts.leftCols ?? 1;
  const total = widths.reduce((a, b) => a + b, 0);
  const cell = (txt, i, { bold = false, top = NOB, bottom = NOB } = {}) => new TableCell({
    width: { size: widths[i], type: WidthType.DXA }, verticalAlign: VerticalAlign.CENTER,
    margins: { top: 30, bottom: 30, left: 50, right: 50 },
    borders: { top, bottom, left: NOB, right: NOB },
    children: [new Paragraph({
      alignment: i < firstLeft ? AlignmentType.LEFT : AlignmentType.CENTER, spacing: { before: 0, after: 0, line: 240 },
      children: ctx.runs(txt, { size, bold: bold || undefined }),
    })],
  });
  const trs = [new TableRow({ tableHeader: true, children: header.map((h, i) => cell(h, i, { bold: true, top: LINE(10), bottom: LINE(6) })) })];
  rows.forEach((r, ri) => {
    const last = ri === rows.length - 1;
    trs.push(new TableRow({ cantSplit: true, children: r.map((c, i) => cell(c, i, { bottom: last ? LINE(10) : NOB })) }));
  });
  return new Table({ width: { size: total, type: WidthType.DXA }, columnWidths: widths, rows: trs,
    borders: { top: NOB, bottom: NOB, left: NOB, right: NOB, insideHorizontal: NOB, insideVertical: NOB } });
}

// Tabla con grilla suave para el informe (texto largo)
function tableGrid(ctx, header, rows, widths, opts = {}) {
  const size = opts.size || 18; const G = { style: BorderStyle.SINGLE, size: 4, color: 'BFBFBF' };
  const cell = (txt, i, h) => new TableCell({
    width: { size: widths[i], type: WidthType.DXA }, margins: { top: 50, bottom: 50, left: 70, right: 70 },
    borders: { top: G, bottom: G, left: G, right: G }, shading: h ? { type: D.ShadingType.CLEAR, fill: 'E8EEF2', color: 'auto' } : undefined,
    children: String(txt).split('\n').map(t => new Paragraph({ spacing: { before: 0, after: 30, line: 250 }, children: ctx.runs(t, { size, bold: h || undefined }) })),
  });
  const trs = [new TableRow({ tableHeader: true, children: header.map((c, i) => cell(c, i, true)) })];
  rows.forEach(r => trs.push(new TableRow({ children: r.map((c, i) => cell(c, i, false)) })));
  return new Table({ width: { size: widths.reduce((a, b) => a + b, 0), type: WidthType.DXA }, columnWidths: widths, rows: trs });
}

function image(file, widthPx, heightPx, type) {
  return new ImageRun({ type, data: fs.readFileSync(file), transformation: { width: widthPx, height: heightPx } });
}
function csv(file) {
  const txt = fs.readFileSync(file, 'utf8').replace(/^﻿/, ''); const rows = []; let row = [], f = '', q = false;
  for (let i = 0; i < txt.length; i++) {
    const c = txt[i];
    if (q) { if (c === '"') { if (txt[i + 1] === '"') { f += '"'; i++; } else q = false; } else f += c; }
    else if (c === '"') q = true; else if (c === ',') { row.push(f); f = ''; } else if (c === '\n') { row.push(f); rows.push(row); row = []; f = ''; } else if (c !== '\r') f += c;
  }
  if (f || row.length) { row.push(f); rows.push(row); }
  const h = rows.shift(); return rows.map(r => Object.fromEntries(h.map((k, i) => [k, r[i]])));
}
module.exports = { Ctx, table3, tableGrid, image, csv, D };
