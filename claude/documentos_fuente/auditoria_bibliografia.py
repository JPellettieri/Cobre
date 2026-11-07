"""Genera claude/bibliografia/auditoria_referencias.csv cruzando la lista del borrador con Crossref."""
import csv, json, os, re, sys
sys.stdout.reconfigure(encoding='utf-8')
TMP = r'C:\Users\julip\.claude\jobs\98e45638\tmp'
OUT = os.path.join(os.path.dirname(__file__), '..', 'bibliografia'); os.makedirs(OUT, exist_ok=True)
xr = {r['idx']: r for r in json.load(open(TMP + r'\crossref_out.json', encoding='utf-8'))}
xr2 = json.load(open(TMP + r'\crossref2.json', encoding='utf-8'))
ms = {}
for l in open(TMP + r'\ms.txt', encoding='utf-8'):
    m = re.match(r'\[(\d+)\](?:<\w+>)?\s?(.*)', l.rstrip('\n'))
    if m: ms[int(m.group(1))] = m.group(2)
# Cuerpo donde se buscan las citas: intro a discusion y notas sueltas (sin la lista)
body = ' '.join(v for k, v in ms.items() if (21 <= k <= 358) or (424 <= k <= 445) or k >= 454)
# Notas manuales (verificadas contra Crossref u otra fuente)
N = {
 374: ('Datos OK', 'Sin citar en el cuerpo actual; eliminar o citar.'),
 375: ('OK', ''),
 376: ('OK', ''),
 384: ('OK', ''),
 377: ('Literatura gris', 'Informe de ONG. Solo aparece en párrafo descartado.'),
 378: ('Corregir', 'Páginas de suplemento: 1069S–1073S.'),
 379: ('Literatura gris', 'Informe incompleto: falta institución y URL.'),
 380: ('OK', 'Corregir autor: Bastos de Melo GW.'),
 381: ('OK (sin DOI)', 'Crossref no la indexa; existencia confirmada en sumario de Agrociencia 47(4).'),
 382: ('Corregir', 'Publicada en 2020, 52:145–149. El borrador cita 2019.'),
 383: ('Corregir', 'Autores con errores tipográficos: "Meiera", "Borie b G" → Meier S, Borie G.'),
 385: ('OK', 'El método XRF de suelo no cubre tejido vegetal; hace falta referencia y validación para plantas.'),
 386: ('Corregir', 'El título original está en español; verificar la cita completa.'),
 387: ('OK', 'La búsqueda general falló; verificada con búsqueda dirigida.'),
 388: ('Corregir', 'La revista es Pure Appl Chem 74:793–807, no "Clinical Chem".'),
 389: ('Corregir', 'Autores deformados por gestor ("EF, A., Abeer, H."): Abd_Allah EF, Hashem A, Alqarawi AA et al. No está en Crossref; verificar en la revista (Pak J Bot 47:785–795).'),
 390: ('Corregir', 'Páginas de suplemento: 1098S–1102S.'),
 391: ('OK', ''),
 392: ('Corregir', 'Título: "The role of glomalin..."; pp. 317–323.'),
 393: ('OK', 'Solo sostiene la medición de agregación, que quedó en duda.'),
 394: ('Corregir', '"Rilling" → Rillig; PLoS ONE 6:e28426.'),
 395: ('Corregir', 'Es un libro/comunicación técnica: "Sand and water culture methods..."; Commonwealth Bureau of Horticulture and Plantation Crops.'),
 396: ('OK', ''), 397: ('OK', ''), 398: ('OK', ''),
 399: ('Corregir', 'Páginas 797S–811S. No respalda la definición de "heavy metal" por densidad para la que se cita.'),
 400: ('Corregir', 'Publicada en 2015, Sci Total Environ 505:555–564; "de Lorio" → "de Iorio"; "J Sci Total Environ" es incorrecto.'),
 401: ('OK', ''),
 402: ('Corregir', 'Es Newman 1966, J Appl Ecol 3:139–145. La lista dice 1965 y 2:139–145.'),
 403: ('Corregir', 'Revista: Nat Rev Microbiol.'),
 404: ('OK', 'Muy pertinente para el resultado de compartimientos; estaba sin usar en el texto.'),
 405: ('OK', ''), 406: ('OK', ''),
 407: ('Corregir', '"Rilling" → Rillig.'),
 408: ('OK', ''),
 409: ('Corregir', 'Páginas 1413–1421 (la lista dice 413–1421).'),
 410: ('OK', ''),
 411: ('Corregir', 'Falta el año: 2019.'),
 412: ('OK', ''), 413: ('OK', ''), 414: ('OK', ''),
 415: ('No verificada', 'No aparece en Crossref con esos datos; no se cita. Eliminar.'),
 416: ('Corregir', '"Moutogolis" → Moutoglis. Sin citar en el cuerpo actual.'),
 417: ('OK', 'Sin citar en el cuerpo actual.'),
 418: ('Corregir', 'Título: "...effects of glomalin, mycorrhizal hyphae and roots..."; Sci Rep 4:5823.'),
 419: ('OK', 'Es sobre Cr, no Cu.'), 420: ('OK', 'Es sobre Cr, no Cu.'),
 446: ('No verificada', 'En sección de descartes.'), 447: ('No verificada', 'En sección de descartes.'),
 448: ('Incompleta', 'Marcada INCOMPLETA en el borrador; en sección de descartes.'),
 449: ('OK', 'En sección de descartes.'), 450: ('OK', 'En sección de descartes.'),
 451: ('Corregir', '"Rilling" → Rillig. En sección de descartes.'), 452: ('OK', 'En sección de descartes.'),
}
dirigida = {387: 'Declerck2004', 400: 'Mendoza2015', 416: 'vdH1998', 417: 'WuF2010', 419: 'Wu2015', 420: 'Wu2016'}
def cited(ref):
    m = re.match(r'\s*([A-Za-zÀ-ÿß\-\' ]+?)[ ,]', ref)
    sur = m.group(1).strip().split()[0] if m else ''
    y = re.search(r'(19|20)\d{2}', ref)
    if not sur or not y: return 'no determinable'
    return 'sí' if re.search(re.escape(sur[:5]) + r'[^.;]{0,40}' + y.group(0), body) else 'no'
rows = []
for k in sorted(xr):
    if k >= 1000: continue
    r = xr[k]; est, nota = N.get(k, ('Revisar', ''))
    doi = r.get('doi') if r.get('score', 0) >= .45 else ''
    tit = r.get('title') if doi else ''
    jr = f"{r.get('journal','')} {r.get('volume') or ''}:{r.get('page') or ''} ({r.get('year')})" if doi else ''
    if k in dirigida:
        it = xr2[dirigida[k]][0]; doi = it['DOI']; tit = ' '.join(it.get('title') or []); jr = f"{' '.join(it.get('container-title') or [])} {it.get('volume')}:{it.get('page')}"
    rows.append([ms[k], est, nota, cited(ms[k]), tit, jr, doi])
faltan = [
 ('Adriano 2005', 'No está en la lista', 'La obra es Adriano 2001 (2.ª ed., Springer). La cifra de 5–30 mg/kg no se pudo verificar sin el libro.'),
 ('Baldi et al. 2018', 'No está en la lista', 'Encontrada: Sci Hortic 227:102–111, doi 10.1016/j.scienta.2017.09.010.'),
 ('Liu and Beckerman 2022', 'No está en la lista; poco pertinente', 'Es sobre biosorbentes de cáñamo en aguas residuales (J Environ Chem Eng 10:107494). No respalda fuentes de Cu en suelo; reemplazada.'),
 ('Collins et al. 2014; Wang 2023; Jeremias et al. 2023; Alshammari et al. 2023', 'No identificadas', 'Faltan en la lista y no se pudieron identificar con esos datos. Parecen heredadas de otro texto. Eliminadas.'),
 ('Sytar et al. 2021', 'No identificada', 'No se halló esa cita. Hay Sytar et al. 2016, 2018 y 2022 sobre otros temas. Eliminada.'),
 ('Murillo et al. 1999', 'No está en la lista', 'Encontrada: Sci Total Environ 242:281–292. Usada para encuadre de fitoextracción, que se eliminó.'),
 ('Khalilvand-Behrouzyar et al. 2007', 'No identificada', 'No se halló. Eliminada.'),
 ('Son et al. 2024', 'Solo en comentario', 'Verificada: Front Soil Sci 4:1418072. Incorporada.'),
 ('Nichols and Wright 2006', 'No está en la lista', 'Encontrada: Biol Fertil Soils 43:215–220, pero trata de pools de C y N, no de la afirmación sobre metales. No usada.'),
 ('Statello et al. 2023', 'Año incorrecto', 'Es Statello et al. 2024, Rev Argent Microbiol 56:198–199.'),
 ('Statello et al. 2024 (afirmación sobre GRSP)', 'MALA ATRIBUCIÓN', 'El borrador la cita por "GRSP secuestra iones metálicos en suelos contaminados". El trabajo es una imagen de microPIXE y no mide GRSP. La afirmación corresponde a González-Chávez et al. 2004 y Cornejo et al. 2008.'),
 ('Francis and Ndukwn 2019', 'No identificada', 'Apellido probablemente mal escrito; no se halló. Eliminada.'),
 ('Phillips and Hayman 1970', 'No está en la lista', 'Encontrada: Trans Br Mycol Soc 55:158–161. Incorporada.'),
 ('Ferrol et al. 2009', 'No está en la lista', 'Encontrada: Phytochem Rev 8:551–559. Reemplazada por Ferrol et al. 2016 (revisión más reciente).'),
 ('Enkhtuya et al. 2000', 'No está en la lista', 'Encontrada: Appl Soil Ecol 14:201–211. No usada en esta versión.'),
 ('Linder et al. 1996', 'Autor incompleto', 'Es Linder and Hazegh-Azam 1996; no sustenta la definición de metal pesado.'),
 ('Colpaert and Van-Assche 1993', 'FUERA DE ALCANCE', 'Es sobre ectomicorrizas (lo señaló Alicia): no sirve para HMA. No usar el pasaje copiado de Janeeshma and Puthur 2020.'),
 ('Texto copiado de otros artículos', 'RIESGO DE PLAGIO', 'Las páginas finales del borrador tienen párrafos textuales de González-Chávez 2004, Hua 2009, Wu 2015, Cabral 2010, Janeeshma 2020 y otros. No se usaron; la versión revisada está redactada de nuevo.'),
]
with open(os.path.join(OUT, 'auditoria_referencias.csv'), 'w', newline='', encoding='utf-8-sig') as f:
    w = csv.writer(f)
    w.writerow(['Referencia en el borrador', 'Estado', 'Problema o corrección', 'Citada en el cuerpo', 'Título en Crossref', 'Revista vol:pág (año)', 'DOI'])
    w.writerows(rows)
with open(os.path.join(OUT, 'citas_sin_referencia_o_con_problemas.csv'), 'w', newline='', encoding='utf-8-sig') as f:
    w = csv.writer(f); w.writerow(['Cita en el texto', 'Estado', 'Detalle']); w.writerows(faltan)
from collections import Counter
print(Counter(r[1] for r in rows), Counter(r[3] for r in rows), len(rows))
json.dump({'counts': Counter(r[1] for r in rows), 'cited': Counter(r[3] for r in rows), 'n': len(rows), 'faltan': faltan}, open(TMP + r'\audit_summary.json', 'w', encoding='utf-8'), ensure_ascii=False)
