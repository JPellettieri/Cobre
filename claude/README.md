# Revisión del manuscrito sobre cobre

Nada de esta carpeta modifica los archivos originales del proyecto.

| Archivo o carpeta | Contenido |
|---|---|
| `Informe_de_revision.docx` | **Empezar por acá.** Problemas a resolver, re-análisis, otros enfoques, bibliografía y revistas. |
| `Manuscrito_cobre_3.docx` | Versión aparte del manuscrito, en inglés, con comentarios para los autores y pendientes en amarillo. **No está lista para enviar.** |
| `Manuscrito_cobre_2_mismas_figuras.docx` | **Mismas figuras y tablas del borrador**, corregidas y con diseño único; texto de los autores con cambios mínimos y sus comentarios. |
| `Cambios_figuras_y_tablas.docx` | Correspondencia con el borrador y lista de lo corregido. |
| `figuras_mismas/` | Figuras de esa versión (TIFF, PNG, PDF). |
| `figuras/` | Figuras en TIFF (600 dpi), PNG y PDF. `Informe_*` son solo para el informe. |
| `bibliografia/` | Auditoría de referencias contra Crossref (CSV). |
| `analisis/` | Scripts de R, resultados (`tablas/`) y tablas del manuscrito (`tablas_manuscrito/`). |
| `documentos_fuente/` | Scripts que generan los .docx. |

## Reproducir el análisis

Desde la raíz del proyecto:

```
Rscript claude/analisis/01_analisis_revisado.R
Rscript claude/analisis/02_figuras_y_tablas.R
Rscript claude/analisis/03_mismas_figuras_y_tablas.R
```

## Regenerar los documentos

Requiere Node y el paquete `docx`, y conviene definir `NODE_PATH` hacia la carpeta `node_modules` donde esté instalado:

```
node claude/documentos_fuente/manuscrito.js
node claude/documentos_fuente/informe.js
node claude/documentos_fuente/manuscrito_mismas.js
node claude/documentos_fuente/cambios_figuras.js
```

`auditoria_bibliografia.py` usó archivos temporales de la sesión; su resultado ya está en `bibliografia/`.
