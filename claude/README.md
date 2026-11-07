# Revisión del manuscrito sobre cobre

Nada de esta carpeta modifica los archivos originales del proyecto.

| Archivo o carpeta | Contenido |
|---|---|
| `Informe_de_revision.docx` | **Empezar por acá.** Problemas a resolver, re-análisis, otros enfoques, bibliografía y revistas. |
| `Manuscrito_cobre_3.docx` | Versión aparte del manuscrito, en inglés, con comentarios para los autores y pendientes en amarillo. **No está lista para enviar.** |
| `figuras/` | Figuras en TIFF (600 dpi), PNG y PDF. `Informe_*` son solo para el informe. |
| `bibliografia/` | Auditoría de referencias contra Crossref (CSV). |
| `analisis/` | Scripts de R, resultados (`tablas/`) y tablas del manuscrito (`tablas_manuscrito/`). |
| `documentos_fuente/` | Scripts que generan los .docx. |

## Reproducir el análisis

Desde la raíz del proyecto:

```
Rscript claude/analisis/01_analisis_revisado.R
Rscript claude/analisis/02_figuras_y_tablas.R
```

## Regenerar los documentos

Requiere Node y el paquete `docx`, y conviene definir `NODE_PATH` hacia la carpeta `node_modules` donde esté instalado:

```
node claude/documentos_fuente/manuscrito.js
node claude/documentos_fuente/informe.js
```

`auditoria_bibliografia.py` usó archivos temporales de la sesión; su resultado ya está en `bibliografia/`.
