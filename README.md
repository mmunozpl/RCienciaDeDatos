# R para la Ciencia de Datos (edición 2026)

Manual de introducción a la programación para la ciencia de datos con R, del
nivel principiante al universitario avanzado, actualizado al estado del arte de
2026 (`renv`, `targets`, tidyverse, `duckdb`/`duckplyr`, arrow,
tidymodels). El caso transversal es la **música** (Spotify Tracks Dataset).

## Leer el libro en la web

La edición web (16 capítulos, con buscador y matemáticas) se publica con GitHub
Pages desde la carpeta `docs/`:

> https://mmunozpl.github.io/RCienciaDeDatos/

## Ejecutar el código

Cada capítulo tiene un botón **«Ejecutar este capítulo en Binder»** que abre
RStudio en la nube, con el entorno y los datos ya listos. También en local:

```bash
Rscript binder/install.R          # instala los paquetes (una vez)
Rscript src/cap01_entorno.R
```

## Estructura

- `docs/` — sitio web navegable (un HTML por capítulo, figuras SVG, MathJax).
- `src/` — código reproducible por capítulo (R ≥ 4.4).
- `data/` — datos que el código y las figuras consumen.
- `binder/` — entorno reproducible para mybinder.org.

Todo el código es original y determinista (semillas fijas): cualquier cifra del
libro se puede volver a obtener.

## Licencia y copyright

© 2026 **Manuel Muñoz Plá**. Reservados todos los derechos. Queda prohibida, sin
la autorización escrita del titular del *copyright*, la reproducción total o
parcial de esta obra (texto, figuras y código) por cualquier medio o
procedimiento.

Los datos de música de `data/` proceden del *Spotify Tracks Dataset* de
maharshipandya (Hugging Face), publicado bajo licencia BSD (con espejos CC0), y
no están cubiertos por el *copyright* de esta obra.
