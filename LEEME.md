# R para la Ciencia de Datos

🇬🇧 [English](README.md) · 🇪🇸 Español

**Manuel Muñoz Plá**

[![Cite](https://img.shields.io/badge/Cite-BibTeX-009e73)](#cómo-citar)

Manual de introducción a la **programación para la ciencia de datos** con R,
actualizado al estado del arte de 2026 (`renv`, `targets`, tidyverse,
`duckdb`/`duckplyr`, arrow, tidymodels). El caso transversal es la
**música** (Spotify Tracks Dataset).
Dieciséis capítulos, volumen hermano de *Python para la Ciencia de Datos*.

Este repositorio reúne una **vista previa web navegable** del libro y el
**código reproducible** que genera cada resultado. Los ejercicios de cada
capítulo, sus soluciones y los apéndices están en la **obra completa** (papel,
PDF y EPUB), que se distribuye por separado.

> 📘 **Ficha del libro** y más obras del autor: [manpla.net/libros/r-ciencia-datos](https://manpla.net/libros/r-ciencia-datos/)

## Contenido

```
.
├── src/                  # un módulo R reproducible por capítulo
├── data/                 # los datos que el código y las figuras consumen
├── binder/               # entorno reproducible para mybinder.org (RStudio en
│                         #   la nube)
├── renv.lock             # versiones exactas (reproducibilidad bit a bit)
├── .Rprofile, renv/      # arranque automático de renv al abrir el proyecto
└── CITATION.cff
```

La edición web no vive aquí: se lee en manpla.net, enlazada más abajo.

## Leer el libro

La edición web —16 capítulos, con buscador y matemáticas— se publica en el
sitio del autor:

> https://manpla.net/libros/r-ciencia-datos/

Cada capítulo es una página de manpla.net, con su propia navegación, y se
compone con figuras vectoriales SVG renderizadas con el mismo pdfLaTeX del
libro, de modo que las referencias y los números de figura, tabla y listado
son los del texto impreso. Este repositorio guarda el código y los datos que
la respaldan.

## Ejecutar el código

Cada capítulo trae un módulo reproducible en `src/capNN_*.R`, determinista
con semilla fija. El `renv.lock` fija las versiones exactas con las que se
verificó el libro (cap. 1 explica `renv` en detalle):

```r
install.packages("renv")
renv::restore()                   # instala EXACTAMENTE lo que dice renv.lock
```

```bash
Rscript src/cap01_entorno.R
```

Además, cada capítulo de la web trae un botón **«Ejecutar este capítulo en
Binder»** que abre RStudio en la nube, con el entorno y los datos ya listos,
sin instalar nada.

## Licencia

© 2026 **Manuel Muñoz Plá**.

| Parte | Qué es | Licencia |
|---|---|---|
| `src/`, `data/`, `binder/` | Código reproducible, datos e infraestructura | [MIT](src/LICENSE) — uso libre |
| Texto del libro (edición web en manpla.net) | La obra | [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/deed.es) — leer y compartir con atribución; sin uso comercial ni obras derivadas |

La **obra completa** —con los ejercicios de cada capítulo, sus soluciones y
los apéndices— se publica en papel, PDF y EPUB con todos los derechos
reservados.

Que el texto lleve una licencia restrictiva **no limita el uso del código**:
puedes llevarte los módulos de `src/` o el entorno de `binder/` a un
proyecto propio, incluso comercial, en los términos de la licencia MIT.

Los datos de música de `data/` proceden del *Spotify Tracks Dataset* de
maharshipandya (Hugging Face), publicado bajo licencia BSD (con espejos CC0), y
no están cubiertos por la licencia de esta obra.

## Cómo citar

Si mencionas o usas esta obra, cítala así (GitHub también ofrece el botón
«Cite this repository», generado desde `CITATION.cff`):

```bibtex
@book{munozpla2026rcienciadedatos,
  author    = {Muñoz Plá, Manuel},
  title     = {R para la Ciencia de Datos},
  year      = {2026},
  publisher = {qWORD.dev},
  url       = {https://manpla.net/libros/r-ciencia-datos/}
}
```

---

*Autor: Manuel Muñoz Plá.*
