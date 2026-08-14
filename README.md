# R for Data Science

🇬🇧 English · 🇪🇸 [Español](LEEME.md)

**Manuel Muñoz Plá**

[![Cite](https://img.shields.io/badge/Cite-BibTeX-009e73)](#how-to-cite)

An introduction to **programming for data science** with R, brought up to
2026 state of the art (`renv`, `targets`, tidyverse, `duckdb`/`duckplyr`,
arrow, tidymodels). The running case study is **music** (Spotify Tracks
Dataset). Sixteen chapters, sister volume to *Python for Data Science*.

This repository holds a **browsable web preview** of the book and the
**reproducible code** behind every result. Each chapter's exercises, their
solutions, and the appendices live in the **complete work** (print, PDF and
EPUB), distributed separately.

> 📘 **Book page** and more of the author's work: [manpla.net/libros/r-ciencia-datos](https://manpla.net/libros/r-ciencia-datos/)

## Contents

```
.
├── src/                  # one reproducible R module per chapter
├── data/                 # the data the code and figures consume
├── binder/               # reproducible environment for mybinder.org (RStudio
│                         #   in the cloud)
├── renv.lock             # exact versions (bit-for-bit reproducibility)
├── .Rprofile, renv/      # auto-bootstraps renv when the project is opened
└── CITATION.cff
```

The web edition does not live here: it is read on manpla.net, linked below.

## Read the book

The web edition —16 chapters, with search and maths— is published on the
author's site:

> https://manpla.net/libros/r-ciencia-datos/

Each chapter is a page of manpla.net, with its own navigation, and is composed
with vector SVG figures rendered with the same pdfLaTeX as the book, so
references and figure/table/listing numbers match the printed text. This
repository holds the code and the data behind it.

## Run the code

Each chapter ships a reproducible module in `src/capNN_*.R`, seeded for
determinism. `renv.lock` pins the exact versions the book was verified
against (chapter 1 covers `renv` in depth):

```r
install.packages("renv")
renv::restore()                   # installs EXACTLY what renv.lock says
```

```bash
Rscript src/cap01_entorno.R
```

Every chapter on the web edition also has a **"Run this chapter in Binder"**
button that opens RStudio in the cloud, with the environment and data already
set up — nothing to install.

## License

© 2026 **Manuel Muñoz Plá**.

| Part | What it is | License |
|---|---|---|
| `src/`, `data/`, `binder/` | Reproducible code, data and infrastructure | [MIT](src/LICENSE) — free to use |
| Book text (web edition on manpla.net) | The work itself | [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/) — read and share with attribution; no commercial use or derivative works |

The **complete work** —with each chapter's exercises, their solutions, and the
appendices— is published in print, PDF and EPUB, all rights reserved.

The text carrying a restrictive license does **not limit use of the code**:
you're free to take the modules in `src/` or the environment in `binder/`
into a project of your own, commercial included, under the terms of the MIT
license.

The music data in `data/` comes from the *Spotify Tracks Dataset* by
maharshipandya (Hugging Face), published under the BSD license (with CC0
mirrors), and is not covered by this work's license.

## How to cite

If you mention or use this work, please cite it like this (GitHub also offers
a "Cite this repository" button, generated from `CITATION.cff`):

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

*Author: Manuel Muñoz Plá.*
