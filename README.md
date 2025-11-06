# arete <img align="right" width="250" src="man/figures/logo.png">
Automated REtrieval from TExt

[![CRAN status](https://www.r-pkg.org/badges/version/arete)](https://cran.r-project.org/package=arete)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/arete)](https://cran.r-project.org/package=arete)

arete is a Python based pipeline for extraction of species occurrence data through the usage of large language models. Due to the well-known limitations of LLMs, arete comes with validation tools designed to handle model hallucinations, allowing for a scientific, rigorous application of LLM. 
Currently supports usage of GPT with more planned, including local and non-proprietary models.

## Installation
As arete is available from CRAN, you can use `install.packages("arete")` to get the current released version. Alteratively, install the latest release through `remotes::install_github("VascoBranco/arete")`.

We recommend that you install Python before running arete_setup. Usually performed through `sudo apt install python3-venv python3-pip python3-dev` on linux systems or `reticulate::install_python()` for other OS. Some external software might also be needed to successfully install arete's dependencies, including: GDAL, GEOS, PROJ, netcdf, sqlite3, tbb, gfortran, libgmp3-dev, harfbuzz freetype2 fribidi (deb:libharfbuzz-dev libfribidi-dev (Debian, Ubuntu, etc), rpm: harfbuzz-devel fribidi-devel (Fedora, EPEL), brew: harfbuzz fribidi (OSX)). The vast majority of these should already be installed in most Windows installs. An extensive description of all dependencies is planned to be released soon.

## References
 - Atsma, A.J. (no date) ARETE - Greek Goddess or Spirit of Virtue & Valour (Roman Virtus), Theoi. Available at: https://www.theoi.com/Daimon/Arete.html (Accessed: 06 November 2025).
 - Kent, A. et al. (1955). "Machine literature searching VIII. Operational criteria for designing information retrieval systems", *American Documentation*, 6(2), pp. 93–101. doi: 10.1002/asi.5090060209.
 - Levenshtein, V.I. (1966). "Binary codes capable of correcting deletions, insertions, and reversals", *Soviet Physics-Doklady*, 10(8), pp. 707–710 [Translated from Russian].
 - van Rijsbergen, C.J. (1979). "Information Retrieval", Architectural Press. ISBN: 978-0408709293.
