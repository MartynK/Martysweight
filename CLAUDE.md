# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MartysWeight (v1.0.1) is a personal analytics R package for modeling weight as a function of time and fasting duration using natural splines. The primary deliverable is the splines vignette. Secondary analyses cover fasting studies, sleep correlations, and blood glucose (Libre 2 CGM) data.

- Docs site: https://martynk.github.io/Martysweight/
- GitHub: https://github.com/MartynK/MartysWeight

## Build & Development Commands

```bash
# Build and check the package
R CMD build .
R CMD check MartysWeight_1.0.1.tar.gz

# Using devtools (preferred, per .Rproj config)
Rscript -e "devtools::check()"
Rscript -e "devtools::build()"
Rscript -e "devtools::install()"

# Build vignettes
Rscript -e "devtools::build_vignettes()"

# Render individual vignette
Rscript -e "rmarkdown::render('vignettes/splines_vignette.rmd')"

# Build pkgdown site
Rscript -e "pkgdown::build_site()"

# Generate documentation
Rscript -e "devtools::document()"
```

No tests exist yet (tests/ directory is absent despite testthat in DESCRIPTION).

## Architecture

This is a **script-based analysis package** -- there are no exported R functions (no R/ directory). All analytical work lives under `inst/` as sequential iteration scripts. The package shell (DESCRIPTION, NAMESPACE, vignettes/) exists to leverage R's vignette and pkgdown infrastructure.

### Data Flow

1. Raw data in `inst/extdata/*.xlsx` (weight, fasting, glucose, feeding)
2. Scripts in `inst/spliney/ier1.r` through `iter7.r` develop models iteratively
3. Cross-validation results saved to `inst/spliney/crossval_results/*.rdata`
4. Final results rendered in `vignettes/splines_vignette.rmd`

### Core Modeling Approach

- Natural cubic splines (`splines::ns()`) with knot interval k=13 days
- Rolling 30-day forecast cross-validation to tune hyperparameters (k, d=days_before_last)
- Three model families compared: linear (`lm`), GLS (`nlme::gls`), LME (`nlme::lme`)
- Final model: GLS with optimized k=13, d=9
- "Episodes" represent feeding/fasting cycles as a grouping structure

### Key Packages (beyond DESCRIPTION Imports)

Scripts depend on: `readxl`, `splines`, `nlme` (lme, gls), `effects`, `MuMIn`, `mgcv`, `car`, `ggpubr` -- these are not listed in DESCRIPTION Imports.

## File Roles

### Top-level
- `DESCRIPTION` -- Package metadata; Imports are incomplete (many script dependencies unlisted)
- `NAMESPACE` -- Roxygen-generated, empty (no exports)
- `NEWS.md` -- Changelog: v1.0.0 first release, v1.0.1 improved splines vignette

### vignettes/
- `splines_vignette.rmd` -- **Main publishable output.** Weight modeling with splines, rolling forecasts, model comparison. Publication-ready.
- `fast_study.rmd` -- Fasting study analysis on ~1400 public-domain subjects (linear regression, GLS, effects)

### inst/spliney/ (weight modeling -- main analysis)
- `ier1.r` -- Initial mixed-effects exploration
- `ier2.r` -- Episode-based analysis, k=20
- `iter3.r` -- GAM and LME exploration, k=15
- `iter4.r` -- Rolling forecast optimization begins
- `iter5.r` -- Hyperparameter tuning continuation
- `iter6.r` -- Rolling forecast with linear model
- `iter6b_rolling_forecast_with_gls.r` -- GLS variance modeling variant
- `iter6b_rolling_forecast_with_lme.r` -- LME variant (convergence issues)
- `iter7.r` -- Final optimization results
- `crossval_results/` -- Pre-computed rolling forecast .rdata files (lm, gls, lme)

### inst/extdata/ (raw data)
- `martysweight.xlsx` -- Primary weight dataset (date, mass, fasting duration, episode)
- `fasting.xlsx` -- Structured fasting study data
- `feeding.csv.xlsx` -- Blood glucose / Libre 2 CGM data
- `studtab2.csv` -- Public-domain fasting study (~1400 subjects)

### inst/fast_sleep/ (sleep & fasting correlation)
- `analysis.r`, `fasting.r`, `iter2.r`, `iter3.r` -- Sleep-weight relationship scripts
- `calib.r`, `calib.rmd` -- Scale calibration analysis

### inst/ (other studies)
- `fasttab.r` -- Quick fasting metric tabulation
- `1st_libre/` -- Libre 2 CGM blood glucose analysis
- `Fast_2x24h_OGTT_study/` -- 24-hour fasting + oral glucose tolerance test
- `insul_noodles/` -- Insulin & carb interaction study
- `Fasting_presentation_biostat/` -- Conference presentation (.rmd + .pdf)

## Script Chronology & Current Status

The spliney iteration scripts represent a completed model development arc:
- **ier1-ier2**: Exploratory phase (mixed effects, episode structure)
- **iter3-iter5**: Model selection and hyperparameter search
- **iter6-iter6b**: Rolling forecast cross-validation (linear, GLS, LME variants)
- **iter7**: Final results consolidation (k=13, d=9 optimal)

Rolling forecast results are pre-computed and cached in `crossval_results/`. The optimization was frozen around 2023-11-20. Weight data collection is ongoing (through 2024-09 per v1.0.1).

## Recent Fixes & Data Management

- v1.0.1: Splines vignette improved for readability, data extended to 2024-09
- `martysweight.xlsx` is the actively maintained data file (currently modified per git status)

## Architecture Notes

- No `load_stuff.r` loader exists in this project (unlike the template in CLAUDE.local.md)
- Scripts source packages directly at their top; no central loader pattern
- State is passed between iteration scripts via `save.image()` / `load()` and `.rdata` files
- The `docs/` directory is generated by pkgdown -- do not edit directly
