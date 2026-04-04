# CLAUDE.local.md

This file provides specifications to Claude Code (claude.ai/code) I'd like to observe throughout all my projects.

## Package Structure

The projects generally follow an R project strcture.

- **R/**: Core utility functions exported & used by the package
- **inst/**: Main analysis scripts, iteration experiments, and data files
  - `iter*.r`: Sequential analysis iterations with increasing complexity
  - `function/load_stuff.r`: Central loader that attaches packages, sources R/ functions, and loads saved data
  - `extdata/`: Raw data files (gas readings, weather data)
- **data/**: Processed datasets in RData format only
- **tests/**: Unit tests using testthat framework
- **vignettes/**: Package documentation and examples

## R Style Guide Options

### General policies

-  The language of the files should be English. Finding hungarian comments is an ISSUE usually
-  Readability trumps performance and even functionality. (Tasks are usually quick and handle small objects.)
-  Quick operations should be kept within a file; if a task is long (2+ minutes), then it merits its own R script with a save() or save.image() at the end.
-  The train-of-thought within a project should be kept via naming the scripts (eg. numbering prefixes like 01_intro.r 02_desc_stats.r etc.) or related files (child1.qmd, child2.qmd...)
-  Length of a single file should be kept <500 lines (ideally 400 lines). Use save.image() and load() or source() appropriately.
-  After a long script, the end state or the relevant object should be saved under the data/ folder as an .Rdata or a .rda object denting the file name (eg. end_state_iter8_mixedmod_stuff.RData)
-  Each script should start with a comment briefly explaining what the script does. Then Library calls and helper function source()-ing as appropriate.
-  Be afraid of state change. If a state change occurs, try to give the changed object a different name. Try to identify common needs for objects (eg. data wrangling wise) at the beginning tand try to construct objects which are then used several times throughout the processes. Also don't skimp on simple 'throwaway' objects if some modification is required for a single task and would most likely not be needed elsewhere.
-  Prefer the long format for data; be aware that input data from the database itself may be in the wide format. In those cases validate the transformations.

### Pipe Operators

-  ✅ Use magrittr pipes %>% over native pipes |>
-  Chain operations with pipes when >3 steps or when aesthetically better >=2
-  Break long pipe chains at logical points

### Iteration & Functional Programming

-  ✅ Use for loops over purrr/map functions especially if <3000 iterations are expected
-  Use vectorized operations when possible
-  Aim for pre-allocating vectors/lists in loops

### Naming Conventions - Variables

-  ✅ Use snake_case for all variables
-  Use descriptive variable names (>3 characters)
-  Avoid abbreviations unless well-known

### Naming Conventions - Objects

-  ✅ Prefix data frames / tibbles with dat_
-  ✅ Use chunk-based naming: dat_chunkname_locf if using an object in a single chunk only
-  Prefix models with mod_ or model_
-  Prefix plots with fig_ or plot_
-  Prefix functions with fun_ or no prefix
-  Use lst_ for lists, vec_ for vectors


### Naming Conventions - Functions

-  Use verb_noun pattern for function names
-  Use snake_case for function names, capitalize if function is Vectorized
-  Start with action verbs (get_, create_, calculate_)
-  End with data type if appropriate (_df, _list, _plot)

### Code Organization

-  ✅ Descriptive chunk names in R Markdown, aim for uniqueness (you do this well)
-  Use # for major sections, ## for subsections, ### to keep things organized within a subsection
-  Load all libraries at top of script
-  Define constants/parameters at top after libraries, in ALL_CAPS
-  Use consistent indentation (2 spaces vs 4 spaces)

### Assignment & Operators

-  Use <- for assignment (R standard)
-  Space around operators: x + y not x+y
-  No space before comma, space after: c(1, 2, 3)
-  Always elaborate for if statements eg. if (cond == TRUE) {...

### Line Length & Formatting

-  Maximum 80 characters per line
-  Break long function calls across lines
-  Align parameters in multi-line function calls
-  Use trailing commas in multi-line lists

### Comments & Documentation

-  Use # for inline comments with space after
-  Use #' for roxygen2 documentation
-  Write comments explaining "why" and "what" too. Use copious amounts of comments.
-  Use TODO/FIXME/NOTE for code annotations
-  Document all function parameters and returns

### Error Handling & Defensive Programming

-  Always check for NULL/missing data before operations
-  Use stop() for critical errors, warning() for non-critical, and message() for good-to-know info
-  Validate function inputs at start of function
-  Use meaningful error messages
-  Use try()/tryCatch() for operations that might fail, especially if nested within a loop

## Key considerations

- The main goal is usually to produce 'Reports' from input data.
- 'Reports' mainly consist of text, figures and tables, in a .qmd ecology.
- I prefer a structure where figures and tables are named and referenced in the Report.
- Reports are generated using Quarto from `inst/report/report.qmd`

## When to Refactor Large Files
  - Break files when they exceed ~400-500 lines
  - Split at logical section boundaries (e.g., after Primary Endpoint,
  before ROM analyses)
  - Or when a slow section (eg. model fitting) is attempted.
  - Each child document or script should end with `save.image(file =
  here::here("inst", "report", "state_after_childX.RData"))` or similar
  - Next child document or appropriate script should start with `load(here::here("inst", "report",
   "state_after_childX.RData"))`

## R packages for Ubuntu

Ubuntu Compiled R Package Libraries for Cross-Environment Compatibility:
To enable full statistical analysis capabilities including mixed-effects
modeling and advanced plotting, Ubuntu-compiled R packages are stored at
@/mnt/c/Users/mrkma/OneDrive/DKM/Stats_R/R/_Libraries/_Ubuntu_packages/
and accessed via .libPaths() configuration. This directory contains over
200 compiled R packages including critical dependencies that require
system-level compilation (nloptr, lme4, effects, emmeans, zoo,
RcppArmadillo) which cannot be easily installed in restricted environments
 due to cmake and system library requirements. The Ubuntu packages are
fully compatible across similar Linux environments and can be activated by
 prepending the library path: .libPaths(c('/mnt/c/Users/mrkma/OneDrive/DKM
/Stats_R/R/_Libraries/_Ubuntu_packages', .libPaths())) before loading
packages. This approach enables complete statistical workflows including
lme4::lmer() mixed-effects models, emmeans::emmeans() contrasts,
zoo::na.locf() last-observation-carried-forward imputation, and
effects::predictorEffects() visualization without requiring admin
privileges or system-level package compilation. The compiled libraries
maintain full functionality across different computational environments
while preserving reproducibility and ensuring consistent statistical
analysis capabilities between development scripts (inst/iter1.r,
inst/iter2.r) and production report generation workflows.


## Development Workflow

### Essential Setup

Every analysis script assumes this setup first:
```r
source(here::here("inst", "function", "load_stuff.r"))
```

This single command usually:
- Loads common packages (dplyr, ggplot2, lubridate, nlme, splines, etc.)
- Sources all functions from R/ directory
- Loads preprocessed data from `data/`
- Loads backend models and results from `inst/function/backend/`

The second ubiquitous command is:

```r
source(here::here("inst", "function", "wrangling.r"))
```

Which compiles the database for the project from an external source (if applicable).

### Analysis Architecture

The projects usually follow an iterative exploration pattern:

1. **Data Processing**: Raw gas meter readings and weather data are cleaned and joined
2. **Model Development**: Multiple approaches tested usually.
3. **Reporting**: Quarto reports eg. in `inst/Report_heatneed/`

### Main CLAUDE.md management

The following sections should be generated and updated in CLAUDE.md

1. File roles - list the files in the project with a one-liner explanation of their contents and roles
2. Script Chronology & Current Status - Should be completed in the main CLAUDE.md for each file under @inst/
3. Recent Fixes & Data Management
4. 🔧 Current Architecture Notes

