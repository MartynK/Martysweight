# port_anthropometrics.r -- Port the "Anthropometrics" sheet of the personal
# filing workbook into a tidy, machine-readable Excel file.
#
# The source sheet is TRANSPOSED (metrics in rows, observation dates across
# columns) and stacks two people on top of each other in one sheet. This
# script:
#   1. reads both person-blocks,
#   2. transposes them so one ROW = one observation date,
#   3. translates the Hungarian metric labels to the English names already
#      used by iter2.r,
#   4. writes one WORKSHEET PER PERSON, and
#   5. re-creates the derived-metric EQUATIONS as live Excel formulas
#      (BMI, Jackson/Pollock, Mod.JP3, YMCA, US.Navy, FFMI, ...).
#
# Raw (non-derived) measurements are copied as values. Derived cells get a
# formula wherever all of its inputs are present; where an input is missing
# the original stored value is kept, so nothing is lost.
#
# Output: inst/extdata/anthropometrics_tidy.xlsx
#
# NOTE: the pre-existing inst/extdata/anthropometrics.xlsx (raw transposed
# dump) is left untouched -- iter2.r still reads that one.

library(openxlsx)
library(here)

# ---- Constants ----

SRC_PATH  <- "C:/Users/mrkma/OneDrive/DKM/filing_new-MartysPC.xlsx"
SRC_SHEET <- "Anthropometrics"
OUT_PATH  <- here::here("inst", "extdata", "anthropometrics_tidy.xlsx")

XL_ORIGIN   <- "1899-12-30"   # Excel serial-date origin (Windows)
HEADER_ROW  <- 7L             # row of the column headers in the OUTPUT sheets
FIRST_DATA  <- HEADER_ROW + 1L

# Baseline date for the delta_* columns (source used column BZ = 2024-01-17,
# even though its label says 2024.01.15).
DELTA_BASE_DATE <- as.Date("2024-01-17")

# ---- Read the source sheet ------------------------------------------------
#
# The live workbook sits in OneDrive and is usually open in Excel, which
# holds a lock that base R's file.copy()/file() refuse ("Permission denied").
# PowerShell's Copy-Item opens the handle with FILE_SHARE_READ and gets
# through, so fall back to that.

copy_locked_file <- function(src, dest) {
  if (file.copy(src, dest, overwrite = TRUE) == TRUE) {
    return(TRUE)
  }
  src_win  <- normalizePath(src,  winslash = "\\", mustWork = TRUE)
  dest_win <- normalizePath(dest, winslash = "\\", mustWork = FALSE)
  cmd <- sprintf("Copy-Item -LiteralPath '%s' -Destination '%s' -Force",
                 src_win, dest_win)
  system2("powershell", c("-NoProfile", "-Command", shQuote(cmd)),
          stdout = FALSE, stderr = FALSE)
  file.exists(dest)
}

tmp_src <- file.path(tempdir(), "anthro_source.xlsx")
if (copy_locked_file(SRC_PATH, tmp_src) == FALSE) {
  stop("Could not copy the source workbook: ", SRC_PATH)
}

raw <- read.xlsx(tmp_src, sheet = SRC_SHEET, colNames = FALSE,
                 skipEmptyRows = FALSE, skipEmptyCols = FALSE)

# Pull one source row as a numeric vector over the given source columns.
get_row <- function(row_idx, cols) {
  suppressWarnings(as.numeric(unlist(raw[row_idx, cols], use.names = FALSE)))
}

# Pull one source row keeping whatever type it has (for junk text like "54%?").
get_row_raw <- function(row_idx, cols) {
  unlist(raw[row_idx, cols], use.names = FALSE)
}

# ---- Person-block definitions --------------------------------------------
#
# `rows` maps OUTPUT column name -> source row index.
# `derived` lists the output columns that get a live formula.

# Person 1: "Marci", source rows 1-42.
p1 <- list(
  sheet      = "Marci",
  name       = as.character(raw[1, 3]),
  birth_date = as.Date(as.numeric(raw[2, 3]), origin = XL_ORIGIN),
  sex        = as.numeric(raw[3, 3]),
  height_cm  = as.numeric(raw[4, 3]),
  date_row   = 7L,
  rows = c(
    age_years        = 8,
    body_mass        = 9,
    abdomen_narrow   = 10,
    waist            = 11,
    hip              = 12,
    neck             = 13,
    mid_thigh        = 14,
    forearm          = 15,
    sf_abdomen       = 16,
    sf_chest         = 17,
    sf_thigh         = 18,
    sf_biceps        = 19,
    sf_suprailiac    = 20,
    sf_calf          = 21,
    sf_triceps       = 22,
    sf_lower_back    = 23,
    sf_subscapular   = 24,
    sf_midaxillary   = 25,
    bmi              = 27,
    bf_navy          = 28,
    bf_jp3           = 29,
    bf_mod_jp3       = 30,
    bf_ymca          = 31,
    fat_mass         = 32,
    ffm              = 33,
    ffmi             = 34,
    ffmi_std         = 35,
    delta_ffm        = 36,
    delta_fat_mass   = 37
  )
)

# Person 2: unnamed in the source (female), source rows 52-86.
p2 <- list(
  sheet      = "Person_2",
  name       = as.character(raw[52, 2]),
  birth_date = as.Date(as.numeric(raw[53, 2]), origin = XL_ORIGIN),
  sex        = as.numeric(raw[54, 2]),
  height_cm  = as.numeric(raw[55, 2]),
  date_row   = 58L,
  rows = c(
    age_years           = 59,
    body_mass           = 60,
    waist               = 61,
    belly               = 62,
    hip                 = 63,
    neck                = 64,
    sf_chest            = 65,
    sf_abdomen          = 66,
    sf_biceps           = 67,
    sf_triceps          = 68,
    sf_subscapular      = 69,
    sf_midaxillary      = 70,
    sf_suprailiac       = 71,
    sf_lower_back       = 72,
    sf_thigh            = 73,
    sf_calf             = 74,
    bmi                 = 76,
    bf_navy             = 77,
    bf_durnin_womersley = 78,
    bf_parillo          = 79,
    bf_jp3              = 80,
    bf_jp4              = 81,
    bf_jp7              = 82,
    fat_mass            = 83,
    ffm                 = 84
  )
)

# ---- Assemble the tidy (transposed) data frame for one person -------------

build_person <- function(pp) {
  # Source data columns = every column of the DATUM row holding a plausible
  # Excel date serial. This also drops the waist-to-height reference table
  # that sits far to the right of the real data.
  date_serial <- suppressWarnings(
    as.numeric(unlist(raw[pp$date_row, ], use.names = FALSE))
  )
  cols <- which(!is.na(date_serial) & date_serial > 30000 & date_serial < 60000)

  out <- data.frame(date = as.Date(date_serial[cols], origin = XL_ORIGIN))
  for (nm in names(pp$rows)) {
    out[[nm]] <- get_row(pp$rows[[nm]], cols)
  }

  # Carry non-numeric junk through verbatim so nothing silently disappears.
  for (nm in names(pp$rows)) {
    verbatim <- get_row_raw(pp$rows[[nm]], cols)
    lost <- is.na(out[[nm]]) & !is.na(verbatim)
    if (any(lost)) {
      message("  non-numeric kept as text in '", nm, "': ",
              paste(unique(verbatim[lost]), collapse = ", "))
    }
  }

  out[order(out$date), ]
}

dat_p1 <- build_person(p1)
dat_p2 <- build_person(p2)

message("Person 1 '", p1$name, "': ", nrow(dat_p1), " observations")
message("Person 2 '", p2$name, "': ", nrow(dat_p2), " observations")

# ---- Formula helpers ------------------------------------------------------

# A1-style column letter from a 1-based index (handles > 26).
col_letter <- function(i) {
  res <- ""
  while (i > 0) {
    r   <- (i - 1) %% 26
    res <- paste0(LETTERS[r + 1], res)
    i   <- (i - 1) %/% 26
  }
  res
}

# Write a formula column, but only on the rows flagged TRUE in `ok`.
# Contiguous runs are written in one call to keep this fast.
write_formula_col <- function(wb, sheet, col_idx, formulas, ok) {
  idx <- which(ok)
  if (length(idx) == 0) {
    return(invisible(NULL))
  }
  brk <- c(0, which(diff(idx) != 1), length(idx))
  for (b in seq_len(length(brk) - 1)) {
    sel <- idx[(brk[b] + 1):brk[b + 1]]
    writeFormula(wb, sheet, x = formulas[sel],
                 startCol = col_idx, startRow = FIRST_DATA + sel[1] - 1)
  }
}

# ---- Workbook -------------------------------------------------------------

wb <- createWorkbook()

hdr_style <- createStyle(textDecoration = "bold", halign = "center",
                         border = "bottom", borderStyle = "medium")
key_style <- createStyle(textDecoration = "bold")
date_style <- createStyle(numFmt = "yyyy-mm-dd")
frac_style <- createStyle(numFmt = "0.0000")
num_style  <- createStyle(numFmt = "0.00")

add_person_sheet <- function(pp, dat) {
  sh <- pp$sheet
  addWorksheet(wb, sh)

  # -- metadata block (rows 1-5); formulas reference $B$2 and $B$4 --
  meta <- data.frame(
    key   = c("name", "birth_date", "sex", "height_cm", "source"),
    value = c(if (is.na(pp$name)) "" else pp$name,
              as.character(pp$birth_date),
              as.character(pp$sex),
              as.character(pp$height_cm),
              paste0(basename(SRC_PATH), " / ", SRC_SHEET)),
    note  = c("", "", "0 = female, 1 = male", "", ""),
    stringsAsFactors = FALSE
  )
  writeData(wb, sh, meta, startCol = 1, startRow = 1, colNames = FALSE)
  addStyle(wb, sh, key_style, rows = 1:5, cols = 1, gridExpand = TRUE)
  # writeData() above put everything in as text; re-write the cells that must
  # stay numeric (the derived formulas read $B$2 and $B$4).
  writeData(wb, sh, pp$birth_date, startCol = 2, startRow = 2)
  writeData(wb, sh, pp$sex,        startCol = 2, startRow = 3)
  writeData(wb, sh, pp$height_cm,  startCol = 2, startRow = 4)
  addStyle(wb, sh, date_style, rows = 2, cols = 2)

  # -- data table --
  writeData(wb, sh, dat, startCol = 1, startRow = HEADER_ROW,
            colNames = TRUE, headerStyle = hdr_style)

  n <- nrow(dat)
  r <- FIRST_DATA + seq_len(n) - 1        # sheet row number of each obs

  cn  <- names(dat)
  idx <- setNames(seq_along(cn), cn)      # column name -> column index
  L   <- function(nm) col_letter(idx[[nm]])
  ref <- function(nm) paste0(L(nm), r)    # e.g. "C8", "C9", ...

  addStyle(wb, sh, date_style, rows = r, cols = 1, gridExpand = TRUE)
  freezePane(wb, sh, firstActiveRow = FIRST_DATA, firstActiveCol = 2)
  setColWidths(wb, sh, cols = seq_along(cn), widths = "auto")

  present <- function(nm) !is.na(dat[[nm]])

  # -- age --
  # The source started with ROUNDDOWN(...,0) and switched to ROUNDDOWN(...,1)
  # partway through (source column BT onwards). Use the newer 1-decimal form
  # throughout: it is the current convention and it feeds the Jackson/Pollock
  # equations, where the extra precision matters slightly.
  write_formula_col(
    wb, sh, idx[["age_years"]],
    sprintf("ROUNDDOWN((%s-$B$2)/365,1)", ref("date")),
    present("date")
  )

  # -- BMI: mass / height_m^2 --
  write_formula_col(
    wb, sh, idx[["bmi"]],
    sprintf("%s/(POWER($B$4/100,2))", ref("body_mass")),
    present("body_mass")
  )

  # -- fat_mass / ffm --
  # Person 1 drives these off Mod.JP3, person 2 off plain JP3 (as in source).
  bf_src <- if ("bf_mod_jp3" %in% cn) "bf_mod_jp3" else "bf_jp3"
  write_formula_col(
    wb, sh, idx[["fat_mass"]],
    sprintf("%s*%s", ref("body_mass"), ref(bf_src)),
    present("body_mass") & present(bf_src)
  )
  write_formula_col(
    wb, sh, idx[["ffm"]],
    sprintf("%s-%s", ref("body_mass"), ref("fat_mass")),
    present("body_mass") & present(bf_src)
  )

  # -- Jackson/Pollock 3-site (chest + abdomen + thigh) --
  if ("bf_jp3" %in% cn && "bf_mod_jp3" %in% cn) {
    s3 <- sprintf("(%s+%s+%s)", ref("sf_chest"), ref("sf_abdomen"),
                  ref("sf_thigh"))
    ok3 <- present("sf_chest") & present("sf_abdomen") &
           present("sf_thigh") & present("age_years")
    write_formula_col(
      wb, sh, idx[["bf_jp3"]],
      sprintf(paste0("((495/(1.10938-0.0008267*%s+0.0000016*POWER(%s,2)",
                     "-0.0002575*%s))-450)/100"),
              s3, s3, ref("age_years")),
      ok3
    )

    # -- Modified JP3: JP3 + waist and forearm terms --
    write_formula_col(
      wb, sh, idx[["bf_mod_jp3"]],
      sprintf(paste0("((495/(1.099075-0.0008209*%s+0.0000026*POWER(%s,2)",
                     "-0.0002017*%s-0.00005675*%s+0.000018586*%s))-450)/100"),
              s3, s3, ref("age_years"), ref("waist"), ref("forearm")),
      ok3 & present("waist") & present("forearm")
    )
  }

  # -- US Navy (male form: waist and neck circumference) --
  if ("bf_navy" %in% cn && pp$sex == 1) {
    write_formula_col(
      wb, sh, idx[["bf_navy"]],
      sprintf("((86.01*LOG10(%s-%s))-(70.041*LOG10($B$4))+30.3)/100",
              ref("waist"), ref("neck")),
      present("waist") & present("neck")
    )
  }

  # -- YMCA (waist and mass, imperial conversion inline) --
  if ("bf_ymca" %in% cn) {
    write_formula_col(
      wb, sh, idx[["bf_ymca"]],
      sprintf("(0-98.42+4.15*%s/2.54-0.082*%s/0.4536)/(%s/0.45359)",
              ref("waist"), ref("body_mass"), ref("body_mass")),
      present("waist") & present("body_mass")
    )
  }

  # -- FFMI and height-standardised FFMI --
  if ("ffmi" %in% cn) {
    write_formula_col(
      wb, sh, idx[["ffmi"]],
      sprintf("%s/POWER($B$4/100,2)", ref("ffm")),
      present("body_mass") & present(bf_src)
    )
    write_formula_col(
      wb, sh, idx[["ffmi_std"]],
      sprintf("%s+6.1*(1.8-$B$4/100)", ref("ffmi")),
      present("body_mass") & present(bf_src)
    )
  }

  # -- deltas vs the fixed baseline observation --
  if ("delta_ffm" %in% cn) {
    base_i <- which(dat$date == DELTA_BASE_DATE)
    if (length(base_i) == 1) {
      base_r <- FIRST_DATA + base_i - 1
      ok_d   <- (seq_len(n) >= base_i) & present("body_mass") & present(bf_src)
      write_formula_col(
        wb, sh, idx[["delta_ffm"]],
        sprintf("$%s$%d-%s", L("ffm"), base_r, ref("ffm")), ok_d
      )
      write_formula_col(
        wb, sh, idx[["delta_fat_mass"]],
        sprintf("$%s$%d-%s", L("fat_mass"), base_r, ref("fat_mass")), ok_d
      )
    } else {
      warning("Baseline date ", DELTA_BASE_DATE, " not found on sheet ", sh)
    }
  }

  # -- number formats --
  frac_cols <- idx[grep("^bf_", cn)]
  if (length(frac_cols) > 0) {
    addStyle(wb, sh, frac_style, rows = r, cols = frac_cols,
             gridExpand = TRUE)
  }
  other_cols <- idx[intersect(cn, c("bmi", "fat_mass", "ffm", "ffmi",
                                    "ffmi_std", "delta_ffm",
                                    "delta_fat_mass"))]
  if (length(other_cols) > 0) {
    addStyle(wb, sh, num_style, rows = r, cols = other_cols,
             gridExpand = TRUE)
  }
}

add_person_sheet(p1, dat_p1)
add_person_sheet(p2, dat_p2)

# ---- Notes sheet: measurement-site definitions from the source ------------

addWorksheet(wb, "Notes")
notes <- data.frame(
  item = c(
    "abdomen_narrow", "waist", "hip", "neck",
    "bf_* columns", "sex", "delta_ffm / delta_fat_mass",
    "age_years", "bf_navy", "2013-06-15 bf_navy",
    "fat_mass / ffm", "source", "reference"
  ),
  note = c(
    "Narrowest point of the abdomen ('legszukebb ponton').",
    "1 cm below the navel after a normal exhalation.",
    "At the widest point of the buttocks.",
    "Just below the Adam's apple.",
    "Body-fat estimates expressed as a FRACTION, not a percentage.",
    "0 = female, 1 = male.",
    paste0("Difference vs the ", DELTA_BASE_DATE, " baseline observation ",
           "(positive = loss since baseline)."),
    paste0("ROUNDDOWN((date - birth_date)/365, 1). The source used 0 ",
           "decimals for older rows and 1 decimal from 2024 onwards; the ",
           "1-decimal form is applied throughout here. This shifts the ",
           "older Jackson/Pollock estimates by at most 0.001 (0.1 ",
           "percentage points of body fat)."),
    paste0("Male circumference formula. In the source only one cell held ",
           "this formula and the rest were typed in by hand, but the ",
           "formula reproduces 92 of 93 hand-typed values exactly. ",
           "Person_2's values are kept as values (female form differs)."),
    paste0("The one hand-typed value the formula does not reproduce: the ",
           "source stored 0.2920, the formula gives 0.2260 from waist 90 / ",
           "neck 37.2. The stored value looks like a typo and is now ",
           "recomputed."),
    paste0("Person 1 uses Mod.JP3 as the body-fat source; Person_2 uses ",
           "plain Jackson/Pollock 3 (matching the original sheet)."),
    paste0(basename(SRC_PATH), " sheet '", SRC_SHEET, "'"),
    "https://www.linear-software.com/body-fat-calculator.html"
  ),
  stringsAsFactors = FALSE
)
writeData(wb, "Notes", notes, headerStyle = hdr_style)
setColWidths(wb, "Notes", cols = 1:2, widths = c(28, 100))

# ---- Reference sheet: waist-to-height cutoffs kept from the source --------

ref_rows <- 8:21
ref_tab  <- data.frame(
  subject             = as.character(unlist(raw[ref_rows, 115])),
  waist_height_ratio  = suppressWarnings(as.numeric(unlist(raw[ref_rows, 116]))),
  stringsAsFactors = FALSE
)
ref_tab <- ref_tab[!is.na(ref_tab$subject), ]
addWorksheet(wb, "Reference")
writeData(wb, "Reference", ref_tab, headerStyle = hdr_style)
setColWidths(wb, "Reference", cols = 1:2, widths = c(34, 22))

# openxlsx only *warns* when it cannot overwrite a file that Excel holds
# open, which makes a failed run look like a successful one. Check first.
if (file.exists(OUT_PATH)) {
  con <- suppressWarnings(try(file(OUT_PATH, open = "r+b"), silent = TRUE))
  if (inherits(con, "try-error")) {
    stop("Cannot write ", OUT_PATH, " -- it is open in Excel or another ",
         "program. Close it and re-run.")
  }
  close(con)
}

saveWorkbook(wb, OUT_PATH, overwrite = TRUE)

# ---- Bake cached results into the formula cells --------------------------
#
# openxlsx writes formulas with no cached value, so every reader other than
# Excel itself (readxl, openxlsx, pandas, ...) sees NA in the derived
# columns. A headless LibreOffice round-trip evaluates them and stores the
# results while leaving the formulas live and editable.

recalc_with_libreoffice <- function(path) {
  soffice <- unname(Sys.which("soffice"))
  if (soffice == "") {
    candidates <- c("C:/Program Files/LibreOffice/program/soffice.exe",
                    "C:/Program Files (x86)/LibreOffice/program/soffice.exe")
    hit <- candidates[file.exists(candidates)]
    if (length(hit) == 0) {
      warning("LibreOffice not found -- the derived columns will read as NA ",
              "until the file is opened and saved once in Excel.")
      return(FALSE)
    }
    soffice <- hit[1]
  }

  # LibreOffice refuses to load the workbook straight from its OneDrive
  # path ("source file could not be loaded"), so stage it through a plain
  # local directory in both directions.
  work_dir <- file.path(tempdir(), "anthro_recalc")
  out_dir  <- file.path(work_dir, "out")
  unlink(work_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  staged <- file.path(work_dir, "recalc.xlsx")
  file.copy(path, staged, overwrite = TRUE)

  system2(soffice,
          c("--headless", "--norestore", "--convert-to", "xlsx",
            "--outdir", shQuote(normalizePath(out_dir, winslash = "\\")),
            shQuote(normalizePath(staged, winslash = "\\"))),
          stdout = FALSE, stderr = FALSE)

  produced <- file.path(out_dir, "recalc.xlsx")
  if (file.exists(produced) == FALSE) {
    warning("LibreOffice recalculation failed; formulas are written but ",
            "have no cached values.")
    return(FALSE)
  }
  file.copy(produced, path, overwrite = TRUE)
}

if (recalc_with_libreoffice(OUT_PATH) == TRUE) {
  message("Formulas recalculated (cached values stored).")
}

message("Written: ", OUT_PATH)
