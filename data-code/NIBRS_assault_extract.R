#-------------------------------------------------------------------------
# NIBRS_assault_extract.R
#
# Purpose:
#   Read the raw ICPSR NIBRS Extract Files for 2021-2024 -- Incident-Level
#   (DS0003), Victim-Level (DS0004), Offender-Level (DS0006) -- and reduce
#   each year to only the rows related to criminal homicide AND aggravated
#   assault, combined into a single violent-offense category (UCR offense
#   codes 91 = Murder/Nonnegligent Manslaughter, 92 = Negligent
#   Manslaughter, and 131 = Aggravated Assault). Justifiable Homicide (93)
#   is EXCLUDED by design, since it is not a crime.
#
#   This mirrors NIBRS_homicide_extraction.R exactly, with one change: the
#   offense-code match now covers 91/92/131 instead of just 91/92. All
#   other logic (incident/victim/offender filtering, multi-year stacking,
#   caveats) is identical.
#
#   Each year's cleaned files are written to data/output/ with the year in
#   the filename, AND all 4 years are stacked into single combined RDS
#   files (one row per victim/offender/incident across 2021-2024), the
#   same way the homicide-only extraction and the ACS extract are stacked.
#
# Inputs (edit STUDIES below if your folder layout differs):
#   data/input/ICPSR_<study>/DS0003/<study>-0003-Data.rda   (incident-level)
#   data/input/ICPSR_<study>/DS0004/<study>-0004-Data.rda   (victim-level)
#   data/input/ICPSR_<study>/DS0006/<study>-0006-Data.rda   (offender-level)
#
# Outputs:
#   data/output/incidents_homicide_assault_<year>.rds / .csv   (one set per year)
#   data/output/victims_homicide_assault_<year>.rds   / .csv
#   data/output/offenders_homicide_assault_<year>.rds / .csv
#   data/output/incidents_homicide_assault_2021_2024.rds        (stacked, all years)
#   data/output/victims_homicide_assault_2021_2024.rds          (stacked, all years)
#   data/output/offenders_homicide_assault_2021_2024.rds        (stacked, all years)
#   data/output/cleaning_summary_homicide_assault.csv            (row counts, all years)
#
#   NOTE: filenames use a "_homicide_assault_" suffix (not just "_assault_")
#   so they never collide with the homicide-only outputs from
#   NIBRS_homicide_extraction.R sitting in the same data/output/ folder --
#   both scripts can be run without either overwriting the other's files.
#
# CAVEATS carried over from the homicide-only version (still apply here):
#
#  1. NIBRS offense codes arrive from ICPSR as FACTOR variables whose
#     levels look like "(091) Murder/Nonnegligent Manslaughter" or
#     "(131) Aggravated Assault" -- the numeric code is embedded as a
#     zero-padded prefix in the label, not stored as a plain number.
#     is_homicide_assault_code() matches on that prefix so it works
#     whether the column comes in as a factor, character, or plain numeric.
#
#  2. The Offender Segment in NIBRS is NOT offense-specific -- an offender
#     record is linked to the whole INCIDENT, not to one particular
#     offense within it. "Offenders" here means "offenders present in an
#     incident that included a homicide or aggravated assault offense" --
#     if an incident had both a homicide and an aggravated assault (e.g.
#     multiple victims with different outcomes), its offenders are
#     included once, not double-counted.
#
#  3. The Victim Segment carries its own offense-code list per victim
#     (V4007-V4016), so victims are filtered more precisely: a victim only
#     counts as included if 91/92/131 appears among THEIR OWN linked
#     offense codes. Restricted to Type of Victim = "Individual"
#     (V4017==1), since only individual persons carry age/sex/race/
#     ethnicity.
#
#  4. Unidentified ("unsolved") offenders are NOT dropped. Do not filter
#     these out -- excluding them would bias offender demographics toward
#     solved cases only.
#
#  5. Agency participation in NIBRS grew each year even after it became
#     the FBI's sole national reporting system in 2021, so 2021 covers
#     somewhat fewer agencies than 2024. A "year" column is kept on every
#     row specifically so this can be checked/controlled for downstream,
#     rather than silently treating all 4 years as equally representative.
#
#  6. NEW for this combined category: a single incident can now be a
#     "homicide+assault incident" via two different routes -- it had an
#     aggravated assault offense, a homicide offense, or both (e.g. one
#     victim killed and another wounded in the same incident). The
#     is_homicide_assault_incident flag is TRUE if ANY of its offense-code
#     slots match 91/92/131; it does not distinguish which combination
#     applies at the incident level. Victim-level rows DO distinguish this
#     (each victim's own offense code(s) are preserved), so if you need to
#     separate "homicide victims" from "aggravated assault victims" within
#     this combined file, filter on the victim's own offense-code columns
#     (V4007-V4016) rather than on the incident-level flag.
#-------------------------------------------------------------------------

suppressMessages({
  library(dplyr)
  library(purrr)
})

## ---- 0. Paths and year/study lookup ---------------------------------------

INPUT_ROOT  <- "data/input"
OUTPUT_DIR  <- "data/output"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ICPSR study number for each year's NIBRS Extract Files
STUDIES <- c(
  "2021" = "38807",
  "2022" = "38925",
  "2023" = "39270",
  "2024" = "39868"
)

## ---- 1. Helpers (same as the homicide-only script, offense match extended) -

# Load an ICPSR .rda file and return its one data frame, regardless of what
# R happened to name the object inside the file.
load_icpsr_df <- function(path) {
  env <- new.env()
  loaded_names <- load(path, envir = env)
  obj_name <- loaded_names[vapply(loaded_names, function(n) is.data.frame(get(n, envir = env)), logical(1))][1]
  if (is.na(obj_name)) stop("No data frame found inside ", path)
  message("    loaded object '", obj_name, "' (", nrow(get(obj_name, envir = env)), " rows) from ", path)
  get(obj_name, envir = env)
}

# TRUE where x represents UCR offense code 91 (Murder/Nonnegligent
# Manslaughter), 92 (Negligent Manslaughter), or 131 (Aggravated Assault).
# Handles factor labels like "(091) Murder/Nonnegligent Manslaughter" or
# "(131) Aggravated Assault", plain character "91"/"131", or numeric
# 91/131.
is_homicide_assault_code <- function(x) {
  x_chr <- as.character(x)
  by_label  <- grepl("^\\(0*9[12]\\)|^\\(0*131\\)", x_chr)
  by_number <- suppressWarnings(as.numeric(x_chr)) %in% c(91, 92, 131)
  by_label | by_number
}

# Build a single incident-key column so the three files can be matched to
# each other (NIBRS incidents are uniquely identified by agency ORI +
# incident number, not by INCNUM alone). Year is included so keys never
# collide across years once files are stacked.
add_incident_key <- function(df, year) {
  df %>% mutate(incident_key = paste(year, ORI, INCNUM, sep = "_"))
}

n_rows <- function(df) format(nrow(df), big.mark = ",")

## ---- 2. One function that does exactly what the homicide-only script did,
##         with the extended offense-code match ------------------------------

clean_nibrs_homicide_assault_year <- function(year, study) {

  message("\n=== ", year, " (ICPSR ", study, ") ===")

  input_dir     <- file.path(INPUT_ROOT, paste0("ICPSR_", study))
  incident_rda  <- file.path(input_dir, "DS0003", paste0(study, "-0003-Data.rda"))
  victim_rda    <- file.path(input_dir, "DS0004", paste0(study, "-0004-Data.rda"))
  offender_rda  <- file.path(input_dir, "DS0006", paste0(study, "-0006-Data.rda"))
  stopifnot(file.exists(incident_rda), file.exists(victim_rda), file.exists(offender_rda))

  summary_rows <- list()

  ## -- Incident-level file: identify homicide/assault incidents ------------
  message("  Reading incident-level file (DS0003) ...")
  incidents <- load_icpsr_df(incident_rda)
  incidents <- add_incident_key(incidents, year)
  n_incidents_total <- nrow(incidents)

  offense_cols <- intersect(c("V20061", "V20062", "V20063"), names(incidents))
  stopifnot(length(offense_cols) > 0)

  incidents$is_homicide_assault_incident <- Reduce(`|`, lapply(offense_cols, function(cn) is_homicide_assault_code(incidents[[cn]])))
  incidents_homicide_assault <- incidents %>% filter(is_homicide_assault_incident) %>% mutate(year = year)
  homicide_assault_incident_keys <- unique(incidents_homicide_assault$incident_key)

  message("    homicide/assault incidents: ", n_rows(incidents_homicide_assault), " of ", n_rows(incidents), " total incidents")
  summary_rows[["incidents"]] <- data.frame(year = year, file = "incidents (DS0003)",
                                             rows_before = n_incidents_total,
                                             rows_after = nrow(incidents_homicide_assault))

  saveRDS(incidents_homicide_assault, file.path(OUTPUT_DIR, paste0("incidents_homicide_assault_", year, ".rds")))
  write.csv(incidents_homicide_assault, file.path(OUTPUT_DIR, paste0("incidents_homicide_assault_", year, ".csv")), row.names = FALSE, na = "")
  rm(incidents); gc()

  ## -- Victim-level file: keep individual homicide/assault victims ---------
  message("  Reading victim-level file (DS0004) ...")
  victims <- load_icpsr_df(victim_rda)
  victims <- add_incident_key(victims, year)
  n_victims_total <- nrow(victims)

  victim_offense_cols <- intersect(paste0("V40", sprintf("%02d", 7:16)), names(victims))
  if (length(victim_offense_cols) == 0) {
    victim_offense_cols <- intersect(paste0("V40", 7:16), names(victims))
  }
  stopifnot(length(victim_offense_cols) > 0)

  victims$is_homicide_assault_victim_offense <- Reduce(`|`, lapply(victim_offense_cols, function(cn) is_homicide_assault_code(victims[[cn]])))

  is_individual <- as.character(victims$V4017) %in% c("1", "(1) Individual") |
    grepl("^\\(0*1\\)", as.character(victims$V4017))

  victims_homicide_assault <- victims %>%
    filter(is_homicide_assault_victim_offense, is_individual) %>%
    filter(incident_key %in% homicide_assault_incident_keys) %>%
    mutate(year = year)

  message("    homicide/assault victims (individual, own-offense = homicide or aggravated assault): ",
          n_rows(victims_homicide_assault), " of ", n_rows(victims), " total victim records")
  summary_rows[["victims"]] <- data.frame(year = year, file = "victims (DS0004)",
                                           rows_before = n_victims_total,
                                           rows_after = nrow(victims_homicide_assault))

  saveRDS(victims_homicide_assault, file.path(OUTPUT_DIR, paste0("victims_homicide_assault_", year, ".rds")))
  write.csv(victims_homicide_assault, file.path(OUTPUT_DIR, paste0("victims_homicide_assault_", year, ".csv")), row.names = FALSE, na = "")
  rm(victims); gc()

  ## -- Offender-level file: keep offenders in homicide/assault incidents ---
  message("  Reading offender-level file (DS0006) ...")
  offenders <- load_icpsr_df(offender_rda)
  offenders <- add_incident_key(offenders, year)
  n_offenders_total <- nrow(offenders)

  offenders_homicide_assault <- offenders %>%
    filter(incident_key %in% homicide_assault_incident_keys) %>%
    mutate(year = year)

  message("    offenders in homicide/assault incidents: ", n_rows(offenders_homicide_assault), " of ", n_rows(offenders), " total offender records")
  summary_rows[["offenders"]] <- data.frame(year = year, file = "offenders (DS0006)",
                                             rows_before = n_offenders_total,
                                             rows_after = nrow(offenders_homicide_assault))

  saveRDS(offenders_homicide_assault, file.path(OUTPUT_DIR, paste0("offenders_homicide_assault_", year, ".rds")))
  write.csv(offenders_homicide_assault, file.path(OUTPUT_DIR, paste0("offenders_homicide_assault_", year, ".csv")), row.names = FALSE, na = "")
  rm(offenders); gc()

  do.call(rbind, summary_rows)
}

## ---- 3. Loop over all 4 years ---------------------------------------------

all_summaries <- map_dfr(names(STUDIES), function(yr) {
  clean_nibrs_homicide_assault_year(yr, STUDIES[[yr]])
})

write.csv(all_summaries, file.path(OUTPUT_DIR, "cleaning_summary_homicide_assault.csv"), row.names = FALSE)
message("\nPer-year cleaning summary:")
print(all_summaries, row.names = FALSE)

## ---- 4. Stack all 4 years into combined microdata files -------------------
# Same idea as the homicide-only extraction and the ACS extract: one long
# file per segment, 2021-2024 pooled, with `year` kept as a column so you
# can still subset back to single years or check year-to-year coverage
# differences later.

message("\nStacking all years into combined files ...")

# ICPSR sometimes ships the same-named column as plain numeric in one year
# and as a labeled factor in another (depends on whether that year's data
# happened to contain any of the special missing-value codes for that
# variable). bind_rows() can't reconcile that automatically, so every
# column is coerced to character right before stacking -- this only
# affects the combined multi-year file; the per-year .rds files saved
# above keep their original types/factor labels untouched.
read_as_character = function(path) {
  readRDS(path) %>% mutate(across(everything(), as.character))
}

incidents_all = map_dfr(names(STUDIES), function(yr) {
  read_as_character(file.path(OUTPUT_DIR, paste0("incidents_homicide_assault_", yr, ".rds")))
})
victims_all = map_dfr(names(STUDIES), function(yr) {
  read_as_character(file.path(OUTPUT_DIR, paste0("victims_homicide_assault_", yr, ".rds")))
})
offenders_all = map_dfr(names(STUDIES), function(yr) {
  read_as_character(file.path(OUTPUT_DIR, paste0("offenders_homicide_assault_", yr, ".rds")))
})

saveRDS(incidents_all, file.path(OUTPUT_DIR, "incidents_homicide_assault_2021_2024.rds"))
saveRDS(victims_all,   file.path(OUTPUT_DIR, "victims_homicide_assault_2021_2024.rds"))
saveRDS(offenders_all, file.path(OUTPUT_DIR, "offenders_homicide_assault_2021_2024.rds"))

message("  incidents_homicide_assault_2021_2024.rds: ", n_rows(incidents_all), " rows")
message("  victims_homicide_assault_2021_2024.rds:   ", n_rows(victims_all), " rows")
message("  offenders_homicide_assault_2021_2024.rds: ", n_rows(offenders_all), " rows")

message("\nDone. Per-year and stacked files written to ", OUTPUT_DIR, "/")
