# Shared setup for the TLG semantic layer.
#
# commons sources every .R file in this directory into one environment, so the
# helpers defined here are available to the measures in tables.R and
# listings.R. Functions without an @measure tag (like these) stay helpers and
# are not registered as measures.
#
# Each measure is an extraction of a validated recipe from the public
# insightsengineering/tlg-catalog, run over CDISC ADaM sample data from
# random.cdisc.data. A measure body mirrors the catalog recipe (same tern /
# rtables composition), with the inputs a monitor would vary lifted to
# documented @param arguments; everything else stays hardcoded as vetted. Each
# measure returns a tidy data frame (via tidy_tlg()) so the agent reasons over
# data rather than a formatted TableTree, and carries an @provenance tag
# pinning the exact catalog source it came from.
#
# Fidelity note: these measures use only columns whose simulated values are
# realistic. Some catalog recipes overwrite real ADaM columns with sample()
# because cadae/cadsl's raw values are unrealistic or degenerate -- AET01's
# AESDTH (yields more "fatal" AEs than there are deaths) and AEACN, and DST01's
# EOTSTT (an exact copy of EOSSTT); rows that would depend on those are omitted.
# EXT01's TDURD / TNDOSMIS are genuinely invented (no such PARAMCD) and are
# likewise omitted. Real, well-behaved columns (AESER, AEREL, AESEV, AETOXGR,
# DTHFL, DCSREAS, ...) are used as-is.

library(tern)
library(dplyr)

source("R/data.R")

# Restrict adsl to an analysis population: "SAF" -> SAFFL, "ITT" -> ITTFL.
filter_population <- function(adsl, population = c("SAF", "ITT")) {
  population <- match.arg(population)
  flag <- if (population == "SAF") "SAFFL" else "ITTFL"
  dplyr::filter(adsl, .data[[flag]] == "Y")
}

# Convert a built rtables TableTree into a flat, labelled data frame for the
# agent: one row per statistic, one column per treatment arm, cells as the
# standard "n (%)" / "mean (sd)" / "min - max" strings. Drops as_result_df()'s
# internal layout metadata, keeping the human-readable row label and the arm
# value columns. Formatted strings (rather than numeric list-columns) are the
# one shape that is uniform across count and descriptive-statistic tables.
tidy_tlg <- function(tt) {
  df <- as_result_df(tt, data_format = "strings", keep_label_rows = TRUE)
  meta <- c("avar_name", "row_name", "row_num", "is_group_summary", "node_class")
  df <- df[, setdiff(names(df), meta), drop = FALSE]
  names(df)[names(df) == "label_name"] <- "statistic"
  df
}
