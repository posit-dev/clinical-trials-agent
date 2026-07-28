# Table (T) measures. Shared setup and the fidelity note live in helpers.R.

#' Adverse event overview
#'
#' @description
#' Adverse-event overview by treatment arm: number of patients who died, who
#' withdrew from the study due to an AE, who had at least one AE, the total
#' number of AEs, and the number of patients with at least one serious,
#' related, related-serious, severe, or Grade 3-5 / Grade 4-5 AE. Computed on
#' analysis records (ANL01FL = 'Y'). A reduced form of tlg-catalog AET01: its
#' fatal- and action-taken rows depend on columns the simulated data does not
#' populate realistically (AESDTH, AEACN) and are omitted.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with one row per overview statistic and one column per
#'   treatment arm, each cell the count and percentage of patients.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet01.qmd
#' @measure
ae_overview <- function(population = c("SAF", "ITT"),
                        adsl = adam_data()$adsl,
                        adae = adam_data()$adae) {
  adsl <- filter_population(adsl, population)
  adae <- dplyr::semi_join(adae, adsl, by = "USUBJID")

  adsl <- df_explicit_na(adsl)
  adae <- df_explicit_na(
    adae,
    omit_columns = c("SMQ01NAM", "SMQ01SC", "SMQ02NAM", "SMQ02SC", "CQ01NAM", "STUDYID", "USUBJID")
  )

  # Flags from the well-behaved real ADaM columns. AET01 also reports fatal /
  # withdrawal / dose-modification rows from AESDTH and AEACN, which the recipe
  # overwrites with sample() because cadae's raw values are unrealistic; those
  # rows are omitted here.
  adae <- adae %>%
    mutate(
      SER = with_label(AESER == "Y", "Serious AE"),
      RELSER = with_label(AESER == "Y" & AEREL == "Y", "Related Serious AE"),
      REL = with_label(AEREL == "Y", "Related AE"),
      SEV = with_label(AESEV == "SEVERE", "Severe AE (at greatest intensity)"),
      CTC35 = with_label(AETOXGR %in% c("3", "4", "5"), "Grade 3-5 AE"),
      CTC45 = with_label(AETOXGR %in% c("4", "5"), "Grade 4-5 AE")
    ) %>%
    filter(ANL01FL == "Y")

  aesi_vars <- c("SER", "RELSER", "REL", "SEV", "CTC35", "CTC45")

  lyt_adsl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DTHFL" = "Y"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of deaths")
    ) %>%
    count_patients_with_event(
      "USUBJID",
      filters = c("DCSREAS" = "ADVERSE EVENT"),
      denom = "N_col",
      .labels = c(count_fraction = "Total number of patients withdrawn from study due to an AE"),
      table_names = "tot_wd"
    )
  result_adsl <- build_table(lyt_adsl, df = adsl, alt_counts_df = adsl)

  lyt_adae <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one AE",
        nonunique = "Total number of AEs"
      ),
      show_labels = "hidden"
    ) %>%
    count_patients_with_flags(
      "USUBJID",
      flag_variables = aesi_vars,
      denom = "N_col",
      var_labels = "Total number of patients with at least one",
      show_labels = "visible"
    )
  result_adae <- build_table(lyt_adae, df = adae, alt_counts_df = adsl)

  col_info(result_adsl) <- col_info(result_adae)
  result <- rbind(
    result_adae[1:2, ],
    result_adsl,
    result_adae[3:nrow(result_adae), ]
  )

  tidy_tlg(result)
}

#' Patient disposition
#'
#' @description
#' Patient disposition by treatment arm and an overall column: end-of-study
#' status (completed / ongoing / discontinued) and the reasons for
#' discontinuation, grouped into safety (adverse event, death) and non-safety
#' reasons. Adapts tlg-catalog DST01 (variant 2). The recipe's end-of-treatment
#' block is omitted because EOTSTT is an exact copy of EOSSTT in this data.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with one row per status or reason and one column per
#'   treatment arm plus an "All Patients" column.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/disposition/dst01.qmd
#' @measure
disposition <- function(population = c("SAF", "ITT"),
                        adsl = adam_data()$adsl) {
  adsl <- filter_population(adsl, population)
  adsl <- df_explicit_na(adsl) %>%
    mutate(
      EOSSTT = factor(EOSSTT, levels = c("COMPLETED", "ONGOING", "DISCONTINUED")),
      DCSREASGP = factor(
        case_when(
          DCSREAS %in% c("ADVERSE EVENT", "DEATH") ~ "Safety",
          DCSREAS != "<Missing>" & !DCSREAS %in% c("ADVERSE EVENT", "DEATH") ~ "Non-Safety",
          DCSREAS == "<Missing>" ~ "<Missing>"
        ),
        levels = c("Safety", "Non-Safety", "<Missing>")
      )
    )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    count_occurrences("EOSSTT", show_labels = "hidden") %>%
    split_rows_by("DCSREASGP", indent_mod = 1L) %>%
    analyze_vars("DCSREAS", .stats = "count_fraction", denom = "N_col", show_labels = "hidden")

  result <- prune_table(build_table(lyt, df = adsl))
  tidy_tlg(result)
}

#' Study drug exposure
#'
#' @description
#' Descriptive summary statistics (n, mean, standard deviation, median, and
#' range) of study-drug exposure by treatment arm, for the overall dosing
#' parameters: total dose administered and total number of doses administered.
#' Adapts tlg-catalog EXT01 (variant 1), restricted to the real ADaM exposure
#' parameters (the recipe's treatment-duration and missed-dose parameters are
#' randomly generated for the demo and are omitted).
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with one row per summary statistic within each exposure
#'   parameter and drug category, and one column per treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/exposure/ext01.qmd
#' @measure
exposure_summary <- function(population = c("SAF", "ITT"),
                             adsl = adam_data()$adsl,
                             adex = adam_data()$adex) {
  adsl <- filter_population(adsl, population)
  adex <- dplyr::semi_join(adex, adsl, by = "USUBJID")

  adsl <- df_explicit_na(adsl)
  adex <- df_explicit_na(adex) %>%
    filter(PARCAT1 == "OVERALL") %>%
    droplevels()

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARCAT2",
      split_label = "Parameter Category (Drug A/Drug B)",
      label_pos = "topleft",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by("PARAM", split_fun = drop_split_levels) %>%
    analyze_vars(vars = "AVAL")

  result <- build_table(lyt, df = adex, alt_counts_df = adsl)
  tidy_tlg(result)
}

#' Demographics and baseline characteristics
#'
#' @description
#' Descriptive summary of demographic and baseline characteristics by treatment
#' arm and an overall column: age (continuous and grouped), sex, ethnicity,
#' race, and biomarker 1 category. Adapts tlg-catalog DMT01 (variants 1-2),
#' computed from adsl alone.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with one row per characteristic (or level) and one
#'   column per treatment arm plus an "All Patients" column.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/demography/dmt01.qmd
#' @measure
demography <- function(population = c("SAF", "ITT"),
                       adsl = adam_data()$adsl) {
  adsl <- filter_population(adsl, population)
  adsl <- df_explicit_na(adsl) %>%
    mutate(
      SEX = factor(case_when(
        SEX == "M" ~ "Male",
        SEX == "F" ~ "Female",
        SEX == "U" ~ "Unknown",
        SEX == "UNDIFFERENTIATED" ~ "Undifferentiated"
      )),
      AGEGR1 = factor(
        case_when(
          between(AGE, 18, 40) ~ "18-40",
          between(AGE, 41, 64) ~ "41-64",
          AGE > 64 ~ ">=65"
        ),
        levels = c("18-40", "41-64", ">=65")
      ),
      BMRKR1_CAT = factor(
        case_when(
          BMRKR1 < 3.5 ~ "LOW",
          BMRKR1 >= 3.5 & BMRKR1 < 10 ~ "MEDIUM",
          BMRKR1 >= 10 ~ "HIGH"
        ),
        levels = c("LOW", "MEDIUM", "HIGH")
      )
    )

  vars <- c("AGE", "AGEGR1", "SEX", "ETHNIC", "RACE", "BMRKR1_CAT")
  var_labels <- c("Age (yr)", "Age Group", "Sex", "Ethnicity", "Race", "Biomarker 1 Categories")

  result <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    add_overall_col("All Patients") %>%
    analyze_vars(vars = vars, var_labels = var_labels) %>%
    build_table(adsl)

  tidy_tlg(result)
}

#' Adverse events by system organ class and preferred term
#'
#' @description
#' Adverse-event incidence nested by MedDRA System Organ Class (AEBODSYS) then
#' Preferred Term (AEDECOD), by treatment arm plus an overall column: patients
#' with at least one event and the total number of events, at both levels.
#' Computed on analysis records (ANL01FL = 'Y'); empty rows pruned and rows
#' sorted by frequency. Reproduces tlg-catalog AET02 (variants 1-2).
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with SOC and preferred-term rows (count and percentage
#'   of patients, and event counts) and one column per treatment arm plus an
#'   "All Patients" column.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet02.qmd
#' @measure
ae_by_soc_pt <- function(population = c("SAF", "ITT"),
                         adsl = adam_data()$adsl,
                         adae = adam_data()$adae) {
  adsl <- filter_population(adsl, population)
  adae <- dplyr::semi_join(adae, adsl, by = "USUBJID")

  adsl <- df_explicit_na(adsl)
  adae <- df_explicit_na(adae) %>%
    var_relabel(
      AEBODSYS = "MedDRA System Organ Class",
      AEDECOD = "MedDRA Preferred Term"
    ) %>%
    filter(ANL01FL == "Y")

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    add_overall_col(label = "All Patients") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Overall total number of events"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event",
        nonunique = "Total number of events"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(path = "AEBODSYS", scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  tidy_tlg(result)
}

#' Adverse events by highest toxicity grade
#'
#' @description
#' Adverse-event incidence by NCI-CTCAE toxicity grade group (Grade 1-2, 3-4,
#' 5), shown for all events overall and nested by System Organ Class then
#' Preferred Term, by treatment arm. Computed on analysis records
#' (ANL01FL = 'Y', grade known); empty rows pruned and rows sorted by
#' frequency. Reproduces tlg-catalog AET04.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with "Any Grade" and grade-group rows within each SOC
#'   and preferred term, and one column per treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet04.qmd
#' @measure
ae_by_grade <- function(population = c("SAF", "ITT"),
                        adsl = adam_data()$adsl,
                        adae = adam_data()$adae) {
  adsl <- filter_population(adsl, population)
  adae <- dplyr::semi_join(adae, adsl, by = "USUBJID")

  adsl <- df_explicit_na(adsl)
  adae <- df_explicit_na(adae) %>%
    var_relabel(
      AEBODSYS = "MedDRA System Organ Class",
      AEDECOD = "MedDRA Preferred Term"
    ) %>%
    filter(ANL01FL == "Y", AETOXGR != "<Missing>")

  grade_groups <- list("Grade 1-2" = c("1", "2"), "Grade 3-4" = c("3", "4"), "Grade 5" = "5")
  adae$TOTAL_VAR <- "- Any adverse events - "

  # Sum the first content leaf's first cell values, for sorting SOC/PT rows by
  # frequency (from tlg-catalog AET04).
  score_all_sum <- function(tt) {
    cleaf <- collect_leaves(tt)[[1]]
    sum(sapply(row_values(cleaf), function(cv) cv[1]))
  }

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by("TOTAL_VAR", label_pos = "hidden", child_labels = "visible", indent_mod = -1L) %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique", .labels = "- Any Grade -", .indent_mods = 7L) %>%
    count_occurrences_by_grade(var = "AETOXGR", grade_groups = grade_groups, .indent_mods = 6L) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = drop_split_levels,
      split_label = var_labels(adae)[["AEBODSYS"]],
      label_pos = "topleft"
    ) %>%
    split_rows_by(
      "AEDECOD",
      child_labels = "visible",
      split_fun = add_overall_level("- Overall -", trim = TRUE),
      split_label = var_labels(adae)[["AEDECOD"]],
      label_pos = "topleft"
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique", .labels = "- Any Grade -", .indent_mods = 6L) %>%
    count_occurrences_by_grade(var = "AETOXGR", grade_groups = grade_groups, .indent_mods = 5L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(path = "AEBODSYS", scorefun = score_all_sum, decreasing = TRUE) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_all_sum, decreasing = TRUE)

  tidy_tlg(result)
}

#' Deaths
#'
#' @description
#' Deaths by treatment arm and an overall column: total number of deaths and
#' the primary cause of death category (adverse event, progressive disease,
#' other). Safety population. Reproduces tlg-catalog DTHT01 (variant 1).
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with a total-deaths row and primary-cause rows, and one
#'   column per treatment arm plus an "All Patients" column.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/deaths/dtht01.qmd
#' @measure
deaths <- function(population = c("SAF", "ITT"),
                   adsl = adam_data()$adsl) {
  adsl <- filter_population(adsl, population)
  adsl <- df_explicit_na(adsl)
  adsl$DTHCAT <- factor(
    adsl$DTHCAT,
    levels = c("ADVERSE EVENT", "PROGRESSIVE DISEASE", "OTHER", "<Missing>")
  )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    count_values(
      "DTHFL",
      values = "Y",
      .labels = c(count_fraction = "Total number of deaths"),
      .formats = c(count_fraction = "xx (xx.x%)")
    ) %>%
    analyze_vars(vars = "DTHCAT", var_labels = "Primary Cause of Death")

  result <- build_table(lyt, df = adsl)
  tidy_tlg(result)
}
