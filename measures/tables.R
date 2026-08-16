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
#' sorted by frequency. Optionally restricted to preferred terms above an
#' incidence threshold. Reproduces tlg-catalog AET02 (variants 1-2, 7).
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @param min_incidence `number` Keep only preferred terms that occur in at
#'   least this fraction (0-1) of patients in at least one treatment arm (e.g.
#'   0.05 for 5%). Omit to show all terms.
#' @return A richly formatted table with SOC and preferred-term rows (count and
#'   percentage of patients, and event counts) and one column per treatment arm
#'   plus an "All Patients" column.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet02.qmd
#' @measure
ae_by_soc_pt <- function(population = c("SAF", "ITT"),
                         min_incidence = NULL,
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

  if (!is.null(min_incidence)) {
    arms <- names(table(adsl$ACTARM))
    result <- prune_table(
      result,
      keep_rows(has_fraction_in_any_col(atleast = min_incidence, col_names = arms))
    )
  }

  rich_tlg(result)
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

#' Laboratory abnormalities not present at baseline
#'
#' @description
#' Count of patients with a post-baseline low or high laboratory abnormality
#' (by reference-range indicator) that was not already abnormal at baseline, by
#' treatment arm, for each laboratory test. On-treatment records only.
#' Reproduces tlg-catalog LBT04.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with low- and high-abnormality patient counts within
#'   each laboratory test, and one column per treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/lab-results/lbt04.qmd
#' @measure
lab_abnormalities <- function(population = c("SAF", "ITT"),
                              adsl = adam_data()$adsl,
                              adlb = adam_data()$adlb) {
  adsl <- filter_population(adsl, population)
  adlb <- dplyr::semi_join(adlb, adsl, by = "USUBJID")

  adsl <- df_explicit_na(adsl)
  adlb <- df_explicit_na(adlb) %>%
    filter(ONTRTFL == "Y", ANRIND != "<Missing>") %>%
    var_relabel(
      PARAM = "Laboratory Test",
      ANRIND = "Direction of Abnormality"
    )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAM",
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = obj_label(adlb$PARAM)
    ) %>%
    count_abnormal(
      var = "ANRIND",
      abnormal = list(Low = c("LOW", "LOW LOW"), High = c("HIGH", "HIGH HIGH")),
      exclude_base_abn = TRUE
    ) %>%
    append_varlabels(adlb, "ANRIND", indent = 1L)

  result <- build_table(lyt, df = adlb, alt_counts_df = adsl)
  tidy_tlg(result)
}

#' Adverse events by greatest intensity
#'
#' @description
#' Adverse-event incidence by greatest reported intensity, nested by MedDRA
#' System Organ Class (AEBODSYS) then Preferred Term (AEDECOD), by treatment
#' arm: for each term the number of patients with an event at each intensity
#' (mild, moderate, severe) and at any intensity, counting each patient once at
#' their greatest intensity. Computed on analysis records (ANL01FL = 'Y'); rows
#' sorted by frequency. Reproduces tlg-catalog AET03, using the real severity
#' column AESEV; the catalog's demonstration-only injection of a "life
#' threatening" level is omitted.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with an "Any Intensity" row and per-intensity rows
#'   within each SOC and preferred term, and one column per treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet03.qmd
#' @measure
ae_by_intensity <- function(population = c("SAF", "ITT"),
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
    filter(ANL01FL == "Y") %>%
    mutate(ASEV = factor(as.character(AESEV), levels = c("MILD", "MODERATE", "SEVERE")))

  grade_groups <- list("- Any Intensity -" = c("MILD", "MODERATE", "SEVERE"))
  split_fun <- trim_levels_in_group

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    count_occurrences_by_grade(var = "ASEV", grade_groups = grade_groups) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = TRUE,
      split_fun = split_fun("ASEV"),
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_occurrences_by_grade(var = "ASEV", grade_groups = grade_groups) %>%
    split_rows_by(
      "AEDECOD",
      child_labels = "visible",
      nested = TRUE,
      indent_mod = -1L,
      split_fun = split_fun("ASEV"),
      label_pos = "topleft",
      split_label = obj_label(adae$AEDECOD)
    ) %>%
    summarize_num_patients(var = "USUBJID", .stats = "unique", .labels = c("- Any Intensity -")) %>%
    count_occurrences_by_grade(var = "ASEV", .indent_mods = -1L) %>%
    append_varlabels(adae, "AESEV", indent = 2L)

  result <- build_table(lyt, adae, alt_counts_df = adsl) %>%
    sort_at_path(path = "AEBODSYS", scorefun = cont_n_allcols, decreasing = TRUE) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = cont_n_allcols, decreasing = TRUE)

  tidy_tlg(result)
}

#' Most frequent adverse events by highest toxicity grade
#'
#' @description
#' Adverse events reported in at least 10% of patients in any treatment arm,
#' nested by MedDRA System Organ Class (AEBODSYS) then Preferred Term
#' (AEDECOD), with treatment-arm columns further split by highest NCI-CTCAE
#' toxicity grade group (any grade, grade 3-4, grade 5). Each patient is counted
#' once per term at their maximum grade. Rows sorted by any-grade frequency.
#' Reproduces tlg-catalog AET04_PI (variant 1).
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A richly formatted table with SOC and preferred-term rows and one
#'   column per treatment-arm-by-grade-group combination.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet04_pi.qmd
#' @measure
ae_frequent_by_grade <- function(population = c("SAF", "ITT"),
                                 adsl = adam_data()$adsl,
                                 adae = adam_data()$adae) {
  adsl <- filter_population(adsl, population)
  adae <- dplyr::semi_join(adae, adsl, by = "USUBJID")

  adae_max <- adae %>%
    group_by(ACTARM, USUBJID, AEBODSYS, AEDECOD) %>%
    summarize(MAXAETOXGR = max(as.numeric(AETOXGR)), .groups = "drop") %>%
    ungroup() %>%
    mutate(
      MAXAETOXGR = factor(MAXAETOXGR),
      AEDECOD = droplevels(as.factor(AEDECOD))
    )

  adsl <- df_explicit_na(adsl)
  adae_max <- df_explicit_na(adae_max)

  grade_groups <- list(
    "Any Grade (%)" = c("1", "2", "3", "4", "5"),
    "Grade 3-4 (%)" = c("3", "4"),
    "Grade 5 (%)" = "5"
  )
  col_counts <- rep(table(adsl$ACTARM), each = length(grade_groups))

  criteria_fun <- function(tr) is(tr, "ContentRow")

  full_table <- basic_table() %>%
    split_cols_by("ACTARM") %>%
    split_cols_by_groups("MAXAETOXGR", groups_list = grade_groups) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible", nested = FALSE, indent_mod = -1L,
      split_fun = trim_levels_in_group("AEDECOD")
    ) %>%
    append_topleft("MedDRA System Organ Class") %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = "unique",
      .labels = "Total number of patients with at least one adverse event"
    ) %>%
    analyze_vars(
      "AEDECOD",
      na.rm = FALSE,
      denom = "N_col",
      .stats = "count_fraction",
      .formats = c(count_fraction = format_fraction_threshold(0.01))
    ) %>%
    append_topleft("  MedDRA Preferred Term") %>%
    build_table(adae_max, col_counts = col_counts) %>%
    sort_at_path(
      path = c("AEBODSYS"),
      scorefun = score_occurrences_cont_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    ) %>%
    sort_at_path(
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = score_occurrences_cols(col_indices = c(1, 4, 7)),
      decreasing = TRUE
    )

  at_least_10percent_any <- has_fraction_in_any_col(atleast = 0.1, col_indices = c(1, 4, 7))

  result <- full_table %>%
    trim_rows(criteria = criteria_fun) %>%
    prune_table(keep_rows(at_least_10percent_any))

  rich_tlg(result)
}

#' Adverse events by sex
#'
#' @description
#' Adverse-event incidence nested by MedDRA System Organ Class (AEBODSYS) then
#' Preferred Term (AEDECOD), with treatment-arm columns further split by sex:
#' patients with at least one event and the total number of events, at both
#' levels. Empty rows pruned and rows sorted by frequency. Reproduces
#' tlg-catalog AET06 (variant 1, adverse events by sex).
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A richly formatted table with SOC and preferred-term rows and one
#'   column per treatment-arm-by-sex combination.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet06.qmd
#' @measure
ae_by_sex <- function(population = c("SAF", "ITT"),
                      adsl = adam_data()$adsl,
                      adae = adam_data()$adae) {
  adsl <- filter_population(adsl, population)
  adae <- dplyr::semi_join(adae, adsl, by = "USUBJID")

  adsl <- df_explicit_na(adsl)
  adae <- df_explicit_na(adae) %>%
    var_relabel(
      AEBODSYS = "MedDRA System Organ Class",
      AEDECOD = "MedDRA Preferred Term"
    )

  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_cols_by("SEX") %>%
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
      split_fun = split_fun,
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
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  rich_tlg(result)
}

#' Adverse events related to study drug
#'
#' @description
#' Incidence of adverse events assessed as related to study drug (AEREL = 'Y'),
#' nested by MedDRA System Organ Class (AEBODSYS) then Preferred Term
#' (AEDECOD), by treatment arm plus an overall column: patients with at least
#' one related event and the total number of related events, at both levels.
#' Empty rows pruned and rows sorted by frequency. Reproduces tlg-catalog AET09
#' (variant 1).
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with SOC and preferred-term rows (count and percentage
#'   of patients, and event counts) and one column per treatment arm plus an
#'   "All Patients" column.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet09.qmd
#' @measure
ae_related <- function(population = c("SAF", "ITT"),
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
    filter(AEREL == "Y")

  split_fun <- drop_split_levels

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    add_overall_col(label = "All Patients") %>%
    analyze_num_patients(
      vars = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Overall total number of events related to study drug"
      )
    ) %>%
    split_rows_by(
      "AEBODSYS",
      child_labels = "visible",
      nested = FALSE,
      split_fun = split_fun,
      label_pos = "topleft",
      split_label = obj_label(adae$AEBODSYS)
    ) %>%
    summarize_num_patients(
      var = "USUBJID",
      .stats = c("unique", "nonunique"),
      .labels = c(
        unique = "Total number of patients with at least one adverse event related to study drug",
        nonunique = "Total number of events related to study drug"
      )
    ) %>%
    count_occurrences(vars = "AEDECOD", .indent_mods = -1L) %>%
    append_varlabels(adae, "AEDECOD", indent = 1L)

  result <- build_table(lyt, df = adae, alt_counts_df = adsl) %>%
    prune_table() %>%
    sort_at_path(path = c("AEBODSYS"), scorefun = cont_n_allcols) %>%
    sort_at_path(path = c("AEBODSYS", "*", "AEDECOD"), scorefun = score_occurrences)

  tidy_tlg(result)
}

#' Most frequent adverse events
#'
#' @description
#' Adverse events reported in at least 5% of patients in any treatment arm, as
#' a flat list of MedDRA Preferred Terms (AEDECOD, not nested under System
#' Organ Class), by treatment arm plus an overall column: count and percentage
#' of patients with each event. Rows sorted by frequency. Reproduces
#' tlg-catalog AET10 (variant 1).
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with one row per preferred term meeting the 5% threshold
#'   and one column per treatment arm plus an "All Patients" column.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet10.qmd
#' @measure
ae_most_frequent <- function(population = c("SAF", "ITT"),
                             adsl = adam_data()$adsl,
                             adae = adam_data()$adae) {
  adsl <- filter_population(adsl, population)
  adae <- dplyr::semi_join(adae, adsl, by = "USUBJID")

  adsl <- df_explicit_na(adsl)
  adae <- df_explicit_na(adae)

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(
      var = "ACTARM",
      split_fun = add_overall_level("All Patients", first = FALSE)
    ) %>%
    count_occurrences(vars = "AEDECOD")

  tbl <- build_table(lyt, df = adae, alt_counts_df = adsl)

  tbl <- prune_table(
    tbl,
    prune_func = keep_rows(
      has_fraction_in_any_col(atleast = 0.05, col_names = levels(adsl$ACTARM))
    )
  )

  result <- sort_at_path(tbl, path = c("AEDECOD"), scorefun = score_occurrences)
  tidy_tlg(result)
}

#' Adverse event rate adjusted for patient-years at risk
#'
#' @description
#' Rate of the first adverse-event occurrence per 100 patient-years at risk,
#' with a 95% confidence interval, by treatment arm: total patient-years at
#' risk, number of patients with an event, and the adjusted rate. Uses the
#' time-to-first-AE parameter (AETTE1) from adaette. Reproduces tlg-catalog
#' AET05.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with patient-years at risk, event count, rate per 100
#'   patient-years, and its confidence interval, one column per treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/adverse-events/aet05.qmd
#' @measure
ae_incidence_rate <- function(population = c("SAF", "ITT"),
                              adsl = adam_data()$adsl,
                              adaette = adam_data()$adaette) {
  adsl <- filter_population(adsl, population)
  adaette <- dplyr::semi_join(adaette, adsl, by = "USUBJID")

  adsl <- df_explicit_na(adsl)
  anl <- df_explicit_na(adaette) %>%
    filter(PARAMCD == "AETTE1") %>%
    mutate(n_events = as.integer(CNSR == 0))

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    estimate_incidence_rate(
      vars = "AVAL",
      n_events = "n_events",
      control = control_incidence_rate(num_pt_year = 100)
    )

  result <- build_table(lyt, anl, alt_counts_df = adsl)
  tidy_tlg(result)
}
