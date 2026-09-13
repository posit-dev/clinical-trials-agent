# Efficacy time-to-event measures. Shared setup lives in helpers.R.

.tte_endpoint_labels <- c(
  OS = "Overall Survival",
  PFS = "Progression-Free Survival",
  EFS = "Event-Free Survival"
)

#' Time-to-event summary
#'
#' @description
#' Event and censoring counts plus the Kaplan-Meier median time to event and
#' its 95% confidence interval, by planned treatment arm. Times are converted
#' from days to months. This measure adapts the core survival summary from
#' tlg-catalog TTET01 without its hypothesis tests and landmark analyses.
#'
#' @param endpoint `enum[OS, PFS, EFS]` Efficacy endpoint: overall survival,
#'   progression-free survival, or event-free survival. Defaults to OS.
#' @param population `enum[ITT, SAF]` Analysis population: intent-to-treat
#'   (ITTFL) or safety (SAFFL). Defaults to intent-to-treat.
#' @return A data frame with event and censoring counts and Kaplan-Meier median
#'   time to event with its 95% confidence interval, by planned treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/tables/efficacy/ttet01.qmd
#' @measure
survival_summary <- function(
  endpoint = c("OS", "PFS", "EFS"),
  population = c("ITT", "SAF"),
  adsl = adam_data()$adsl,
  adtte = adam_data()$adtte
) {
  prepared <- prepare_efficacy_tte(
    endpoint = endpoint,
    population = population,
    adsl = adsl,
    adtte = adtte
  )

  lyt <- basic_table(show_colcounts = TRUE) |>
    split_cols_by("ARM") |>
    analyze_vars(
      vars = "is_event",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients with event (%)")
    ) |>
    analyze_vars(
      vars = "is_censored",
      .stats = "count_fraction",
      .labels = c(count_fraction = "Patients censored (%)"),
      nested = FALSE,
      show_labels = "hidden"
    ) |>
    surv_time(
      vars = "AVAL",
      var_labels = "Time to Event (Months)",
      is_event = "is_event",
      .stats = c("median", "median_ci")
    )

  result <- build_table(
    lyt,
    df = prepared$data,
    alt_counts_df = prepared$adsl
  ) |>
    prune_table()

  tlg_result(result)
}

#' Kaplan-Meier plot of an efficacy endpoint
#'
#' @description
#' Kaplan-Meier curves by planned treatment arm, with censoring marks, median
#' survival annotations, comparative Cox model statistics, and a
#' numbers-at-risk table. This measure reproduces tlg-catalog KMG01 while
#' allowing the endpoint and analysis population to vary.
#'
#' @param endpoint `enum[OS, PFS, EFS]` Efficacy endpoint: overall survival,
#'   progression-free survival, or event-free survival. Defaults to OS.
#' @param population `enum[ITT, SAF]` Analysis population: intent-to-treat
#'   (ITTFL) or safety (SAFFL). Defaults to intent-to-treat.
#' @return A Kaplan-Meier `ggplot` with one curve per planned treatment arm,
#'   censoring marks, median annotations, and a numbers-at-risk table.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/09105206c6fbc70b995c14b15724b46381fa8064/book/graphs/efficacy/kmg01.qmd
#' @measure
survival_km_plot <- function(
  endpoint = c("OS", "PFS", "EFS"),
  population = c("ITT", "SAF"),
  adsl = adam_data()$adsl,
  adtte = adam_data()$adtte
) {
  prepared <- prepare_efficacy_tte(
    endpoint = endpoint,
    population = population,
    adsl = adsl,
    adtte = adtte,
    convert_to_months = FALSE
  )

  tern::g_km(
    df = prepared$data,
    variables = list(
      tte = "AVAL",
      is_event = "is_event",
      arm = "ARMCD"
    ),
    xlab = "Time (Days)",
    ylim = c(0, 1),
    annot_coxph = TRUE,
    font_size = 7,
    lwd = 0.7,
    size = 1.5,
    control_annot_surv_med = tern::control_surv_med_annot(
      digits = 2
    )
  )
}

prepare_efficacy_tte <- function(
  endpoint,
  population,
  adsl,
  adtte,
  convert_to_months = TRUE,
  call = rlang::caller_env()
) {
  endpoint <- measure_choice(
    endpoint,
    names(.tte_endpoint_labels),
    "endpoint",
    call = call
  )
  population <- measure_choice(
    population,
    c("ITT", "SAF"),
    "population",
    call = call
  )
  adsl <- filter_population(adsl, population, call = call)

  analysis <- adtte |>
    dplyr::semi_join(adsl, by = "USUBJID") |>
    dplyr::filter(.data[["PARAMCD"]] == endpoint) |>
    dplyr::transmute(
      USUBJID = .data[["USUBJID"]],
      ARM = factor(as.character(.data[["ARM"]])),
      ARMCD = factor(as.character(.data[["ARMCD"]])),
      AVAL = as.numeric(.data[["AVAL"]]),
      AVALU = as.character(.data[["AVALU"]]),
      CNSR = as.integer(.data[["CNSR"]])
    )

  validate_efficacy_tte(analysis, endpoint, call = call)

  if (convert_to_months) {
    analysis <- analysis |>
      dplyr::mutate(
        AVAL = tern::day2month(.data[["AVAL"]]),
        AVALU = "MONTHS"
      )
  }
  analysis <- analysis |>
    dplyr::mutate(
      is_event = .data[["CNSR"]] == 0L,
      is_censored = .data[["CNSR"]] == 1L
    )

  list(
    data = analysis,
    adsl = adsl,
    endpoint = endpoint,
    endpoint_label = unname(.tte_endpoint_labels[[endpoint]]),
    population = population,
    unit = dplyr::first(analysis$AVALU)
  )
}

validate_efficacy_tte <- function(
  analysis,
  endpoint,
  call = rlang::caller_env()
) {
  if (nrow(analysis) == 0) {
    cli::cli_abort(
      "No time-to-event records are available for endpoint {.val {endpoint}}.",
      call = call
    )
  }

  required <- c("USUBJID", "ARM", "AVAL", "AVALU", "CNSR")
  incomplete <- !stats::complete.cases(analysis[, required])
  if (any(incomplete)) {
    cli::cli_abort(
      "Endpoint {.val {endpoint}} contains {sum(incomplete)} incomplete record{?s}.",
      call = call
    )
  }
  if (any(analysis$AVAL < 0)) {
    cli::cli_abort(
      "Endpoint {.val {endpoint}} contains negative time values.",
      call = call
    )
  }
  if (any(!analysis$CNSR %in% c(0L, 1L))) {
    cli::cli_abort(
      "Endpoint {.val {endpoint}} contains censoring values other than 0 and 1.",
      call = call
    )
  }
  if (anyDuplicated(analysis$USUBJID)) {
    cli::cli_abort(
      "Endpoint {.val {endpoint}} has multiple records for at least one subject.",
      call = call
    )
  }

  units <- unique(analysis$AVALU)
  if (!identical(units, "DAYS")) {
    cli::cli_abort(
      "Endpoint {.val {endpoint}} must use a single time unit of {.val DAYS}.",
      call = call
    )
  }
}
