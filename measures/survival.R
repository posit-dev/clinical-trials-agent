# Survival-analysis measures for adverse-event time-to-event endpoints.

.ae_tte_endpoint_labels <- c(
  AETTE1 = "Time to First Adverse Event",
  AETTE2 = "Time to First Serious Adverse Event",
  AETTE3 = "Time to First Grade 3-5 Adverse Event"
)

prepare_ae_tte <- function(
  endpoint = c("AETTE1", "AETTE2", "AETTE3"),
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl,
  adaette = adam_data()$adaette,
  call = rlang::caller_env()
) {
  endpoint <- measure_choice(
    endpoint,
    names(.ae_tte_endpoint_labels),
    "endpoint",
    call = call
  )
  population <- measure_choice(
    population,
    c("SAF", "ITT"),
    "population",
    call = call
  )

  adsl <- filter_population(adsl, population, call = call)
  analysis <- adaette |>
    dplyr::semi_join(adsl, by = "USUBJID") |>
    dplyr::filter(.data[["PARAMCD"]] == endpoint) |>
    dplyr::transmute(
      USUBJID = .data[["USUBJID"]],
      ACTARM = droplevels(.data[["ACTARM"]]),
      AVAL = as.numeric(.data[["AVAL"]]),
      AVALU = as.character(.data[["AVALU"]]),
      CNSR = as.integer(.data[["CNSR"]])
    )

  if (nrow(analysis) == 0) {
    cli::cli_abort(
      "No records are available for endpoint {.val {endpoint}} in the {population_label(population)} population.",
      call = call
    )
  }

  incomplete <- !stats::complete.cases(
    analysis[, c("USUBJID", "ACTARM", "AVAL", "AVALU", "CNSR")]
  )
  if (any(incomplete)) {
    cli::cli_abort(
      "Endpoint {.val {endpoint}} contains {sum(incomplete)} incomplete survival record{?s}.",
      call = call
    )
  }
  if (any(analysis$AVAL < 0)) {
    cli::cli_abort(
      "Endpoint {.val {endpoint}} contains negative time-to-event values.",
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
      "Endpoint {.val {endpoint}} has more than one record for at least one subject.",
      call = call
    )
  }

  units <- unique(analysis$AVALU)
  if (length(units) != 1) {
    cli::cli_abort(
      "Endpoint {.val {endpoint}} uses multiple time units: {.and {.val {units}}}.",
      call = call
    )
  }

  list(
    data = analysis,
    endpoint = endpoint,
    endpoint_label = unname(.ae_tte_endpoint_labels[[endpoint]]),
    population = population,
    unit = units[[1]]
  )
}

fit_ae_km <- function(prepared) {
  survival::survfit(
    survival::Surv(AVAL, CNSR == 0) ~ ACTARM,
    data = prepared$data,
    conf.int = 0.95,
    conf.type = "log-log"
  )
}

km_curve_data <- function(fit, arms) {
  curve <- summary(fit, censored = TRUE)
  arm <- if (is.null(curve$strata)) {
    rep(arms[[1]], length(curve$time))
  } else {
    sub("^ACTARM=", "", as.character(curve$strata))
  }

  curve_data <- data.frame(
    time = curve$time,
    survival = curve$surv,
    conf_low = curve$lower,
    conf_high = curve$upper,
    n_risk = curve$n.risk,
    n_event = curve$n.event,
    n_censor = curve$n.censor,
    treatment_arm = factor(arm, levels = arms)
  )
  start_data <- data.frame(
    time = 0,
    survival = 1,
    conf_low = 1,
    conf_high = 1,
    n_risk = NA_integer_,
    n_event = 0L,
    n_censor = 0L,
    treatment_arm = factor(arms, levels = arms)
  )

  dplyr::bind_rows(start_data, curve_data) |>
    dplyr::arrange(.data[["treatment_arm"]], .data[["time"]])
}

km_median_data <- function(fit) {
  fit_table <- as.data.frame(summary(fit)$table)
  fit_table$treatment_arm <- sub("^ACTARM=", "", rownames(fit_table))
  rownames(fit_table) <- NULL

  dplyr::transmute(
    fit_table,
    treatment_arm = .data[["treatment_arm"]],
    median_time = .data[["median"]],
    median_ci_lower = .data[["0.95LCL"]],
    median_ci_upper = .data[["0.95UCL"]]
  )
}

#' Kaplan-Meier plot for time to first adverse event
#'
#' @description
#' Kaplan-Meier estimate of the probability of remaining event-free over time,
#' by actual treatment arm, with log-log 95% confidence intervals and censoring
#' marks. Uses one subject-level record per endpoint from adaette, where CNSR = 0
#' indicates an event and CNSR = 1 indicates censoring.
#'
#' @param endpoint `enum[AETTE1, AETTE2, AETTE3]` Adverse-event endpoint:
#'   first adverse event, first serious adverse event, or first Grade 3-5
#'   adverse event. Defaults to AETTE1.
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A Kaplan-Meier `ggplot` with one event-free curve per actual treatment
#'   arm, log-log 95% confidence intervals, and censoring marks.
#' @provenance https://doi.org/10.1080/01621459.1958.10501452
#' @measure
ae_time_to_event_plot <- function(
  endpoint = c("AETTE1", "AETTE2", "AETTE3"),
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl,
  adaette = adam_data()$adaette
) {
  prepared <- prepare_ae_tte(endpoint, population, adsl, adaette)
  fit <- fit_ae_km(prepared)
  arms <- levels(prepared$data$ACTARM)
  curve_data <- km_curve_data(fit, arms)
  censor_data <- dplyr::filter(curve_data, .data[["n_censor"]] > 0)

  ggplot2::ggplot(
    curve_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["survival"]],
      group = .data[["treatment_arm"]]
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data[["conf_low"]],
        ymax = .data[["conf_high"]],
        fill = .data[["treatment_arm"]]
      ),
      alpha = 0.12,
      colour = NA
    ) +
    ggplot2::geom_step(
      ggplot2::aes(colour = .data[["treatment_arm"]]),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = censor_data,
      ggplot2::aes(colour = .data[["treatment_arm"]]),
      shape = 3,
      size = 2
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      labels = scales::label_percent(),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(title = "Actual treatment arm"),
      fill = "none"
    ) +
    ggplot2::labs(
      title = paste("Kaplan-Meier:", prepared$endpoint_label),
      subtitle = paste0(
        population_label(prepared$population),
        " population; adaette PARAMCD = ",
        prepared$endpoint
      ),
      x = paste0("Time (", tolower(prepared$unit), ")"),
      y = "Event-free probability"
    )
}

#' Kaplan-Meier summary for time to first adverse event
#'
#' @description
#' Subject, event, and censoring counts plus the Kaplan-Meier median event-free
#' time and its log-log 95% confidence interval, by actual treatment arm. A
#' missing median or confidence bound means the estimate was not reached.
#'
#' @param endpoint `enum[AETTE1, AETTE2, AETTE3]` Adverse-event endpoint:
#'   first adverse event, first serious adverse event, or first Grade 3-5
#'   adverse event. Defaults to AETTE1.
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A data frame with the endpoint, population, time unit, subject count,
#'   event count, censoring count, and median event-free time with its 95%
#'   confidence interval for each actual treatment arm.
#' @provenance https://doi.org/10.1080/01621459.1958.10501452
#' @measure
ae_time_to_event_summary <- function(
  endpoint = c("AETTE1", "AETTE2", "AETTE3"),
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl,
  adaette = adam_data()$adaette
) {
  prepared <- prepare_ae_tte(endpoint, population, adsl, adaette)
  fit <- fit_ae_km(prepared)

  counts <- prepared$data |>
    dplyr::group_by(.data[["ACTARM"]]) |>
    dplyr::summarise(
      subjects = dplyr::n(),
      events = sum(.data[["CNSR"]] == 0),
      censored = sum(.data[["CNSR"]] == 1),
      .groups = "drop"
    ) |>
    dplyr::rename(treatment_arm = .data[["ACTARM"]]) |>
    dplyr::mutate(treatment_arm = as.character(.data[["treatment_arm"]]))

  counts |>
    dplyr::left_join(km_median_data(fit), by = "treatment_arm") |>
    dplyr::mutate(
      endpoint = prepared$endpoint,
      endpoint_label = prepared$endpoint_label,
      population = prepared$population,
      time_unit = prepared$unit,
      .before = 1
    )
}
