# Survival-analysis measures for adverse-event time-to-event endpoints.

.ae_tte_endpoint_labels <- c(
  AETTE1 = "Time to First Adverse Event",
  AETTE2 = "Time to First Serious Adverse Event",
  AETTE3 = "Time to First Grade 3-5 Adverse Event"
)

.ae_tte_free_labels <- c(
  AETTE1 = "AE-free probability",
  AETTE2 = "Serious AE-free probability",
  AETTE3 = "Grade 3-5 AE-free probability"
)

prepare_ae_tte <- function(
  endpoint = c("AETTE1", "AETTE2", "AETTE3"),
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl,
  adaette = adam_data()$adaette
) {
  endpoint <- match.arg(endpoint)
  population <- match.arg(population)

  analysis <- adaette |>
    dplyr::semi_join(
      filter_population(adsl, population),
      by = "USUBJID"
    ) |>
    dplyr::filter(.data[["PARAMCD"]] == endpoint) |>
    dplyr::transmute(
      USUBJID = .data[["USUBJID"]],
      ACTARM = droplevels(.data[["ACTARM"]]),
      AVAL = as.numeric(.data[["AVAL"]]),
      AVALU = as.character(.data[["AVALU"]]),
      CNSR = as.integer(.data[["CNSR"]])
    )

  list(
    data = analysis,
    endpoint = endpoint,
    endpoint_label = unname(.ae_tte_endpoint_labels[[endpoint]]),
    free_label = unname(.ae_tte_free_labels[[endpoint]]),
    population = population,
    unit = dplyr::first(analysis$AVALU)
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


km_risk_data <- function(fit, arms, times) {
  risk <- summary(fit, times = times, extend = TRUE)
  arm <- if (is.null(risk$strata)) {
    rep(arms[[1]], length(risk$time))
  } else {
    sub("^ACTARM=", "", as.character(risk$strata))
  }
  data.frame(
    time = risk$time,
    n_risk = risk$n.risk,
    treatment_arm = factor(arm, levels = arms)
  )
}

km_risk_times <- function(prepared, n = 4) {
  times <- pretty(c(0, max(prepared$data$AVAL)), n = n)
  unique(c(0, times[times >= 0 & times <= max(prepared$data$AVAL)]))
}

#' Kaplan-Meier plot for time to first adverse event
#'
#' @description
#' Kaplan-Meier estimate of the probability of remaining free of the selected
#' adverse-event endpoint, by actual treatment arm, with log-log 95% confidence
#' intervals, censoring marks, and numbers of patients at risk. This is an
#' adverse-event event-free analysis, not overall survival. Uses one
#' subject-level record per endpoint from adaette, where CNSR = 0 indicates an
#' event and CNSR = 1 indicates censoring.
#'
#' @param endpoint `enum[AETTE1, AETTE2, AETTE3]` Adverse-event endpoint:
#'   first adverse event, first serious adverse event, or first Grade 3-5
#'   adverse event. Defaults to AETTE1.
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A Kaplan-Meier `ggplot` with one AE-free curve per actual treatment
#'   arm, log-log 95% confidence intervals, censoring marks, and a numbers-
#'   at-risk table.
#' @provenance https://doi.org/10.1080/01621459.1958.10501452
#' @measure
ae_event_free_plot <- function(
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

  risk_times <- km_risk_times(prepared)
  risk_data <- km_risk_data(fit, arms, risk_times)
  risk_rows <- data.frame(
    treatment_arm = factor(arms, levels = arms),
    risk_y = -0.14 - 0.08 * (seq_along(arms) - 1)
  )
  risk_data <- dplyr::left_join(
    risk_data,
    risk_rows,
    by = "treatment_arm"
  )
  risk_floor <- min(risk_rows$risk_y) - 0.06
  time_limits <- c(0, max(curve_data$time, risk_times))
  unit_label <- paste0("Time (", tolower(prepared$unit), ")")

  ggplot2::ggplot(
    curve_data,
    ggplot2::aes(
      x = .data[["time"]],
      y = .data[["survival"]],
      colour = .data[["treatment_arm"]]
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
    ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::geom_point(
      data = censor_data,
      shape = 3,
      size = 2
    ) +
    ggplot2::geom_hline(yintercept = -0.04, colour = "grey80") +
    ggplot2::geom_text(
      data = risk_data,
      ggplot2::aes(
        x = .data[["time"]],
        y = .data[["risk_y"]],
        label = .data[["n_risk"]],
        colour = .data[["treatment_arm"]]
      ),
      inherit.aes = FALSE,
      size = 3.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = risk_rows,
      ggplot2::aes(
        x = -Inf,
        y = .data[["risk_y"]],
        label = .data[["treatment_arm"]],
        colour = .data[["treatment_arm"]]
      ),
      inherit.aes = FALSE,
      hjust = 1.3,
      size = 3.2,
      show.legend = FALSE
    ) +
    ggplot2::annotate(
      "text",
      x = -Inf,
      y = -0.08,
      label = "Numbers at risk",
      hjust = 1.3,
      fontface = "bold",
      size = 3.2
    ) +
    ggplot2::scale_y_continuous(
      limits = c(risk_floor, 1),
      breaks = seq(0, 1, by = 0.25),
      labels = scales::label_percent(),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_x_continuous(
      breaks = risk_times,
      limits = time_limits,
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::guides(colour = "none", fill = "none") +
    ggplot2::labs(
      title = paste("Kaplan-Meier:", prepared$free_label),
      subtitle = paste0(
        population_label(prepared$population),
        " population; adaette PARAMCD = ",
        prepared$endpoint
      ),
      x = unit_label,
      y = prepared$free_label
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 105)
    )
}
