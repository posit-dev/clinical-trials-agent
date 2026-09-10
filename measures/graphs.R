# Graph (G) measures. Shared setup and the fidelity note live in helpers.R.

#' Adverse events by system organ class
#'
#' @description
#' Percentage of patients with at least one adverse event in each MedDRA
#' System Organ Class, by treatment arm. Computed on analysis records
#' (ANL01FL = 'Y'), with each patient counted once per system organ class.
#' Adapts the arm-by-covariate percentage bar graph in tlg-catalog BRG01.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A grouped horizontal `ggplot` bar graph of patient incidence
#'   percentages by system organ class and treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/graphs/other/brg01.qmd
#' @measure
ae_by_soc_plot <- function(
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl,
  adae = adam_data()$adae
) {
  adsl <- filter_population(adsl, population)
  adae <- dplyr::semi_join(adae, adsl, by = "USUBJID")

  arm_counts <- dplyr::count(adsl, ACTARM, name = "N_arm")
  anl <- adae |>
    dplyr::filter(ANL01FL == "Y") |>
    dplyr::distinct(USUBJID, ACTARM, AEBODSYS) |>
    dplyr::count(ACTARM, AEBODSYS, name = "n") |>
    dplyr::left_join(arm_counts, by = "ACTARM") |>
    dplyr::mutate(pct = round((n / N_arm) * 100, 2))

  require_plot_data(
    anl,
    "No adverse-event analysis records are available for this population."
  )

  ggplot2::ggplot(
    anl,
    ggplot2::aes(
      x = stats::reorder(.data[["AEBODSYS"]], .data[["pct"]], max),
      y = .data[["pct"]],
      fill = .data[["ACTARM"]]
    )
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::coord_flip() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(title = "Treatment arm")
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    ggplot2::labs(
      title = "Patients With Adverse Events by System Organ Class",
      subtitle = paste(population_label(population), "population"),
      x = "MedDRA System Organ Class",
      y = "Patients with at least one adverse event"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}

#' Study drug exposure by treatment arm
#'
#' @description
#' Distribution of total dose administered (TDOSE) by treatment arm for the
#' overall exposure records. Adapts the standard tlg-catalog BWG01 box plot:
#' whiskers show the observed minimum and maximum, the box shows the quartiles
#' and median, and an asterisk marks the mean.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A `ggplot` box graph of total dose by treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/graphs/other/bwg01.qmd
#' @measure
exposure_by_arm_plot <- function(
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl,
  adex = adam_data()$adex
) {
  adsl <- filter_population(adsl, population)
  adex <- adex |>
    dplyr::semi_join(adsl, by = "USUBJID") |>
    dplyr::filter(PARCAT1 == "OVERALL", PARAMCD == "TDOSE") |>
    droplevels()

  require_plot_data(
    adex,
    "No total-dose exposure records are available for this population."
  )

  catalog_boxplot(
    adex,
    title = "Box Plot of Total Dose Administered",
    subtitle = paste(population_label(population), "population"),
    y_label = paste0(adex$PARAMCD[[1]], " (", adex$AVALU[[1]], ")")
  )
}

#' Age distribution by treatment arm
#'
#' @description
#' Distribution of patient age by treatment arm. Adapts the standard
#' tlg-catalog BWG01 box plot: whiskers show the observed minimum and maximum,
#' the box shows the quartiles and median, and an asterisk marks the mean.
#'
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A `ggplot` box graph of age by treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/graphs/other/bwg01.qmd
#' @measure
age_by_arm_plot <- function(
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl
) {
  adsl <- filter_population(adsl, population)

  require_plot_data(
    adsl,
    "No subject records are available for this population."
  )

  catalog_boxplot(
    adsl,
    y = "AGE",
    title = "Box Plot of Patient Age",
    subtitle = paste(population_label(population), "population"),
    y_label = "Age (years)"
  )
}

#' Laboratory values by treatment arm
#'
#' @description
#' Distribution of laboratory analysis values for one parameter and visit by
#' treatment arm. Reproduces the standard tlg-catalog BWG01 box plot:
#' whiskers show the observed minimum and maximum, the box shows the quartiles
#' and median, and an asterisk marks the mean.
#'
#' @param parameter `enum[ALT, CRP, IGA]` Laboratory parameter code. Defaults
#'   to ALT.
#' @param visit `enum[BASELINE, WEEK 1 DAY 8, WEEK 2 DAY 15, WEEK 3 DAY 22,
#'   WEEK 4 DAY 29, WEEK 5 DAY 36]` Analysis visit. Defaults to WEEK 2 DAY 15,
#'   matching tlg-catalog BWG01.
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A `ggplot` box graph of laboratory values by treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/graphs/other/bwg01.qmd
#' @measure
lab_by_arm_plot <- function(
  parameter = c("ALT", "CRP", "IGA"),
  visit = c(
    "WEEK 2 DAY 15",
    "BASELINE",
    "WEEK 1 DAY 8",
    "WEEK 3 DAY 22",
    "WEEK 4 DAY 29",
    "WEEK 5 DAY 36"
  ),
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl,
  adlb = adam_data()$adlb
) {
  parameter <- measure_choice(
    parameter,
    c("ALT", "CRP", "IGA"),
    "parameter"
  )
  visit <- measure_choice(
    visit,
    c(
      "WEEK 2 DAY 15",
      "BASELINE",
      "WEEK 1 DAY 8",
      "WEEK 3 DAY 22",
      "WEEK 4 DAY 29",
      "WEEK 5 DAY 36"
    ),
    "visit"
  )

  adsl <- filter_population(adsl, population)
  adlb <- adlb |>
    dplyr::semi_join(adsl, by = "USUBJID") |>
    dplyr::filter(PARAMCD == parameter, AVISIT == visit) |>
    droplevels()

  require_plot_data(
    adlb,
    "No laboratory records match the selected parameter, visit, and population."
  )

  catalog_boxplot(
    adlb,
    title = "Box Plot of Laboratory Test Results",
    subtitle = paste("Visit:", visit),
    y_label = paste0(adlb$PARAMCD[[1]], " (", adlb$AVALU[[1]], ")")
  )
}

#' Laboratory values over time
#'
#' @description
#' Mean laboratory value and 95% confidence limits by visit and treatment arm
#' for one laboratory parameter. Reproduces tlg-catalog MNG01 using
#' `tern::g_lineplot()`, restricted to analysis, on-treatment, non-screening
#' records.
#'
#' @param parameter `enum[ALT, CRP, IGA]` Laboratory parameter code. Defaults
#'   to ALT.
#' @param population `enum[SAF, ITT]` Analysis population: safety (SAFFL) or
#'   intent-to-treat (ITTFL). Defaults to safety.
#' @return A `ggplot` line graph of mean laboratory values and 95% confidence
#'   limits over time by treatment arm.
#' @provenance https://github.com/insightsengineering/tlg-catalog/blob/b3019fec92280384bac322680face57b2f685bfc/book/graphs/other/mng01.qmd
#' @measure
lab_over_time_plot <- function(
  parameter = c("ALT", "CRP", "IGA"),
  population = c("SAF", "ITT"),
  adsl = adam_data()$adsl,
  adlb = adam_data()$adlb
) {
  parameter <- measure_choice(
    parameter,
    c("ALT", "CRP", "IGA"),
    "parameter"
  )
  adsl <- filter_population(adsl, population) |>
    tern::df_explicit_na()

  visit_levels <- adlb |>
    dplyr::distinct(AVISIT, AVISITN) |>
    dplyr::arrange(AVISITN) |>
    dplyr::pull(AVISIT) |>
    as.character()

  adlb <- adlb |>
    dplyr::semi_join(adsl, by = "USUBJID") |>
    dplyr::mutate(
      AVISIT = factor(as.character(AVISIT), levels = visit_levels)
    ) |>
    dplyr::filter(
      ANL01FL == "Y",
      ONTRTFL == "Y",
      PARAMCD == parameter,
      AVISIT != "SCREENING"
    ) |>
    droplevels() |>
    tern::df_explicit_na()

  require_plot_data(
    adlb,
    "No on-treatment laboratory analysis records match this parameter and population."
  )

  tern::g_lineplot(
    df = adlb,
    alt_counts_df = adsl,
    variables = tern::control_lineplot_vars(group_var = "ACTARM"),
    subtitle = "Laboratory Test:"
  )
}

catalog_boxplot <- function(data, y = "AVAL", title, subtitle, y_label) {
  fill_color <- "#eaeef5"
  outline_color <- getOption("ggplot2.discrete.fill")[[1]]

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[["ARMCD"]], y = .data[[y]])
  ) +
    ggplot2::stat_summary(
      geom = "boxplot",
      fun.data = catalog_five_num,
      fill = fill_color,
      color = outline_color
    ) +
    ggplot2::stat_summary(
      geom = "point",
      fun = mean,
      size = 3,
      shape = 8
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = "The whiskers extend to the minimum and maximum values.",
      x = "Treatment Group",
      y = y_label
    ) +
    catalog_boxplot_theme()

  catalog_boxplot_annotations(plot, outline_color)
}

catalog_five_num <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1)) {
  result <- stats::quantile(x, probs)
  names(result) <- c("ymin", "lower", "middle", "upper", "ymax")
  result
}

catalog_boxplot_theme <- function() {
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0),
    plot.subtitle = ggplot2::element_text(hjust = 0),
    plot.caption = ggplot2::element_text(hjust = 0),
    panel.background = ggplot2::element_rect(
      fill = "white",
      color = "grey50"
    )
  )
}

catalog_boxplot_annotations <- function(plot, color, annos = 1) {
  metadata <- ggplot2::ggplot_build(plot)$data[[1]]

  plot <- plot +
    ggplot2::geom_segment(
      data = metadata,
      ggplot2::aes(
        x = xmin + (xmax - xmin) / 4,
        xend = xmax - (xmax - xmin) / 4,
        y = ymax,
        yend = ymax
      ),
      linewidth = 0.5,
      color = color
    ) +
    ggplot2::geom_segment(
      data = metadata,
      ggplot2::aes(
        x = xmin + (xmax - xmin) / 4,
        xend = xmax - (xmax - xmin) / 4,
        y = ymin,
        yend = ymin
      ),
      linewidth = 0.5,
      color = color
    )

  if (annos != 1) {
    plot <- plot +
      ggplot2::geom_segment(
        data = metadata,
        ggplot2::aes(
          x = xmin,
          xend = xmax,
          y = middle,
          yend = middle
        ),
        linewidth = 0.5,
        color = color
      )
  }

  plot
}

require_plot_data <- function(data, message, call = rlang::caller_env()) {
  if (nrow(data) == 0) {
    cli::cli_abort(message, call = call)
  }
  invisible(data)
}
