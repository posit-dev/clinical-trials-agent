test_that("graph measures return renderable ggplots", {
  measure_names <- c(
    "ae_by_soc_plot",
    "exposure_by_arm_plot",
    "age_by_arm_plot",
    "lab_by_arm_plot",
    "lab_over_time_plot"
  )

  for (measure_name in measure_names) {
    plot <- measure_env[[measure_name]]()

    expect_s3_class(plot, "ggplot")
    expect_no_error(ggplot2::ggplot_build(plot))
  }
})

test_that("adverse-event plot uses arm population denominators", {
  plot_data <- measure_env$ae_by_soc_plot()$data
  adam <- adam_data()

  expected <- adam$adae |>
    dplyr::filter(ANL01FL == "Y") |>
    dplyr::distinct(USUBJID, ACTARM, AEBODSYS) |>
    dplyr::count(ACTARM, AEBODSYS, name = "n") |>
    dplyr::left_join(
      dplyr::count(
        dplyr::filter(adam$adsl, SAFFL == "Y"),
        ACTARM,
        name = "N_arm"
      ),
      by = "ACTARM"
    ) |>
    dplyr::mutate(pct = round((n / N_arm) * 100, 2))

  expect_equal(
    dplyr::arrange(plot_data, ACTARM, AEBODSYS),
    dplyr::arrange(expected, ACTARM, AEBODSYS)
  )
})

test_that("catalog box plots use observed extrema and show the mean", {
  plot <- measure_env$lab_by_arm_plot()
  built <- ggplot2::ggplot_build(plot)
  source_data <- plot$data

  expected <- source_data |>
    dplyr::group_by(ARMCD) |>
    dplyr::summarise(
      ymin = min(AVAL),
      ymax = max(AVAL),
      mean = mean(AVAL),
      .groups = "drop"
    )

  box_data <- built$data[[1]] |>
    dplyr::arrange(x)
  mean_data <- built$data[[2]] |>
    dplyr::arrange(x)

  expect_equal(box_data$ymin, expected$ymin)
  expect_equal(box_data$ymax, expected$ymax)
  expect_equal(mean_data$y, expected$mean)
})

test_that("lab graph arguments are validated", {
  expect_snapshot(
    measure_env$lab_by_arm_plot(parameter = "BAD"),
    error = TRUE
  )
})
