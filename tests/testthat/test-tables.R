test_that("table measures return model, display, and computation values", {
  measure_names <- c(
    "ae_overview",
    "disposition",
    "exposure_summary",
    "demography",
    "ae_by_soc_pt",
    "ae_by_grade",
    "deaths",
    "lab_abnormalities",
    "ae_by_intensity",
    "ae_frequent_by_grade",
    "ae_by_sex",
    "ae_related",
    "ae_most_frequent",
    "ae_incidence_rate"
  )

  for (measure_name in measure_names) {
    result <- suppressWarnings(measure_env[[measure_name]]())

    expect_s7_class(result, ellmer::ContentToolResult)
    expect_s3_class(result@value, "data.frame")
    expect_s3_class(result@extra$display$html, "shiny.tag")
    expect_true(methods::is(result@extra$data, "VTableTree"))
    expect_true(result@extra$display$open)
    expect_true(result@extra$display$full_screen)
  }
})

test_that("frequent adverse events preserve grouped column headers", {
  result <- measure_env$ae_frequent_by_grade()
  html <- as.character(result@extra$display$html)

  expect_match(html, "Any Grade (%)", fixed = TRUE)
  expect_match(html, "Grade 3-4 (%)", fixed = TRUE)
  expect_match(html, "Grade 5 (%)", fixed = TRUE)
  expect_match(html, "colspan=", fixed = TRUE)
})
