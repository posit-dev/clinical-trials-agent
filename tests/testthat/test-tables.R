test_that("complex table measures return rich rtables output", {
  measure_names <- c(
    "ae_by_soc_pt",
    "ae_frequent_by_grade",
    "ae_by_sex"
  )

  for (measure_name in measure_names) {
    result <- measure_env[[measure_name]]()

    expect_s3_class(result, "commons_rich_table")
    expect_s4_class(result$value, "TableTree")
    expect_type(result$html, "character")
    expect_length(result$html, 1)
    expect_match(result$html, "<table", fixed = TRUE)
    expect_identical(result$model_content, result$html)
  }
})

test_that("frequent adverse events preserve grouped column headers", {
  result <- measure_env$ae_frequent_by_grade()

  expect_match(result$html, "Any Grade (%)", fixed = TRUE)
  expect_match(result$html, "Grade 3-4 (%)", fixed = TRUE)
  expect_match(result$html, "Grade 5 (%)", fixed = TRUE)
  expect_match(result$html, "colspan=", fixed = TRUE)
})
