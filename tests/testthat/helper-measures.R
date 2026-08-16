project_root <- normalizePath(
  file.path(testthat::test_path(), "..", ".."),
  mustWork = TRUE
)
old_working_directory <- setwd(project_root)

measure_env <- new.env(parent = globalenv())
sys.source("measures/helpers.R", envir = measure_env)
sys.source("measures/graphs.R", envir = measure_env)
sys.source("measures/tables.R", envir = measure_env)

setwd(old_working_directory)
