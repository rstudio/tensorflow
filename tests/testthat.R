library(testthat)
library(tensorflow)

if (identical(Sys.getenv("NOT_CRAN"), "true"))
  test_check("tensorflow")
