context("Arguments")

expect_arguments <- function(arguments, expected) {
  splat <- strsplit(arguments, "[[:space:]]+")[[1]]
  parsed <- parse_arguments(splat)
  expect_equal(parsed, expected)
}

test_that("sample command line arguments are parsed as expected", {

  expect_arguments(
    "--alpha=beta --gamma=delta",
    list(alpha = "beta", gamma = "delta")
  )

  expect_arguments(
    "--alpha beta --gamma delta",
    list(alpha = "beta", gamma = "delta")
  )

  expect_arguments(
    "--nested=a=b",
    list(nested = "a=b")
  )

  expect_arguments(
    "--array=[1,2,3]",
    list(array = c(1, 2, 3))
  )

  expect_arguments(
    "--number=1000",
    list(number = 1000)
  )

  expect_arguments(
    "--nested-dashes=1000",
    list(nested_dashes = 1000)
  )

})
