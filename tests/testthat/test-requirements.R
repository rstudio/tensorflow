test_that("TensorFlow requirements allow NumPy 2 on Windows", {
  requirements <- callr::r(function() {
    library(tensorflow)
    testthat::local_mocked_bindings(
      is_linux = function() FALSE,
      is_mac_arm64 = function() FALSE,
      is_windows = function() TRUE,
      .package = "tensorflow"
    )

    reticulate::py_require("numpy>=2", python_version = "3.13", action = "set")
    tensorflow::py_require_tensorflow()
    reticulate::py_require()
  })

  expect_setequal(requirements$packages, c("numpy>=2", "tensorflow"))
  expect_identical(requirements$python_version, "3.13")
})
