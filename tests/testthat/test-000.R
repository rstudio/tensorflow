test_that("TF is working.", {
  expect_true(!is.null(tensorflow::tf_version()))
})
