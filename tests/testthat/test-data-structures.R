test_that("cast List wrapers", {
  model <- tf$keras$models$Sequential(list())
  model$denses <- list(tf$keras$layers$Dense(10), tf$keras$layers$Dense(10))

  expect_true(is.list(model$denses))
})
