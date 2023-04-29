test_that("cast List wrapers", {
  model <- tf$keras$models$Sequential(list())
  model$denses <- list(tf$keras$layers$Dense(10L),
                       tf$keras$layers$Dense(10L))

  model$denses_dict <- list(abc = tf$keras$layers$Dense(10L),
                            def = tf$keras$layers$Dense(10L))

  expect_true(is.list(model$denses))
  expect_true(length(model$denses) == 2)

  expect_true(is.list(model$denses_dict))
  expect_true(length(model$denses_dict) == 2)
  expect_named(model$denses_dict, c("abc", "def"))
})
