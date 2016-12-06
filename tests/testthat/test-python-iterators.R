context("iterators")

# some helpers
test <- import("tftools.test")

test_that("Iterators reflect values back", {
  rlist <- list("foo", "bar", 42L)
  expect_equal(iterate(test$makeIterator(rlist)), rlist)
  a <- test$makeIterator(rlist)
  b <- iterate(a)
  expect_equal(b, rlist)
})


test_that("Generators reflect values back", {
  expect_equal(as.integer(iterate(test$makeGenerator(5))) + 1L, seq(5))
  # Test iterate() returns
  a <- test$makeGenerator(5)
  b <- as.integer(iterate(a))
  expect_equal(b + 1L, seq(5))
})


