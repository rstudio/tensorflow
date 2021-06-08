context("examples")


# some helpers
run_example <- function(example) {
  env <- new.env()
  capture.output({
    example_path <- system.file("examples", example, package = "tensorflow")
    old_wd <- setwd(dirname(example_path))
    on.exit(setwd(old_wd), add = TRUE)
    source(basename(example_path), local = env)
  }, type = "output")
  rm(list = ls(env), envir = env)
  gc()
}

examples <- if (nzchar(Sys.getenv("TENSORFLOW_TEST_EXAMPLES"))) {
  examples <-   c("hello.R",
                  "introduction.R",
                  "mnist/mnist_softmax.R",
                  "mnist/fully_connected_feed.R",
                  "regression/tensorflow_linear_regression.R")

  if (tf_version() >= "2.0") {
    # disable examples since tf_compat() requires session restart
    examples <- NULL
  }

  examples
}

for (example in examples) {
  test_that(paste(example, "example runs successfully"), {
    skip_if_no_tensorflow()
    expect_error(run_example(example), NA)
  })
}

