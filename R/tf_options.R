#' Options for TensorFlow Routines
#'
#' Used to control the various facets of layers constructed with the
#' modeling routines included in this package.
#'
#' @param optimizer The optimizer to be used.
#' @param initialize.session A function used to create an initialize a new
#'   TensorFlow session.
#' @param run.optimizer A function used to run the model. Should accept the parameters
#'   \code{session}, \code{cost}, \code{optimizer} and \code{feed.dict}, and repeatedly
#'   run the optimizer until the cost function has been appropriately minimized.
#'
#' @export
tf_options <- function(optimizer = NULL,
                       initialize.session = NULL,
                       run.optimizer = NULL)
{
  optimizer <- optimizer %||% tf$train$GradientDescentOptimizer(0.05)
  run.optimizer <- run.optimizer %||% tf_default_run_optimizer
  initialize.session <- initialize.session %||% tf_default_initialize_session

  options <- list(
    optimizer = optimizer,
    initialize.session = initialize.session,
    run.optimizer = run.optimizer
  )

  class(options) <- "tf_options"
  options
}

tf_default_initialize_session <- function() {
  init <- tf$global_variables_initializer()
  session <- tf$Session()
  session$run(init)
  session
}

tf_default_run_optimizer <- function(session,
                                     cost,
                                     optimizer,
                                     feed.dict)
{
  epsilon <- .Machine$double.eps
  last_cost <- Inf
  iterations <- 1
  while (iterations < 10000) {
    session$run(optimizer, feed_dict = feed.dict)
    current_cost <- session$run(cost, feed_dict = feed.dict)
    if (last_cost - current_cost < epsilon) break
    last_cost <- current_cost
    iterations <- iterations + 1
  }
  message("* Optimization complete after ", iterations, " iterations.")
}
