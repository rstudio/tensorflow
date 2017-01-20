#' TensorFlow -- Linear Regression
#'
#' Perform linear regression using TensorFlow.
#'
#' @template roxlate-tf-x
#' @template roxlate-tf-response
#' @template roxlate-tf-features
#' @template roxlate-tf-intercept
#' @template roxlate-tf-dots
#' @template roxlate-tf-options
#'
#' @export
tf_linear_regression <- function(x,
                                 response,
                                 features,
                                 intercept = TRUE,
                                 tf.options = tf_options(),
                                 ...)
{
  tf_backwards_compatibility_api()
  tf_prepare_response_features_intercept(x, response, features, intercept)

  # Extract response vector and convert to one-dimensional matrix
  rv <- as.matrix(x[response], ncol = 1L)

  # Extract features
  k <- length(features)
  fv <- as.matrix(x[features], ncol = k)

  # Construct tensorflow placeholders
  X <- tf$placeholder("float", shape = shape(NULL, k))
  Y <- tf$placeholder("float")

  # Construct model specification.
  W <- tf$Variable(tf$zeros(shape(k, 1L)))
  b <- if (intercept) {
    tf$Variable(tf$zeros(shape(1L)))
  }

  # Define how estimates of Y are produced
  Y_hat <- if (intercept) {
    tf$add(tf$matmul(X, W), b)
  } else {
    tf$matmul(X, W)
  }

  # Define cost, and assign optimizer for that cost
  cost <- tf$reduce_mean(tf$square(tf$subtract(Y_hat, Y)))
  optimizer <- tf.options$optimizer$minimize(cost)

  # Initialize session
  initialize.session <- tf.options$initialize.session
  session <- initialize.session()

  # Run the optimizer
  run.optimizer <- tf.options$run.optimizer
  feed.dict <- dict(X = fv, Y = rv)
  run.optimizer(session = session,
                cost = cost,
                optimizer = optimizer,
                feed.dict = feed.dict)

  # Extract coefficients from model fit
  if (intercept) {
    coefficients <- c(session$run(b), c(session$run(W)))
    names(coefficients) <- c("(Intercept)", colnames(fv))
  } else {
    coefficients <- session$run(W)
    names(coefficients) <- colnames(fv)
  }

  tf_model(
    "linear_regression",
    session = session,
    cost = cost,
    optimizer = optimizer,
    feed.dict = feed.dict,
    coefficients = coefficients
  )

}
