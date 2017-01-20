tf_model <- function(name, ...) {
  object <- list(...)
  class(object) <- c("tf_model", sprintf("tf_model_%s", name))
  object
}

tf_backwards_compatibility_api <- function(envir = parent.frame()) {

  # retrieve dots
  dots <- eval(quote(list(...)), envir = envir)

  # if 'x' is a formula, and 'data' exists, then update
  # 'response' and 'x' in parent frame as appropriate
  if (is.formula(envir[["x"]]) && !is.null(dots[["data"]])) {
    assign("response", envir[["x"]], envir = envir)
    assign("x", dots[["data"]], envir = envir)
  }
}

tf_prepare_response_features_intercept <- function(x,
                                                   response,
                                                   features,
                                                   intercept,
                                                   envir = parent.frame())
{
  # if 'x' is a formula, and the 'data' argument has been supplied,
  # respect that

  # extract response from parent frame
  if (is.formula(response)) {
    parsed <- parse_formula(response, data = x)
    response <- parsed$response
    features <- parsed$features
    if (is.logical(parsed$intercept)) intercept <- parsed$intercept
  }

  # ensure types
  response <- ensure_scalar_character(response)
  features <- as.character(features)
  intercept <- ensure_scalar_boolean(intercept)

  # mutate in environment
  assign("response", response, envir = envir)
  assign("features", features, envir = envir)
  assign("intercept", intercept, envir = envir)

  x
}
