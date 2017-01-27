#' @param response The name of the response vector (as a length-one character
#'   vector), or a formula, giving a symbolic description of the model to be
#'   fitted. When \code{response} is a formula, it is used in preference to other
#'   parameters to set the \code{response}, \code{features}, and \code{intercept}
#'   parameters (if available). Currently, only simple linear combinations of
#'   existing parameters is supposed; e.g. \code{response ~ feature1 + feature2 + ...}.
#'   The intercept term can be omitted by using \code{- 1} in the model fit.
