is.formula <- function(x) {
  inherits(x, "formula")
}

parse_formula <- function(formula, data = NULL) {
  formula <- validate_formula(formula)
  n <- length(formula)

  # extract response
  response <- if (n == 3) {
    lhs <- formula[[2]]
    if (!(is.symbol(lhs) || is.character(lhs)))
      stop("expected symbolic response; got '", lhs, "'")
    as.character(lhs)
  }

  # extract features
  terms <- stats::terms(formula, data = data)
  features <- attr(terms, "term.labels")
  intercept <- as.logical(attr(terms, "intercept"))

  list(features = features,
       response = response,
       intercept = intercept)
}

validate_formula <- function(formula) {
  formula <- stats::as.formula(formula)
  for (i in 2:length(formula))
    validate_formula_operators(formula[[i]])
  formula
}

validate_formula_operators <- function(object) {
  n <- length(object)
  if (is.call(object) && n > 0) {

    # check that this is a call to a known operator
    op <- object[[1]]
    if (!is.symbol(op))
      stop("expected a symbol for call; got '", deparse(op), "'")

    ch <- as.character(op)
    if (!ch %in% c("+", "-", "("))
      stop("unhandled formula operator: expected '+' or '-'; got '", ch, "'")

    # validate the rest of the calls
    for (i in 1:n)
      validate_formula_operators(object[[i]])
  }
}
