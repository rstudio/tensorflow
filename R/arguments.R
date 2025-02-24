#' Parse Command Line Arguments
#'
#' Parse command line arguments of the form `--key=value` and
#' `--key value`. The values are assumed to be valid `yaml` and
#' will be converted using [yaml::yaml.load()].
#'
#' @param arguments A vector of command line arguments. When
#'   `NULL` (the default), the command line arguments received
#'   by the current \R process are used.
#'
#' @export
parse_arguments <- function(arguments = NULL) {
  arguments <- arguments %||% commandArgs(TRUE)

  # initialize some state
  values <- list()

  i <- 0; n <- length(arguments)
  while (i < n) {
    i <- i + 1
    argument <- arguments[[i]]

    # skip any command line arguments without a '--' prefix
    if (!grepl("^--", argument))
      next

    # check to see if an '=' was specified for this argument
    equals_idx <- regexpr("=", argument)
    if (identical(c(equals_idx), -1L)) {
      # no '='; the next argument is the value for this key
      key <- substring(argument, 3)
      val <- arguments[[i + 1]]
      i <- i + 1
    } else {
      # found a '='; the next argument is all the text following
      # that character
      key <- substring(argument, 3, equals_idx - 1)
      val <- substring(argument, equals_idx + 1)
    }

    # convert '-' to '_' in key
    key <- gsub("-", "_", key)

    # update our map of argument values
    values[[key]] <- yaml::yaml.load(val)
  }

  values

}
