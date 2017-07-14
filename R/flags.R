#' Parse Configuration Flags for a TensorFlow Application
#'
#' Parse configuration flags for a TensorFlow application. Use
#' this to parse and unify the configuration(s) specified through
#' a `flags.yml` configuration file, alongside other arguments
#' set through the command line.
#'
#' @param config The configuration to use. Defaults to the
#'   active configuration for the current environment (as
#'   specified by the `R_CONFIG_ACTIVE` environment
#'   variable), or `default` when unset.
#' @param file The configuration file to read.
#' @param arguments The command line arguments (as a
#'   character vector) to be parsed.
#'
#' @return A named \R list, mapping configuration keys to values.
#'
#' @export
#' @example examples/examples-flags.R
parse_flags <-
  function(config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
           file = "flags.yml",
           arguments = commandArgs(TRUE))
{
  # backwards compatibility -- if the user is using the
  # TensorFlow FLAGS system for handling their
  # configuration, then explicitly read the configuration
  # from that. we wrap in a try block just in case future
  # versions of TensorFlow deprecate or remove this API
  try(silent = TRUE, {

    # read parser actions that have been registered
    actions <- tf$app$flags$`_global_parser`$`_actions`

    # by default, TensorFlow provides the '--help' action to
    # the argument parser. if anything else is seen there,
    # then assume the user has opted in to using TensorFlow's
    # FLAGS
    if (length(actions) > 1)
      return(parse_tensorflow_flags())
  })

  # read configuration file
  flags <- config::get(config = config, file = file)

  # merge parsed command line arguments
  flags <- config::merge(flags, parse_arguments(arguments))

  # return generated config
  flags
}

parse_tensorflow_flags <- function(arguments = commandArgs(TRUE)) {

  # get R command line arguments
  args <- commandArgs(trailingOnly = TRUE)

  # parse known arguments using the global parser
  parser <- tf$app$flags$`_global_parser`
  result <- tryCatch(parser$parse_known_args(list(args)),
                     error = function(e) NULL)

  # check for error (means user invoked --help)
  if (is.null(result)) {
    if (interactive())
      return(NULL)
    else
      quit(save = "no")
  }

  # set flags from result
  FLAGS <- tf$app$flags$FLAGS
  result <- result[[1]]$`__dict__`
  for (name in names(result))
    FLAGS$`__setattr__`(name, result[[name]])

  # return the FLAGS
  invisible(FLAGS)
}

