#' Configure an Application
#'
#' Configure a TensorFlow application.
#'
#' @note
#'
#' The `tensorflow.config.hook` \R option can be set if custom transformations
#' of the generated configuration file is desired.
#'
#' @param config The configuration to use. Defaults to the active configuration
#'   for the current environment (as specified by the `R_CONFIG_ACTIVE` environment
#'   variable), or `default` when unset.
#' @param file The configuration file to read.
#' @param arguments The command line arguments to be parsed.
#'
#' @return A named \R list, mapping configuration keys to values.
#'
#' @export
#' @example examples/examples-config.R
config <- function(config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
                   file = "config.yml",
                   arguments = commandArgs(TRUE))
{
  # read configuration file
  config <- config::get(config = config, file = file)

  # merge parsed command line arguments
  config <- config::merge(config, parse_arguments(arguments))

  # return generated config
  config
}
