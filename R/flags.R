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

#' @examples
#' \dontrun{
#' # examine an example configuration file provided by tensorflow
#' file <- system.file("examples/config/flags.yml", package = "tensorflow")
#' cat(readLines(file), sep = "\n")
#'
#' # read the default configuration
#' FLAGS <- tensorflow::parse_flags("default", file = file)
#' str(FLAGS)
#'
#' # read the alternate configuration: note that
#' # the default configuration is inherited, but
#' # we override the 'string' configuration here
#' FLAGS <- tensorflow::parse_flags("alternate", file = file)
#' str(FLAGS)
#'
#' # override configuration values using command
#' # line arguments (normally, these would be
#' # passed in through the command line invocation
#' # used to start the process)
#' FLAGS <- tensorflow::parse_flags(
#'   "alternate",
#'   file = file,
#'   arguments = c("--foo=1")
#' )
#' str(FLAGS)
#'
#' }
#' @export
parse_flags <-
  function(config = Sys.getenv("R_CONFIG_ACTIVE", unset = "default"),
           file = "flags.yml",
           arguments = commandArgs(TRUE))
{
  flags <- list()

  # warn if the user has supplied a 'file' argument but no such file exists
  if (!missing(file) && !file.exists(file))
    warning(sprintf("configuration file '%s' does not exist", file))

  # read configuration file if it does exist
  if (file.exists(file))
    flags <- config::get(config = config, file = file)

  # backwards compatibility -- if the user is using the
  # TensorFlow FLAGS system for handling their
  # configuration, then explicitly read the configuration
  # from that; otherwise run our own parser
  actions <- tf$app$flags$`_global_parser`$`_actions`
  if (length(actions) > 1) {
    flags <- config::merge(flags, parse_tensorflow_flags(arguments))
  } else {
    flags <- config::merge(flags, parse_arguments(arguments))
  }

  # return generated config
  flags
}

parse_tensorflow_flags <- function(args = commandArgs(TRUE)) {

  # parse known arguments using the global parser
  parser <- tf$app$flags$`_global_parser`
  result <- tryCatch(
    parser$parse_known_args(as.list(args)),
    error = function(e) NULL
  )

  # check for error (means user invoked --help)
  if (is.null(result)) {
    if (interactive())
      return(NULL)
    else
      quit(save = "no")
  }

  # return parsed flags as named R list
  result[[1]]$`__dict__`
}

