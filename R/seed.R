
#' Use a session with a random seed
#'
#' Set various random seeds required to ensure reproducible results. The
#' provided `seed` value will establish a new random seed for R, Python, NumPy,
#' and TensorFlow. GPU computations and CPU parallelism will also be disabled by
#' default.
#'
#'
#' @param seed A single value, interpreted as an integer
#' @param disable_gpu `TRUE` to disable GPU execution (see *Parallelism* below).
#' @param disable_parallel_cpu `TRUE` to disable CPU parallelism (see
#'   *Parallelism* below).
#' @param quiet `TRUE` to suppress printing of messages.
#'
#' @details This function must be called at the very top of your script (i.e.
#'   immediately after `library(tensorflow)`, `library(keras)`, etc.). Any
#'   existing TensorFlow session is torn down via `tf$reset_default_graph()`.
#'
#'   This function takes all measures known to promote reproducible results from
#'   TensorFlow sessions, however it's possible that various individual
#'   TensorFlow features or dependent libraries escape its effects. If you
#'   encounter non-reproducible results please investigate the possible sources
#'   of the problem, contributions via pull request are very welcome!
#'
#' @section Parallelism: By default the `use_session_with_seed()` function
#'   disables GPU and CPU parallelism, since both can result in
#'   non-deterministic execution patterns (see
#'   <https://stackoverflow.com/questions/42022950/>). You can optionally enable
#'   GPU or CPU parallelism by setting the `disable_gpu` and/or
#'   `disable_parallel_cpu` parameters to `FALSE`.
#'
#' @return TensorFlow session object, invisibly
#'
#' @details Packages which need to be notified before and after the seed is set
#'   can register for the "tensorflow.on_before_use_session" and
#'   "tensorflow.on_use_session" hooks (see [setHook()]) for additional
#'   details on hooks).
#'
#' @examples
#' \dontrun{
#' library(tensorflow)
#' use_session_with_seed(42)
#' }
#'
#' @export
use_session_with_seed <- function(seed,
                                  disable_gpu = TRUE,
                                  disable_parallel_cpu = TRUE,
                                  quiet = FALSE) {


  msg <- "use_session_with_seed will be deprecated in the future. use tensorflow::set_random_seed instead."
  if (tf_version() >= "2.0") {
    tf <- tf$compat$v1
    warning(msg)
  }

  if (tf_version() >= "2.3")
    stop(msg)

  # cast seed to integer
  seed <- as.integer(seed)

  # call hook (returns TRUE if TF seed should be set, this allows users to
  # call this function even when using front-end packages like keras that
  # may not use TF as their backend)
  using_tf <- call_hook("tensorflow.on_before_use_session", quiet)

  # destroy existing session call before hook
  if (using_tf)
    tf$reset_default_graph()

  # note what has been disabled
  disabled <- character()

  # disable CUDA if requested
  if (disable_gpu) {
    Sys.setenv(CUDA_VISIBLE_DEVICES = "-1")
    disabled <- c(disabled, "GPU")
  }

  # set R random seed
  set.seed(seed)

  # set Python/NumPy random seed
  py_set_seed(seed)

  # TF if we are using tf
  if (using_tf) {
    # Force TensorFlow to use single thread as multiple threads are a potential
    # source of non-reproducible results. For further details, see:
    # https://stackoverflow.com/questions/42022950/which-seeds-have-to-be-set-where-to-realize-100-reproducibility-of-training-res

    # disable parallelism if requested
    config <- list()
    if (disable_gpu) {
      config$device_count <-  list(gpu = 0L)
    }
    if (disable_parallel_cpu) {
      config$intra_op_parallelism_threads <- 1L
      config$inter_op_parallelism_threads <- 1L
      disabled <- c(disabled, "CPU parallelism")
    }
    session_conf <- do.call(tf$ConfigProto, config)

    # The below tf$set_random_seed() will make random number generation in the
    # TensorFlow backend have a well-defined initial state. For further details,
    # see: https://www.tensorflow.org/api_docs/python/tf/set_random_seed
    tf$set_random_seed(seed)

    # create session
    sess <- tf$Session(graph = tf$get_default_graph(), config = session_conf)

  } else {
    sess <- NULL
  }

  # show message
  msg <- paste("Set session seed to", seed)
  if (length(disabled) > 0)
    msg <- paste0(msg, " (disabled ", paste(disabled, collapse = ", "), ")")
  if (!quiet)
    message(msg)

  # call after hook
  call_hook("tensorflow.on_use_session", sess, quiet)

  # return  session invisibly
  invisible(sess)
}

#' Set random seed for TensorFlow
#'
#' Sets all random seeds needed to make TensorFlow code reproducible.
#'
#' @details
#'
#' This function should be used instead of [use_session_with_seed()] if
#' you are using TensorFlow >= 2.0, as the concept of `session` doesn't
#' really make sense anymore.
#'
#' This functions sets:
#'
#' - The R random seed with [set.seed()].
#' - The python and Numpy seeds via ([reticulate::py_set_seed()]).
#' - The TensorFlow seed with (`tf$random$set_seed()`)
#'
#' It also optionally disables the GPU execution as this is a potential
#' source of non-reproducibility.
#'
#' @param seed A single value, interpreted as an integer
#' @param disable_gpu `TRUE` to disable GPU execution (see *Parallelism* below).
#'
#' @export
set_random_seed <- function(seed, disable_gpu = TRUE) {

  if(is.null(tf_ver <- tf_version()))
    stop("TensorFlow not installed, please run `tensorflow::install_tensorflow()`")
  else if (tf_ver < "2.0")
    stop("set_random_seed() only works for TF >= 2.0")

  # cast seed to integer
  seed <- as.integer(seed)

  # set R random seed
  set.seed(seed)

  # set Python/NumPy random seed
  py_set_seed(seed)

  # set tensorflow random seed
  tensorflow::tf$random$set_seed(seed)

  if (disable_gpu) {
    Sys.setenv(CUDA_VISIBLE_DEVICES = "-1")
  }

  invisible(NULL)
}
