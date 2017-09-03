#' Set Random Seed
#' 
#' @param seed The random seed number
#' 
#' @export
#' @examples 
#' set_random_seed(42)
set_random_seed <- function(seed = 42) {
  # Ensure reproducibility for certain hash-based operations for Python 3
  # References: https://docs.python.org/3.4/using/cmdline.html#envvar-PYTHONHASHSEED
  #             https://github.com/fchollet/keras/issues/2280#issuecomment-306959926
  if (identical(substring(tf_config()$python_version, 0, 1), "3")) {
    os$environ[["PYTHONHASHSEED"]] <- "0"
  }
  
  seed <- as.integer(seed)
  np$random$seed(seed)
  random$seed(seed)
  tf$set_random_seed(seed)
}

#' Get TensorFlow Session with Random Seed
#' 
#' This function returns a TensorFlow Session with the given seed, particularly useful when
#' your TensorFlow program needs to give reproducible results. Note that this session forces to use
#' only single thread since multiple threads are a potential source of non-reproducible results.
#' For further details, see: https://stackoverflow.com/questions/42022950/which-seeds-have-to-be-set-where-to-realize-100-reproducibility-of-training-res
#' 
#' @param seed The random seed number
#' @return The TensorFlow Session object with the specified random seed
#' 
#' @export
#' 
#' @examples 
#' sess <- tf_session_with_seed()
#' sess$run(tf$constant(1L) + tf$constant(2L))
tf_session_with_seed <- function(seed = 42) {
  session_conf <- tf$ConfigProto(intra_op_parallelism_threads = 1L, inter_op_parallelism_threads = 1L)
  set_random_seed(seed)
  sess <- tf$Session(graph = tf$get_default_graph(), config = session_conf)
  invisible(sess)
}
