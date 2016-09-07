

#' @export
tf.Session <- R6Class("tf.Session",
  inherit = PyObject,
  public = list(
    initialize = function(target = "", graph = NULL, config = NULL) {
      private$instance <- tf$Session(target, graph, config)
    },
    run = function(fetches, feed_dict=NULL, options=NULL, run_metadata=NULL) {
      "Runs operations and evaluates tensors in fetches."
      private$instance$run(fetches, feed_dict, options, run_metadata)
    },
    close = function() {
      "Closes this session."
      private$instance$close()
    }
  )
)

#' @export
tf.InteractiveSession <- R6Class("tf.InteractiveSession",
  inherit = tf.Session,
  public = list(
    initialize = function(target = "", graph = NULL, config = NULL) {
      private$instance = tf$InteractiveSession(target, graph, config)
    }
  )
)

#' @export
tf.initialize_all_variables <- function() {
  tf$initialize_all_variables()
}



