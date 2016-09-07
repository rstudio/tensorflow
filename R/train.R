
#' @export
tf.train.Optimizer <- R6Class("tf.train.Optimizer",
  inherit = PyObject,
  public = list(
    initialize = function(optimizer) {
      private$instance <- optimizer
    },
    minimize = function(loss, global_step=NULL, var_list=NULL,
                        gate_gradients=1L, aggregation_method=NULL,
                        colocate_gradients_with_ops=FALSE, name=NULL,
                        grad_loss=NULL) {
      "Add operations to minimize loss by updating var_list."
      private$instance$minimize(
        loss, global_step = global_step, var_list = var_list,
        gate_gradients = as.integer(gate_gradients),
        aggregation_method = aggregation_method,
        colocate_gradients_with_ops = colocate_gradients_with_ops,
        name = name, grad_loss = grad_loss)
    }
  )
)

#' @export
tf.train.GradientDescentOptimizer <- R6Class("tf.train.GradientDescentOptimizer",
  inherit = tf.train.Optimizer,
  public = list(
   initialize = function(learning_rate, use_locking=FALSE, name="GradientDescent") {
     optimizer <- tf$train$GradientDescentOptimizer(learning_rate,
                                                    use_locking = use_locking,
                                                    name = name)
     super$initialize(optimizer)
   }
  )
)

