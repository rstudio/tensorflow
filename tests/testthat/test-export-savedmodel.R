context("Save")

train_mnist_graph <- function(sess) {

  IPython <- IPython <- reticulate::import("IPython")
  py_capture_output <- IPython$utils$capture$capture_output

  with(py_capture_output(), {
    datasets <- tf$contrib$learn$datasets
    mnist <- datasets$mnist$read_data_sets("MNIST-data", one_hot = TRUE)
  })

  if (tf_version() >= "1.14")
    placeholder <- tf$compat$v1$placeholder
  else
    placeholder <- tf$placeholder

  x <- placeholder(tf$float32, shape(NULL, 784L))

  W <- tf$Variable(tf$zeros(shape(784L, 10L)))
  b <- tf$Variable(tf$zeros(shape(10L)))

  y <- tf$nn$softmax(tf$matmul(x, W) + b)

  y_ <- placeholder(tf$float32, shape(NULL, 10L))
  cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_ * log(y), reduction_indices = 1L))

  if (tf_version() >= "1.14")
    optimizer <- tf$compat$v1$train$GradientDescentOptimizer(0.5)
  else
    optimizer <- tf$train$GradientDescentOptimizer(0.5)

  train_step <- optimizer$minimize(cross_entropy)

  if (tf_version() >= "1.14")
    init <- tf$compat$v1$global_variables_initializer()
  else
    init <- tf$global_variables_initializer()

  sess$run(init)

  for (i in 1:1000) {
    batches <- mnist$train$next_batch(100L)
    batch_xs <- batches[[1]]
    batch_ys <- batches[[2]]
    sess$run(train_step,
             feed_dict = dict(x = batch_xs, y_ = batch_ys))
  }

  correct_prediction <- tf$equal(tf$argmax(y, 1L), tf$argmax(y_, 1L))
  accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))

  sess$run(accuracy, feed_dict = dict(x = mnist$test$images, y_ = mnist$test$labels))

  list(input = x, output = y)
}

# TODO: consider testing in a new R session with tf$compat$v1$disable_eager_execution()
# skip("Don't have sessions to export when running eager.")
if (!tf$executing_eagerly())
test_that("export_savedmodel() works with MNIST", {
  skip_if_no_tensorflow()

  temp_path <- tempfile()

  if (tf_version() >= "1.14")
    sess <- tf$compat$v1$Session()
  else
    sess <- tf$Session()

  tensors <- train_mnist_graph(sess)

  export_savedmodel(
    sess,
    temp_path,
    inputs = list(images = tensors$input),
    outputs = list(scores = tensors$output)
  )

  expect_true(file.exists(file.path(temp_path, "saved_model.pb")))

})
