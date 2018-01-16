context("Save")

train_mnist <- function(sess) {
  datasets <- tf$contrib$learn$datasets
  mnist <- datasets$mnist$read_data_sets("MNIST-data", one_hot = TRUE)

  x <- tf$placeholder(tf$float32, shape(NULL, 784L))

  W <- tf$Variable(tf$zeros(shape(784L, 10L)))
  b <- tf$Variable(tf$zeros(shape(10L)))

  y <- tf$nn$softmax(tf$matmul(x, W) + b)

  y_ <- tf$placeholder(tf$float32, shape(NULL, 10L))
  cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(y), reduction_indices=1L))

  optimizer <- tf$train$GradientDescentOptimizer(0.5)
  train_step <- optimizer$minimize(cross_entropy)

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

  sess$run(accuracy, feed_dict=dict(x = mnist$test$images, y_ = mnist$test$labels))

  list(
    input = x,
    output = y
  )
}

test_that("export_savedmode() works with MNIST", {

  skip_if_no_tensorflow()

  sess <- tf$Session()
  tensors <- train_mnist(sess)
  temp_path <- tempfile()

  export_savedmodel(
    sess,
    temp_path,
    inputs = list(images = tensors$input),
    outputs = list(scores = tensors$output)
  )

  expect_true(file.exists(file.path(temp_path, "saved_model.pb")))
})
