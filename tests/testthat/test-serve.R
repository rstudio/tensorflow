context("Serve")

source("utils.R")

test_mnist_train <- function(sess) {
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

test_mnist_save <- function(sess, model_dir, x, y) {
  builder <- tf$saved_model$builder$SavedModelBuilder(model_dir)

  builder$add_meta_graph_and_variables(
    sess,
    list(
      tf$python$saved_model$tag_constants$SERVING
    ),
    signature_def_map = list(
      serving_default = tf$saved_model$signature_def_utils$build_signature_def(
        inputs = list(images = tf$saved_model$utils$build_tensor_info(x)),
        outputs = list(scores = tf$saved_model$utils$build_tensor_info(y))
      )
    )
  )

  builder$save()
}

test_that("can serve mnist model", {
  sess <- tf$Session()
  model_dir <- tempfile()

  trained <- test_mnist_train(sess)
  test_mnist_save(sess, model_dir, trained$input, trained$output)

  handle <- serve_savedmodel(model_dir, daemonized = TRUE)

  expect_true(!is.null(handle))

  swagger_file <- tempfile(fileext = ".json")
  download.file("http://127.0.0.1:8089/swagger.json", swagger_file)
  swagger_contents <- readChar(swagger_file, file.info(swagger_file)$size)

  expect_true(grepl("serving_default", swagger_contents))

  httpuv::stopDaemonizedServer(handle)
})
