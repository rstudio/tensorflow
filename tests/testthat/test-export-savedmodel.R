context("Save")
source("utils.R")

train_mnist_eager <- function() {

  mnist <- tf$keras$datasets$mnist$load_data()
  x_train <- mnist[[1]][[1]] / 255
  y_train <- mnist[[1]][[2]] %>% as.matrix()

  model <- tf$keras$Sequential(
    list(
      tf$keras$layers$Reshape(
        target_shape = c(28L, 28L, 1L),
        input_shape = c(28L, 28L)
      ),
      tf$keras$layers$Conv2D(8L, 5L, padding = "same", activation = tf$nn$relu),
      tf$keras$layers$MaxPooling2D(c(2, 2), c(2, 2), padding = "same"),
      tf$keras$layers$Conv2D(16L, 5L, padding = "same", activation = tf$nn$relu),
      tf$keras$layers$MaxPooling2D(c(2, 2), c(2, 2), padding = "same"),
      tf$keras$layers$Flatten(),
      tf$keras$layers$Dense(32L, activation = tf$nn$relu),
      tf$keras$layers$Dropout(0.4),
      tf$keras$layers$Dense(10L)
    )
  )

  # not needed if this is only entered for TF2 (vs. for eager execution in general)
  if (reticulate::py_has_attr(tf, "keras")) {
    optimizer <- tf$keras$optimizers$Adam()
  }
  else {
    optimizer <- tf$train$AdamOptimizer()
  }

  model$compile(
    optimizer = optimizer,
    loss = "sparse_categorical_crossentropy"
  )

  model$fit(x_train[1:10, , ], y_train[1:10, ], epochs = 1L)
  model

}

train_mnist_graph <- function(sess) {

  datasets <- tf$contrib$learn$datasets
  mnist <- datasets$mnist$read_data_sets("MNIST-data", one_hot = TRUE)

  x <- tf$placeholder(tf$float32, shape(NULL, 784L))

  W <- tf$Variable(tf$zeros(shape(784L, 10L)))
  b <- tf$Variable(tf$zeros(shape(10L)))

  y <- tf$nn$softmax(tf$matmul(x, W) + b)

  y_ <- tf$placeholder(tf$float32, shape(NULL, 10L))
  cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(y), reduction_indices = 1L))

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

  sess$run(accuracy, feed_dict = dict(x = mnist$test$images, y_ = mnist$test$labels))

  list(input = x, output = y)
}

test_that("export_savedmodel() works with MNIST", {
  skip_if_no_tensorflow()

  temp_path <- tempfile()

  if (tf$executing_eagerly()) {

    model <- train_mnist_eager()
    tf$saved_model$save(model, temp_path)

  } else {

    sess <- tf$Session()
    tensors <- train_mnist_graph(sess)

    export_savedmodel(
      sess,
      temp_path,
      inputs = list(images = tensors$input),
      outputs = list(scores = tensors$output)
    )
  }

  expect_true(file.exists(file.path(temp_path, "saved_model.pb")))

})
