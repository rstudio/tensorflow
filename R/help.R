

register_tf_help_handler <- function() {
  reticulate::register_module_help_handler("tensorflow", function(name, subtopic = NULL) {

    # # Version specific URLs are disabled because
    # # upstream TF is missing public docs for later version
    # # https://github.com/tensorflow/tensorflow/issues/89084
    # # get the base tensorflow help url
    # version <- tf$`__version__`
    # version <- strsplit(version, ".", fixed = TRUE)[[1]]
    # help_url <- paste0("https://www.tensorflow.org/versions/r",
    #                    version[1], ".", version[2], "/api_docs/python/")

    help_url <- "https://www.tensorflow.org/api_docs/python/"

    # some adjustments
    name <- sub("^tensorflow\\._api\\.v2\\.", "tensorflow.", name)
    name <- sub("^tensorflow", "tf", name)
    name <- sub("python.client.session.", "", name, fixed = TRUE)
    name <- sub("python.ops.", "", name, fixed = TRUE)
    if (grepl("tf.contrib.opt", name)) {
      components <- strsplit(name, ".", fixed = TRUE)[[1]]
      class_name <- components[[length(components)]]
      name <- paste0("tf.contrib.opt", ".", class_name)
    }

    # form topic url
    topic_url <- gsub(".", "/", name, fixed = TRUE)
    if (!is.null(subtopic))
      topic_url <- paste0(topic_url, "#", subtopic)

    # return the full url
    paste0(help_url, topic_url)
  })
}

