#' View a Saved Model
#'
#' View a serialized model from disk.
#'
#' @param export_dir The path to the exported directory, as a string.
#' @param log_dir Directories to scan for training logs. If this is a named
#'   character vector then the specified names will be used as aliases within
#'   TensorBoard.
#'
#' @return The path to the exported directory, as a string.
#'
#' @export
view_savedmodel <- function(
  export_dir,
  log_dir = tempdir()
) {
  export_files <- dir(export_dir, full.names = TRUE, recursive = TRUE)
  export_pb <- export_files[grepl("\\.pb$", export_files)]
  if (length(export_pb) != 1) stop("Failed to find 'pb' file under ", export_dir)

  gfile <- tf$python$platform$gfile
  compat <- tf$python$util$compat
  saved_model_pb2 <- tf$core$protobuf$saved_model_pb2

  with(tf$Session() %as% sess, {
    with(gfile$FastGFile(export_pb, "rb") %as% f, {
      graph_def <- tf$GraphDef()

      data <- compat$as_bytes(f$read())
      sm <- saved_model_pb2$SavedModel()
      sm$ParseFromString(data)

      if (sm$meta_graphs$`__len__`() > 1) stop("Saved model contains more than one graph")

      g_in <- tf$import_graph_def(sm$meta_graphs[[0]]$graph_def)
    })

    train_writer <- tf$summary$FileWriter(log_dir)
    train_writer$add_graph(sess$graph)
    train_writer$close()
  })

  tensorboard(log_dir = log_dir)
}
