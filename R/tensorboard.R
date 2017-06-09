




#' TensorBoard Visualization Tool
#'
#' TensorBoard is a tool inspecting and understanding your TensorFlow runs and
#' graphs.
#'
#' @param log_dir Root directory for training logs.
#' @param host Host for serving TensorBoard
#' @param port Port for serving TensorBoard. If "auto" is specified (the
#'   default) then an unused port will be chosen automatically.
#'
#' @details When TensorBoard is passed a logdir at startup, it recursively walks
#'   the directory tree rooted at logdir looking for subdirectories that contain
#'   tfevents data. Every time it encounters such a subdirectory, it loads it as
#'   a new run, and the frontend will organize the data accordingly.
#'
#' @seealso
#' <https://github.com/tensorflow/tensorflow/blob/master/tensorflow/tensorboard/>
#'
#' @export
tensorboard <- function(log_dir = ".", host = "127.0.0.1", port = "auto") {

  # ensure that tensorflow initializes (so we get tensorboard on our path)
  ensure_loaded()

  # expand log dir
  log_dir <- normalizePath(log_dir, mustWork = FALSE)
  if (!utils::file_test("-d", log_dir))
    stop("Specified log_dir not found at ", log_dir)

  # if we already have a tensorboard for this directory then kill it
  # and re-use it's port
  if (exists(log_dir, envir = .globals$tensorboards)) {
    tb <- get(log_dir, envir = .globals$tensorboards)
    remove(list = log_dir, envir = .globals$tensorboards)
    p <- tb$process
    if (p$is_alive()) {
      p$kill()
      p$wait(1000L)
      if (is.null(port))
        port <- tb$port
    }
  }

  # determine port
  explicit_port <- !identical(port, "auto")
  if (!explicit_port) {
    # generate a port (but don't use ports that Chrome considers unsafe)
    while(TRUE) {
      port <- 3000 + sample(5000, 1)
      if (!port %in% c(3659, 4045, 6000, 6665:6669))
        break
    }
  }

  # start the process
  p <- processx::process$new("tensorboard",
                             c("--logdir", log_dir,
                               "--host", host,
                               "--port", as.character(port)),
                             stdout = "|", stderr = "|")

  # poll for output until we get the port
  started <- FALSE
  while(!started) {

    # poll for io
    res <- p$poll_io(100L)

    # see if we have stdout
    if (identical(res[["output"]], "ready")) {

      # forward output
      out <- p$read_output_lines()
      write(out, stdout())

      # see if we have started
      if (any(grepl("http://", out, fixed = TRUE))) {
        started <- TRUE
      }
    }

    # see if we have stderr
    if (identical(res[["error"]], "ready")) {

      # capture error output
      err <- p$read_error_lines()

      # if it's 'already in use' and there is no explicit port then call the function back
      # to try another random port
      if (!explicit_port && any(grepl(paste0("^.*", port, ".*already in use.*$"), err))) {
        tensorboard(log_dir = log_dir, host = host, port = "auto")
      } else {
        write(err, stderr())
      }
    }

    # abort if the process is dead
    if (!p$is_alive())
      break
  }

  # success!
  if (p$is_alive()) {

    # close connections
    close(p$get_output_connection())
    close(p$get_error_connection())

    # add to global list of tensorboards
    .globals$tensorboards[[log_dir]] <- list(process = p, port = port)

    # browse the url
    utils::browseURL(paste0("http://", host,":", port))
  }
}
