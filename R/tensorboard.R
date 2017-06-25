#' TensorBoard Visualization Tool
#'
#' TensorBoard is a tool inspecting and understanding your TensorFlow runs and
#' graphs.
#'
#' @param log_dir Root directory for training logs.
#' @param action Specify whether to start or stop TensorBoard for the
#'   given `log_dir` (TensorBoard will be stopped automatically when
#'   the R session from which it is launched is terminated).
#' @param host Host for serving TensorBoard
#' @param port Port for serving TensorBoard. If "auto" is specified (the
#'   default) then an unused port will be chosen automatically.
#' @param launch_browser `TRUE` to open a web browser for TensorBoard
#'   after launching.
#' @param reload_interval How often the backend should load more data.
#' @param purge_orphaned_data Whether to purge data that may have been orphaned due to TensorBoard restarts.
#' Disabling purge_orphaned_data can be used to debug data disappearance.
#'
#' @return URL for browsing TensorBoard (invisibly).
#'
#' @details When TensorBoard is passed a logdir at startup, it recursively walks
#'   the directory tree rooted at logdir looking for subdirectories that contain
#'   tfevents data. Every time it encounters such a subdirectory, it loads it as
#'   a new run, and the frontend will organize the data accordingly.
#'
#'   The TensorBoard process will be automatically destroyed when the R
#'   session in which it is launched exits. You can pass `action = "stop"``
#'   to manually terminate a TensorBoard process for a given `log_dir`.
#'
#' @export
tensorboard <- function(log_dir = ".", action = c("start", "stop"),
                        host = "127.0.0.1", port = "auto",
                        launch_browser = interactive(),
                        reload_interval = 5,
                        purge_orphaned_data = TRUE
                        ) {

  # ensure that tensorflow initializes (so we get tensorboard on our path)
  ensure_loaded()

  # verify we can find tensorboard
  if (!nzchar(Sys.which("tensorboard")))
    stop("Unable to find tensorboard on PATH")

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
    }
    if (identical(port, "auto"))
      port <- tb$port
  }

  # exit if this was action = "stop"
  action <- match.arg(action)
  if (identical(action, "stop")) {
    cat("TensorBoard stopped.\n")
    return(invisible(NULL))
  }


  # for port = "auto", attempt to find a port up to 20 times
  if (identical(port, "auto")) {

    for (i in 1:20) {

      # determine the port (exclude those considered unsafe by Chrome)
      while(TRUE) {
        port <- 3000 + sample(5000, 1)
        if (!port %in% c(3659, 4045, 6000, 6665:6669))
          break
      }

      # attempt to launch
      p <- launch_tensorboard(log_dir, host, port, FALSE, reload_interval, purge_orphaned_data)
      if (p$is_alive())
        break
    }

  } else {
    p <- launch_tensorboard(log_dir, host, port, TRUE, reload_interval, purge_orphaned_data)
  }

  if (p$is_alive()) {

    # close connections
    close(p$get_output_connection())
    close(p$get_error_connection())

    # add to global list of tensorboards
    .globals$tensorboards[[log_dir]] <- list(process = p, port = port)

    # browse the url if requested
    url <- paste0("http://", host, ":", port)
    cat("Started TensorBoard at", url, "\n")
    if (launch_browser)
      utils::browseURL(url)

    # return the url invisibly
    invisible(url)

  } else {
    stop("Unable to launch tensorboard")
  }
}

launch_tensorboard <- function(log_dir, host, port, explicit_port, reload_interval, purge_orphaned_data) {

  # start the process
  p <- processx::process$new("tensorboard",
                             c("--logdir", log_dir,
                               "--host", host,
                               "--port", as.character(port),
                               "--reload_interval", as.integer(reload_interval),
                               "--purge_orphaned_data", purge_orphaned_data),
                             stdout = "|", stderr = "|")

  # poll for output until we've succesfully started up or the process dies
  started <- FALSE
  while(!started && p$is_alive()) {

    # poll for io
    res <- p$poll_io(100L)

    # see if we have stdout
    if (identical(res[["output"]], "ready")) {

      # capture output
      out <- p$read_output_lines()

      # check if it's a startup notification. if it is then set the started flag,
      # otherwise just forward the ouptut
      if (any(grepl("http://", out, fixed = TRUE)))
        started <- TRUE
      else
        write(out, stdout())
    }

    # see if we have stderr
    if (identical(res[["error"]], "ready")) {

      # capture error output
      err <- p$read_error_lines()

      # write it unless it's a port in use error when we are auto-binding
      if (explicit_port || !any(grepl(paste0("^.*", port, ".*already in use.*$"), err)))
        write(err, stderr())
    }
  }

  # return the process
  p
}


#' @rdname tensorboard
#' @export
unique_log_dir <- function(log_dir = "logs") {
  file.path(log_dir, strftime(Sys.time(), format = "%Y_%m_%d_%H_%M_%S"))
}

