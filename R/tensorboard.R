#' TensorBoard Visualization Tool
#'
#' TensorBoard is a tool inspecting and understanding your TensorFlow runs and
#' graphs.
#'
#' @param log_dir Directories to scan for training logs. If this is a named
#'   character vector then the specified names will be used as aliases within
#'   TensorBoard. The default is `NULL`, which will result in the active
#'   [run_dir()] (if available) and otherwise will use the current working
#'   directory.
#' @param action Specify whether to start or stop TensorBoard for the given
#'   `log_dir` (TensorBoard will be stopped automatically when the R session
#'   from which it is launched is terminated).
#' @param host Host for serving TensorBoard
#' @param port Port for serving TensorBoard. If "auto" is specified (the
#'   default) then an unused port will be chosen automatically.
#' @param launch_browser `TRUE` to open a web browser for TensorBoard after
#'   launching.
#' @param reload_interval How often the backend should load more data.
#' @param purge_orphaned_data Whether to purge data that may have been orphaned
#'   due to TensorBoard restarts. Disabling purge_orphaned_data can be used to
#'   debug data disappearance.
#'
#' @return URL for browsing TensorBoard (invisibly).
#'
#' @details When TensorBoard is passed a logdir at startup, it recursively walks
#'   the directory tree rooted at logdir looking for subdirectories that contain
#'   tfevents data. Every time it encounters such a subdirectory, it loads it as
#'   a new run, and the frontend will organize the data accordingly.
#'
#'   The TensorBoard process will be automatically destroyed when the R session
#'   in which it is launched exits. You can pass `action = "stop"` to manually
#'   terminate TensorBoard.
#'
#' @export
tensorboard <- function(log_dir = NULL, action = c("start", "stop"),
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

  # determine the default log_dir if necessary
  if (is.null(log_dir)) {
    if (!is.null(run_dir()))
      log_dir <- run_dir()
    else
      log_dir <- "."
  }

  # create log_dir(s) if necessary
  log_dir <- as.character(lapply(log_dir, function(dir) {
    if (!utils::file_test("-d", dir))
      dir.create(dir, recursive = TRUE)
    dir
  }))

  # if we already have a tensorboard for this session then kill it and re-use it's port
  if (!is.null(.globals$tensorboard)) {
    p <- .globals$tensorboard$process
    if (p$is_alive()) {
      p$kill()
      p$wait(1000L)
    }
    if (identical(port, "auto"))
      port <- .globals$tensorboard$port
    .globals$tensorboard <- NULL
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

    # save as global tensorboard
    .globals$tensorboard <- list(process = p, port = port)

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

  # check for names and provide defaults if length > 1
  names <- names(log_dir)
  if (is.null(names) && (length(log_dir) > 1))
    names <- basename(log_dir)

  # concatenate names if we have them
  if (!is.null(names))
    log_dir <- paste0(names, ":", log_dir)

  # build log_dir
  log_dir <- paste(log_dir, collapse = ",")

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











