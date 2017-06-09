





#' @export
tensorboard <- function(log_dir = "logs", port = NULL) {

  # expand log dir
  log_dir <- normalizePath(log_dir, mustWork = FALSE)
  if (utils::file_test("-d", log_dir))
    stop("Specified log_dir not found at ", log_dir)

  # TODO: lookup by log_dir
  # kill and reload if we have an existing tensorboard instance

  # ensure that tensorflow initializes (so we get tensorboard on our path)
  tf$`__version__`

  # TODO: port generation
  # port (we'll get this from tensorboard after it starts)
  if (is.null(port))
    port = "7777"

  # start the process
  p <- processx::process$new("tensorboard",
                             c("--logdir", log_dir,
                               "--host", "127.0.0.1",
                               "--port", as.character(port)),
                             stdout = "|", stderr = "|",
                             cleanup = TRUE)

  # poll for output until we get the port
  while(TRUE) {

    # poll for io
    res <- p$poll_io(100L)

    # see if we have stdout
    if (identical(res[["output"]], "ready")) {

      # forward output
      out <- p$read_output_lines()
      write(out, stdout())

      # see if this includes the port
      matches <- regexec("^.*http://127.0.0.1:(\\d+)$", out)
      matches <- regmatches(out, matches)
      for (match in matches) {
        if (length(match) == 2)
          port <- match[[2]]
      }

      # if we got the port then break
      if (!is.null(port))
        break
    }

    # see if we have stderr
    if (identical(res[["error"]], "ready")) {

      # forward output
      err <- p$read_error_lines()
      write(err, stderr())

    }

    # abort if the process is dead
    if (!p$is_alive())
      break
  }

  # add to global list of tensorboards
  if (p$is_alive()) {
    close(p$get_output_connection())
    close(p$get_error_connection())

    # TODO: save by log_dir

    .globals$tensorboards[[length(.globals$tensorboards) + 1]] <- p
  }

  # launch browser if we have a port
  if (!is.null(port))
    browseURL(paste0("http://127.0.0.1:", port))
}
