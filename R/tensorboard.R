


#' Visualize Training with TensorBoard
#'
#'
#' @param logdir Directory where training events are logged.
#' @param host What host to listen to
#'
#'
#' @export
tensorboard <- function(logdir, host = "127.0.0.1", port = "auto") {

  # deterimine port
  if (identical(port, "auto"))
    port <- free_port(host)
  else if (!is.numeric(port))
    stop("port must be an integer")

  # run tensorboard
  system2("tensorboard", args = shQuote(c("--logdir", logdir,
                                        "--host", host,
                                        "--port", port)))
}



# get a free tcp/ip port
free_port <- function(host = "127.0.0.1") {
  socket <- import("socket")
  s <- socket$socket(socket$AF_INET, socket$SOCK_STREAM)
  on.exit(s$close(), add = TRUE)
  s$bind(tuple(host, 0L))
  addr = s$getsockname()
  addr[[2]]
}

