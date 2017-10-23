#' Serve a TensorFlow Model
#'
#' Serve a TensorFlow Model into a local REST/JSON API.
#'
#' @importFrom httpuv runServer
#' @export
serve <- function(
  model_dir,
  host = "127.0.0.1",
  port = 8089,
  browse = interactive(),
  daemonized = FALSE
  ) {
  httpuv_start <- if (daemonized) httpuv::startDaemonizedServer else httpuv::runServer
  serve_run(model_dir, host, port, httpuv_start, browse)
}

serve_load_model <- function(sess, model_dir) {
  tf$reset_default_graph()

  graph <- tf$saved_model$loader$load(
    sess,
    list(tf$python$saved_model$tag_constants$SERVING),
    model_dir)

  graph$signature_def
}

serve_content_type <- function(file_path) {
  file_split <- strsplit(file_path, split = "\\.")[[1]]
  switch(file_split[[length(file_split)]],
         "css" = "text/css",
         "html" = "text/html",
         "js" = "application/javascript",
         "json" = "application/json",
         "map" = "text/plain",
         "png" = "image/png"
  )
}

serve_static_file_response <- function(file_path) {
  file_path <- system.file(file_path, package = "tfserve")
  file_contents <- if (file.exists(file_path)) readBin(file_path, "raw", n = file.info(file_path)$size) else NULL

  list(
    status = 200L,
    headers = list(
      "Content-Type" = paste0(serve_content_type(file_path))
    ),
    body = file_contents
  )
}

serve_invalid_request <- function(message = NULL) {
  list(
    status = 404L,
    headers = list(
      "Content-Type" = "text/plain; charset=UTF-8"
    ),
    body = charToRaw(enc2utf8(
      paste(
        "Invalid Request. ",
        message
      )
    ))
  )
}

serve_handlers <- function(host, port) {
  list(
    "^/swagger.json" = function(req, sess, signature_def) {
      list(
        status = 200L,
        headers = list(
          "Content-Type" = paste0(serve_content_type("json"), "; charset=UTF-8")
        ),
        body = charToRaw(enc2utf8(
          swagger_from_signature_def(signature_def, host, port)
        ))
      )
    },
    "^/$" = function(req, sess, signature_def) {
      serve_static_file_response("swagger-ui/index.html")
    },
    "^/[^/]*$" = function(req, sess, signature_def) {
      serve_static_file_response(file.path("swagger-ui", req$PATH_INFO))
    },
    "^/api/[^/]*/predict" = function(req, sess, signature_def) {
      signature_names <- signature_def$keys()
      signature_name <- strsplit(req$PATH_INFO, "/")[[1]][[3]]

      if (!signature_name %in% signature_names) {
        serve_invalid_request()
        return()
      }

      json_raw <- req$rook.input$read()
      json_req <- jsonlite::fromJSON(rawToChar(json_raw))

      tensor_input_names <- signature_def$get(signature_name)$inputs$keys()
      if (length(tensor_input_names) != 1) {
        serve_invalid_request("Currently, only single-tensor inputs are supported but found ", length(tensor_input_names))
        return()
      }

      tensor_output_names <- signature_def$get(signature_name)$outputs$keys()

      fetches_list <- lapply(seq_along(tensor_output_names), function(fetch_idx) {
        sess$graph$get_tensor_by_name(
          signature_def$get(signature_name)$outputs$get(tensor_output_names[[fetch_idx]])$name
        )
      })

      feed_dict <- list()
      feed_dict[[signature_def$get(signature_name)$inputs$get(tensor_input_names[[1]])$name]] <- json_req$instances
      result <- sess$run(
        fetches = fetches_list,
        feed_dict = feed_dict
      )

      list(
        status = 200L,
        headers = list(
          "Content-Type" = paste0(serve_content_type("json"), "; charset=UTF-8")
        ),
        body = charToRaw(enc2utf8(
          jsonlite::toJSON(result)
        ))
      )
    },
    ".*" = function(req, signature_def) {
      serve_invalid_request()
    }
  )
}

serve_run <- function(model_dir, host, port, start, browse) {
  sess <- tf$Session()
  signature_def <- serve_load_model(sess, model_dir)

  if (browse) utils::browseURL(paste0("http://", host, ":", port))

  handlers <- serve_handlers(host, port)

  start(host, port, list(
    onHeaders = function(req) {
      NULL
    },
    call = function(req){
      matches <- sapply(names(handlers), function(e) grepl(e, req$PATH_INFO))
      handlers[matches][[1]](req, sess, signature_def)
    },
    onWSOpen = function(ws) {
      NULL
    }
  ))
}
