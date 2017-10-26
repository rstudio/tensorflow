#' Serve a TensorFlow Model
#'
#' Serve a TensorFlow Model into a local REST/JSON API.
#'
#' @param model_dir The path to the exported model, as a string.
#' @param host Address to use to serve model, as a string.
#' @param port Port to use to serve model, as numeric.
#' @param daemonized Makes 'httpuv' server daemonized so R interactive sessions
#'   are not blocked to handle requests. To terminate a daemonized server, call
#'   'httpuv::stopDaemonizedServer()' with the handle returned from this call.
#'
#' @importFrom httpuv runServer
#' @export
serve_savedmodel <- function(
  model_dir,
  host = "127.0.0.1",
  port = 8089,
  daemonized = FALSE
  ) {
  httpuv_start <- if (daemonized) httpuv::startDaemonizedServer else httpuv::runServer
  serve_run(model_dir, host, port, httpuv_start, !daemonized && interactive())
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

serve_static_file_response <- function(package, file_path, remove = NULL) {
  file_path <- system.file(file_path, package = package)
  file_contents <- if (file.exists(file_path)) readBin(file_path, "raw", n = file.info(file_path)$size) else NULL

  if (!is.null(remove)) {
    contents <- rawToChar(file_contents)
    contents <- sub(remove, "", contents)
    file_contents <- charToRaw(enc2utf8(contents))
  }

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
          swagger_from_signature_def(signature_def)
        ))
      )
    },
    "^/$" = function(req, sess, signature_def) {
      serve_static_file_response(
        "swagger",
        "dist/index.html",
        "http://petstore\\.swagger\\.io/v2"
      )
    },
    "^/[^/]*$" = function(req, sess, signature_def) {
      serve_static_file_response("swagger", file.path("dist", req$PATH_INFO))
    },
    "^/api/[^/]*/predict" = function(req, sess, signature_def) {
      signature_names <- signature_def$keys()
      signature_name <- strsplit(req$PATH_INFO, "/")[[1]][[3]]

      if (!signature_name %in% signature_names) {
        stop("Signature '", signature_name, "' not available in model signatures.")
      }

      json_raw <- req$rook.input$read()
      json_req <- jsonlite::fromJSON(
        rawToChar(json_raw),
        simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE
      )

      signature_obj <- signature_def$get(signature_name)

      tensor_input_names <- signature_obj$inputs$keys()
      if (length(tensor_input_names) == 0) {
        stop("Signature '", signature_name, "' contains no inputs.")
      }

      tensor_output_names <- signature_obj$outputs$keys()

      fetches_list <- lapply(seq_along(tensor_output_names), function(fetch_idx) {
        sess$graph$get_tensor_by_name(
          signature_obj$outputs$get(tensor_output_names[[fetch_idx]])$name
        )
      })

      input_instances <- json_req$instances

      feed_dict <- list()
      if (is.list(input_instances)) {
        lapply(tensor_input_names, function(tensor_input_name) {
          placeholder_name <- signature_obj$inputs$get(tensor_input_name)$name

          if (length(tensor_input_names) == 1) {
            input_instance <- input_instances[[1]]
          }
          else if (!tensor_input_name %in% names(input_instances)) {
            stop("Input '", tensor_input_name, "' found in model but not in API request.")
          } else {
            input_instance <- input_instances[[tensor_input_name]]
          }

          if (is.list(input_instance) && "b64" %in% names(input_instance)) {
            feed_dict[[placeholder_name]] <<- tf$decode_base64(instance$b64)
          }
          else if (length(tensor_input_names) == 1 && length(names(input_instance)) == 0) {
            feed_dict[[placeholder_name]] <<- input_instance
          }
          else if (!tensor_input_name %in% names(input_instance)) {
            stop("Input named '", tensor_input_name, "' not defined in all input instances.")
          }
          else {
            feed_dict[[placeholder_name]] <<- lapply(input_instance[[tensor_input_name]], function(e) {
              if (is.list(e) && "b64" %in% names(e))
                tf$decode_base64(instance$b64)
              else
                e
            })
          }
        })
      }
      else {
        feed_dict[[signature_obj$inputs$get(tensor_input_names[[1]])$name]] <- input_instances
      }

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
      stop("Invalid path.")
    }
  )
}

serve_run <- function(model_dir, host, port, start, browse) {
  sess <- tf$Session()
  on.exit(sess$close(), add = TRUE)

  signature_def <- serve_load_model(sess, model_dir)

  if (browse) utils::browseURL(paste0("http://", host, ":", port))

  handlers <- serve_handlers(host, port)

  start(host, port, list(
    onHeaders = function(req) {
      NULL
    },
    call = function(req) {
      tryCatch({
        matches <- sapply(names(handlers), function(e) grepl(e, req$PATH_INFO))
        handlers[matches][[1]](req, sess, signature_def)
      }, error = function(e) {
        serve_invalid_request(e$message)
      })
    },
    onWSOpen = function(ws) {
      NULL
    }
  ))
}
