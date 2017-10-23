#' Serve a TensorFlow Model
#'
#' Serve a TensorFlow Model into a local REST/JSON API.
#'
#' @importFrom httpuv runServer
#' @export
serve <- function(
  model_path,
  host = "127.0.0.1",
  port = 8089,
  browse = interactive()
  ) {

  run_server(host, port, sess, graph, httpuv::runServer, model_path, browse)

}

load_model <- function(sess, model_path) {
  tf$reset_default_graph()

  graph <- tf$saved_model$loader$load(
    sess,
    list(tf$python$saved_model$tag_constants$SERVING),
    model_path)

  graph$signature_def
}

server_content_type <- function(file_path) {
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

server_static_file_response <- function(file_path) {
  file_path <- system.file(file_path, package = "tfserve")
  file_contents <- if (file.exists(file_path)) readBin(file_path, "raw", n = file.info(file_path)$size) else NULL

  list(
    status = 200L,
    headers = list(
      "Content-Type" = paste0(server_content_type(file_path))
    ),
    body = file_contents
  )
}

server_invalid_request <- function(message = NULL) {
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

server_handlers <- function(host, port) {
  list(
    "^/swagger.json" = function(req, sess, graph) {
      list(
        status = 200L,
        headers = list(
          "Content-Type" = paste0(server_content_type("json"), "; charset=UTF-8")
        ),
        body = charToRaw(enc2utf8(
          swagger_from_graph(graph, host, port)
        ))
      )
    },
    "^/$" = function(req, sess, graph) {
      server_static_file_response("swagger-ui/index.html")
    },
    "^/[^/]*$" = function(req, sess, graph) {
      server_static_file_response(file.path("swagger-ui", req$PATH_INFO))
    },
    "^/api/[^/]*/predict" = function(req, sess, graph) {
      signature_names <- graph$keys()
      signature_name <- strsplit(req$PATH_INFO, "/")[[1]][[3]]

      if (!signature_name %in% signature_names) {
        server_invalid_request()
        return()
      }

      json_raw <- req$rook.input$read()
      json_req <- jsonlite::fromJSON(rawToChar(json_raw))

      tensor_input_names <- graph$get(signature_name)$inputs$keys()
      if (length(tensor_input_names) != 1) {
        server_invalid_request("Currently, only single-tensor inputs are supported but found ", length(tensor_input_names))
        return()
      }

      tensor_output_names <- graph$get(signature_name)$outputs$keys()

      fetches_list <- lapply(seq_along(tensor_output_names), function(fetch_idx) {
        sess$graph$get_tensor_by_name(
          graph$get(signature_name)$outputs$get(tensor_output_names[[fetch_idx]])$name
        )
      })

      feed_dict <- list()
      feed_dict[[graph$get(signature_name)$inputs$get(tensor_input_names[[1]])$name]] <- json_req$instances
      result <- sess$run(
        fetches = fetches_list,
        feed_dict = feed_dict
      )

      list(
        status = 200L,
        headers = list(
          "Content-Type" = paste0(server_content_type("json"), "; charset=UTF-8")
        ),
        body = charToRaw(enc2utf8(
          jsonlite::toJSON(result)
        ))
      )
    },
    ".*" = function(req, graph) {
      server_invalid_request()
    }
  )
}

run_server <- function(host, port, sess, graph, start, model_path, browse) {
  sess <- tf$Session()
  graph <- load_model(sess, model_path)
  # sess$run(tf$global_variables_initializer())

  if (browse) utils::browseURL(paste0("http://", host, ":", port))

  handlers <- server_handlers(host, port)

  start(host, port, list(
    onHeaders = function(req) {
      NULL
    },
    call = function(req){
      matches <- sapply(names(handlers), function(e) grepl(e, req$PATH_INFO))
      handlers[matches][[1]](req, sess, graph)
    },
    onWSOpen = function(ws) {
      NULL
    }
  ))
}
