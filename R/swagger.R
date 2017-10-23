#' Create Swagger Definition
#'
#' Creates a Swagger definition from a TensorFlow signature.
#'
#' @param signature_def A signature definition exported indirectly through
#'   'export_savedmodel()' or explicitly using 'build_signature_def()'.
#'
#' @import jsonlite
#' @export
swagger_from_signature_def <- function(
  signature_def,
  host = "127.0.0.1",
  port = 8089,
  model_dir = NULL) {
  def <- c(
    swagger_header(host, port),
    swagger_paths(signature_def),
    swagger_defs(signature_def)
  )

  json <- jsonlite::toJSON(def)

  if (is.character(model_dir)) {
    writeLines(as.character(json), file.path(model_dir, "swagger.json"))
  }

  json
}

swagger_header <- function(host, port) {
  list(
    swagger = unbox("2.0"),
    info = list(
      description = unbox("API to TensorFlow Model."),
      version = unbox("1.0.0"),
      title = unbox("TensorFlow Model")
    ),
    host = unbox(paste(host, port, sep = ":")),
    basePath = unbox("/api"),
    schemes = list(
      unbox("http")
    )
  )
}

swagger_path <- function(siagnature_name, signature_id) {
  list(
    post = list(
      summary = unbox(paste0("Perform prediction over '", siagnature_name, "'")),
      description = unbox(""),
      consumes = list(
        unbox("application/json")
      ),
      produces = list(
        unbox("application/json")
      ),
      parameters = list(
        list(
          "in" = unbox("body"),
          name = unbox("body"),
          description = unbox(paste0("Prediction instances for '", siagnature_name, "'")),
          required = unbox(TRUE),
          schema = list(
            "$ref" = unbox(paste0("#/definitions/Instances", signature_id))
          )
        )
      ),
      responses = list(
        "200" = list(
          description = unbox("Success")
        )
      )
    )
  )
}

swagger_paths <- function(signature_def) {
  path_names <- signature_def$keys()
  path_values <- lapply(seq_along(path_names), function(path_index) {
    swagger_path(path_names[[path_index]], path_index)
  })
  names(path_values) <- path_names

  serving_default <- tf$saved_model$signature_constants$DEFAULT_SERVING_SIGNATURE_DEF_KEY
  if (!serving_default %in% path_names) {
    warning(
      "Signature '",
      serving_default,
      "' is missing but is required for some services like CloudML."
    )
  }
  else {
    # make serving default first entry in swagger-ui
    path_names <- path_names[path_names != serving_default]
    serving_default_value <- path_values[[serving_default]]
    path_values[[serving_default]] <- NULL

    path_names <- c(serving_default, path_names)
    path_values <- c(list(serving_default_value), path_values)
  }

  full_urls <- paste0("/", path_names, "/predict/")

  names(path_values) <- full_urls

  path_values[order(unlist(path_values), decreasing=TRUE)]

  list(
    paths = path_values
  )
}

swagger_dtype_to_swagger <- function(dtype) {
  # DTypes: https://github.com/tensorflow/tensorflow/blob/master/tensorflow/python/framework/dtypes.py
  # Swagger: https://swagger.io/docs/specification/data-models/data-types/

  regex_mapping <- list(
    "int32"     = list(type = "integer", format = "int32"),
    "int64"     = list(type = "integer", format = "int64"),
    "int"       = list(type = "integer", format = ""),
    "float"     = list(type = "number",  format = "float"),
    "complex"   = list(type = "number",  format = ""),
    "string"    = list(type = "string",  format = ""),
    "bool"      = list(type = "boolean", format = "")
  )

  regex_name <- Filter(function(r) grepl(r, dtype$name), names(regex_mapping))
  if (length(regex_name) == 0) {
    stop("Failed to map dtype ", dtype$name, " to swagger type.")
  }

  result <- regex_mapping[[regex_name[[1]]]]

  lapply(result, jsonlite::unbox)
}

swagger_def <- function(signature_def, signature_key, signature_id) {
  tensor_input_names <- signature_def$get(signature_key)$inputs$keys()
  if (length(tensor_input_names) != 1) {
    stop("Currently, only single-tensor inputs are supported but found ", length(tensor_input_names))
  }

  tensor_input <- signature_def$get(signature_key)$inputs$get(tensor_input_names[[1]])

  tensor_input_dim <- tensor_input$tensor_shape$dim
  tensor_input_dim_len <- tensor_input_dim$`__len__`()

  swagger_type_def <- NULL
  if (tensor_input_dim_len == 0) {
    swagger_type_def <- swagger_dtype_to_swagger(tf$DType(tensor_input$dtype))
  }
  else {
    swagger_type_def <- list(
      type = unbox("array"),
      items = swagger_dtype_to_swagger(tf$DType(tensor_input$dtype)),
      example = rep(1.0, tensor_input$tensor_shape$dim[[tensor_input_dim_len - 1]]$size)
    )

    for (idx in seq_len(tensor_input_dim_len - 1)) {
      swagger_type_def <- list(
        type = unbox("array"),
        items = swagger_type_def
      )
    }
  }

  list(
    type = unbox("object"),
    properties = list(
      instances = swagger_type_def
    )
  )
}

swagger_defs <- function(signature_def) {
  defs_names <- signature_def$keys()
  defs_values <- lapply(seq_along(defs_names), function(defs_index) {
    swagger_def(signature_def, defs_names[[defs_index]], defs_index)
  })
  names(defs_values) <- lapply(seq_along(defs_names), function(def_idx) {
    paste0("Instances", def_idx)
  })

  list(
    definitions = defs_values
  )
}
