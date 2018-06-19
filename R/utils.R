"%||%" <- function(x, y) if (is.null(x)) y else x

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_unix <- function() {
  identical(.Platform$OS.type, "unix")
}

is_osx <- function() {
  Sys.info()["sysname"] == "Darwin"
}

is_linux <- function() {
  identical(tolower(Sys.info()[["sysname"]]), "linux")
}

is_ubuntu <- function() {
  # check /etc/lsb-release
  if (is_unix() && file.exists("/etc/lsb-release")) {
    lsbRelease <- readLines("/etc/lsb-release")
    any(grepl("Ubuntu", lsbRelease))
  } else {
    FALSE
  }
}

dir_exists <- function(x) {
  utils::file_test('-d', x)
}

ensure_loaded <- function() {
  invisible(tf$`__version__`)
}

aliased <- function(path) {
  sub(Sys.getenv("HOME"), "~", path)
}


call_hook <- function(name, ...) {
  hooks <- getHook(name)
  if (!is.list(hooks))
    hooks <- list(hooks)
  response <- FALSE
  lapply(hooks, function(hook) {
    if (isTRUE(hook(...)))
      response <<- TRUE
  })
  response
}

