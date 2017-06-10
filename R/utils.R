

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

ensure_loaded <- function() {
  invisible(tf$`__version__`)
}


