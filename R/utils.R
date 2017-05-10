

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

is_unix <- function() {
  identical(.Platform$OS.type, "unix")
}

is_osx <- function() {
  Sys.info()["sysname"] == "Darwin"
}

is_ubuntu <- function() {
  uname <- suppressWarnings(system2("uname", "--all", stdout = TRUE, stderr = TRUE))
  grepl("ubuntu", uname[[1]], ignore.case = TRUE)
}


