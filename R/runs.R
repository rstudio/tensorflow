
#' Create a timestamp based unique directory name
#'
#' Directory name is a timestamp compatible with filesystem naming restrictions
#' (e.g. "2017-07-13T16-18-51.569Z").
#'
#' @param parent_dir Parent directory
#' @param prefix Character value to prepend to directory name
#'
#' @return Unique directory name
#'
#' @note The name of the directory is based on the current time (in milliseconds
#'   resolution). If the generated directory name already exists then the
#'   function will wait long enough to return a directory with a distinct
#'   timestamp.
#'
#' @seealso [run_dir()]
#'
#' @export
unique_dir <- function(parent_dir, prefix = NULL, format = "%Y-%m-%dT%H-%M-%OS3Z") {
  while(TRUE) {
    dir <- file.path(parent_dir,
                     paste0(prefix, strftime(Sys.time(), format = format, tz = "gmt")))
    if (!file.exists(dir))
      return(dir)
    else
      Sys.sleep(0.1)
  }
}

#' Run Directory
#'
#' Timestamped directory for storing training/logging data in a separate
#' location for each training run. Note that if the `RUNDIR` environment
#' variable is availale it will be returned as the run directory.
#'
#' @param parent_dir Parent to create run directory within.
#'
#' @return Timestamped run directory.
#'
#' @export
run_dir <- function(parent_dir = "runs") {
  dir <- Sys.getenv("RUNDIR", unset = NA)
  if (is.na(dir))
    dir <- unique_dir(parent_dir)
  if (!utils::file_test("-d", dir))
    dir.create(dir, recursive = TRUE)
  dir
}

