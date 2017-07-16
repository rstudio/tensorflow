


#' Run Directory
#'
#' Timestamped directory for storing training/logging data in a separate
#' location for each training run. Note that if the `RUNDIR` environment
#' variable is availale it will be returned as the run directory.
#'
#' @param runs_dir Parent directory for runs
#' @param latest_n List only the latest n runs
#'
#' @return `run_dir()` retruns a timestamped run directory. The
#'  `list_runs()` function returns a character vector with the
#'  paths of existing run directories (`latest_run()` is a convenience
#'  function for getting the path to the most recent run directory).
#'
#' @export
run_dir <- function(runs_dir = "runs") {
  dir <- Sys.getenv("RUNDIR", unset = NA)
  if (is.na(dir))
    dir <- unique_dir(runs_dir, format = "%Y-%m-%dT%H-%M-%SZ")
  if (!utils::file_test("-d", dir))
    dir.create(dir, recursive = TRUE)
  dir
}

#' @rdname run_dir
#' @export
latest_run <- function(runs_dir = "runs") {
  list_runs(runs_dir, latest_n = 1)
}


#' @rdname run_dir
#' @export
list_runs <- function(runs_dir = "runs", latest_n = NULL) {
  if (file.exists(runs_dir)) {
    runs <- list.files(runs_dir,
                       pattern = "\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}Z",
                       full.names = FALSE)
    if (length(runs) > 0) {
      runs <- runs[order(runs, decreasing = TRUE)]
      if (!is.null(latest_n))
        runs <- runs[1:min(length(runs),latest_n)]
      runs <- data.frame(created = as.POSIXct(runs, format = "%Y-%m-%dT%H-%M-%SZ"),
                         run_dir = file.path(runs_dir, runs),
                         stringsAsFactors = FALSE)
      runs <- runs[order(runs$created , decreasing = TRUE ),]
      runs$run_dir
    } else {
      character()
    }
  }
  else
    character()
}


unique_dir <- function(parent_dir, prefix = NULL, format = "%Y-%m-%dT%H-%M-%SZ") {
  while(TRUE) {
    dir <- file.path(parent_dir,
                     paste0(prefix, strftime(Sys.time(), format = format, tz = "GMT")))
    if (!file.exists(dir))
      return(dir)
    else
      Sys.sleep(0.1)
  }
}





