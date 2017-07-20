

#' Run Directory
#'
#' Timestamped directory for storing training/logging data in a separate
#' location for each training run.
#'
#' The `use_run_dir()` function establishes a unique run directory (by default
#' in a sub-directory named "runs") and stores it's value for saving various
#' artifacts of training (e.g. model checkpoints, tensorflow logs, etc.).
#'
#' The `run_dir()` function returns the current run directory (`NULL` if none
#' yet established).
#'
#' If you utilize the automatic creation of run directories within the "runs"
#' directory then you can use the `latest_run()` and `latest_runs()` functions
#' to get the path(s) to the most recently created run directories and the
#' `clean_runs()` function to remove previously created run directories.
#'
#' @note You can also establish a run directory by defining the
#'   `TENSORFLOW_RUN_DIR` environment variable (this is technically equivalent
#'   to calling `use_run_dir()` within an R script).
#'
#' @param run_dir Path to run directory (`NULL` to automatically create a
#'   timestamped directory within the `runs_dir`)
#' @param runs_dir Parent directory for runs
#' @param quiet `FALSE` to prevent printing the path to the run dir
#' @param keep Number of most recent runs to keep when cleaning runs.
#' @param n Number of recent runs
#'
#' @name run_dir
#'
#' @export
use_run_dir <- function(run_dir = NULL, runs_dir = "runs", quiet = FALSE) {

  # if no run_dir is specified by the caller then figure out the right value
  if (is.null(run_dir)) {
    run_dir <- environment_run_dir() # check e.g. TENSORFLOW_RUN_DIR
    if (is.null(run_dir))
      run_dir <- unique_dir(runs_dir, format = "%Y-%m-%dT%H-%M-%SZ")
  }

  # create the directory if necessary
  if (!utils::file_test("-d", run_dir))
    if (!dir.create(run_dir, recursive = TRUE))
      stop("Unable to create run directory at ", run_dir)

  # this is new definition for the run_dir, save it
  .globals$run_dir$path <- run_dir

  # execute any pending writes
  for (name in ls(.globals$run_dir$pending_writes))
    .globals$run_dir$pending_writes[[name]](run_dir)

  # show message
  if (!quiet)
    message(paste("Using run directory at:", run_dir))

  # return invisibly
  invisible(run_dir)
}

#' @rdname run_dir
#' @export
run_dir <- function() {

  # do we already have a run_dir?
  if (!is.null(.globals$run_dir$path)) {

    .globals$run_dir$path

  # is there an environment variable that could establish a run_dir?
  } else if (!is.null(environment_run_dir())) {

    # set the environment variable as our current run directory
    use_run_dir(environment_run_dir())

  # no run_dir currently established
  } else {

    NULL

  }
}


#' @rdname run_dir
#' @export
latest_run <- function(runs_dir = "runs") {
  list_runs(runs_dir, latest_n = 1)
}


#' @rdname run_dir
#' @export
latest_runs <- function(runs_dir = "runs", n) {
  list_runs(runs_dir, latest_n = n)
}



#' @rdname run_dir
#' @export
clean_runs <- function(runs_dir = "runs", keep = NULL) {
  remove_runs <- list_runs(runs_dir)
  if (!is.null(keep)) {
    if (!is.numeric(keep))
      stop("keep must be a numeric value")
    if (keep >= length(remove_runs))
      invisible(0)
    else
      unlink(remove_runs[keep+1:length(remove_runs)], recursive = TRUE)
  } else {
    unlink(remove_runs, recursive = TRUE)
  }
}


# check for a run_dir provided by the environment
environment_run_dir <- function() {
  run_dir <- Sys.getenv("TENSORFLOW_RUN_DIR", unset = NA)
  if (!is.na(run_dir))
    run_dir
  else
    NULL
}

# helper function to write run data. writes to any active run_dir
# we have. if no run_dir is active then queue it as a pending write
# in case a run_directory becomes active. write_fn should take a
# single run_dir argument
write_run_data <- function(name, write_fn) {
  run_dir <- run_dir()
  if (!is.null(run_dir))
    write_fn(run_dir)
  else
    .globals$run_dir$pending_writes[[name]] <- write_fn
}


have_run_dir <- function() {
  !is.null(run_dir())
}


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






