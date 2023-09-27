
clean_python_tmp_dir <- function() {
  if (!reticulate::py_available())
    return()

  tryCatch({
    python_temp_dir <- dirname(
      reticulate::py_run_string(
        "import tempfile; x=tempfile.NamedTemporaryFile().name",
        local = TRUE
      )$x
    )
    detritus <- list.files(path = python_temp_dir,
                           pattern = "__autograph_generated_file|__pycache__",
                           all.files = TRUE, include.dirs = TRUE, no.. = TRUE,
                           full.names = TRUE)

    if(length(detritus)) {
      # cat("Unlinking:\n",
      #     paste("-", detritus, "\n"), sep = "")
      unlink(detritus, TRUE, TRUE)
    }
  },

  error = function(e) {
    warning(e)
  })
}

withr::defer(clean_python_tmp_dir(), teardown_env())
