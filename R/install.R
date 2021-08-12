#' Install TensorFlow and its dependencies
#'
#' `install_tensorflow()` installs just the tensorflow python package and it's
#' direct dependencies. For a more complete installation that includes
#' additional optional dependencies, use `keras::install_keras()`.
#'
#' @details You may be prompted you if you want it to download and install
#'   miniconda if reticulate did not find a non-system installation of python.
#'   Miniconda is the recommended installation method for most users, as it
#'   ensures that the R python installation is isolated from other python
#'   installations. All python packages will by default be installed into a
#'   self-contained conda or venv environment named "r-reticulate". Note that
#'   "conda" is the only supported method on Windows.
#'
#' @section Custom Installation: `install_tensorflow()` or
#'   `keras::install_keras()` isn't required to use tensorflow with the package.
#'   If you manually configure a python environment with the required
#'   dependencies, you can tell R to use it by pointing reticulate at it,
#'   commonly by setting an environment variable:
#'
#'   ``` Sys.setenv("RETICULATE_PYTHON" = "~/path/to/python-env/bin/python") ```
#'
#' @section Apple Silicon: Tensorflow on Apple Silicon is not officially
#'   supported by the tensorflow maintainers. It is known that there can be
#'   issues running the official Tensorflow package under Rosetta as well.
#'   Fortunately, for the time being Apple has published a custom version of
#'   Tensorflow compatible with M1 macs. Installation instructions can be found
#'   at: \url{https://developer.apple.com/metal/tensorflow-plugin/}. Please note
#'   that this is an experimental build of both python and tensorflow. After
#'   following the instructions provided by Apple, you can advise reticulate to
#'   use that python installation by placing the following in your
#'   `.Renviron` file:
#'
#'   ``` RETICULATE_PYTHON = "~/miniforge3/bin/python" ```
#'
#' @section Additional Packages:
#'
#'   If you wish to add additional PyPI packages to your Keras / TensorFlow
#'   environment you can either specify the packages in the `extra_packages`
#'   argument of `install_tensorflow()` or `install_keras()`, or alternatively
#'   install them into an existing environment using the
#'   [reticulate::py_install()] function. Note that `install_keras()` includes a
#'   set of additional python packages by default, see `?keras::install_keras`
#'   for details.
#'
#' @md
#'
#' @inheritParams reticulate::py_install
#' @param version TensorFlow version to install. Valid values include:
#'
#'   +  `"default"` installs  `r default_version`
#'
#'   + `"release"` installs the latest release version of tensorflow (which may
#'   be incompatible with the current version of the R package)
#'
#'   + A version specification like `"2.4"` or `"2.4.0"`. Note that if the patch
#'   version is not supplied, the latest patch release is installed (e.g.,
#'   `"2.4"` today installs version "2.4.2")
#'
#'   + `nightly` for the latest available nightly build.
#'
#'   + To any specification, you can append "-cpu" to install the cpu version
#'   only of the package (e.g., `"2.4-cpu"`)
#'
#'   + The full URL or path to a installer binary or python *.whl file.
#'
#' @param extra_packages Additional Python packages to install along with
#'   TensorFlow.
#'
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio).
#'
#' @param python_version,conda_python_version the python version installed in
#'   the created conda environment. Ignored when attempting to install with a
#'   Python virtual environment.
#'
#' @param ... other arguments passed to [reticulate::conda_install()] or
#'   [reticulate::virtualenv_install()], depending on the `method` used.
#'
#' @seealso keras::install_keras()
#'
#' @export
install_tensorflow <- function(method = c("auto", "virtualenv", "conda"),
                               conda = "auto",
                               version = "default",
                               envname = NULL,
                               extra_packages = NULL,
                               restart_session = TRUE,
                               conda_python_version = "3.7",
                               ...,
                               python_version = conda_python_version) {

  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop("Unable to install TensorFlow on this platform.",
         "Binary installation is only available for 64-bit platforms.")
  }

  method <- match.arg(method)

  # some special handling for windows
  if (is_windows()) {

    # conda is the only supported method on windows
    method <- "conda"

    # confirm we actually have conda - let reticulate prompt miniconda installation
    have_conda <- !is.null(tryCatch(conda_binary(conda), error = function(e) NULL))
    if (!have_conda) {
      stop("Tensorflow installation failed (no conda binary found)\n\n",
           "Install Miniconda by running `reticulate::install_miniconda()` or ",
           "install Anaconda for Python 3.x (https://www.anaconda.com/download/#windows) ",
           "before installing Tensorflow.\n",
           call. = FALSE)
    }

    # avoid DLL in use errors
    if (py_available()) {
      stop("You should call install_keras() only in a fresh ",
           "R session that has not yet initialized Keras and TensorFlow (this is ",
           "to avoid DLL in use errors during installation)")
    }
  }

  packages <- unique(c(
    parse_tensorflow_version(version),
    as.character(extra_packages)
  ))

  # don't double quote if packages were shell quoted already
  # TODO: patch quoting in reticulate::pip_install, or maybe py_install()
  packages <- shQuote(gsub("[\"']", "", packages))

  # message("Installing the python pip packages :\n",
          # paste("  -", packages, collapse = "\n"))

  reticulate::py_install(
    packages       = packages,
    envname        = envname,
    method         = method,
    conda          = conda,
    python_version = python_version,
    pip            = TRUE,
    ...
  )

  cat("\nInstallation complete.\n\n")

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)
}


default_version <- numeric_version("2.6")

parse_tensorflow_version <- function(version) {
  # returns unquoted string directly passable to pip, e.g 'tensorflow==2.5.*'

  if(is.null(version) || is.na(version) || version %in% c("", "release"))
    return("tensorflow")

  version <- as.character(version) # if numeric_version()

  # full path to whl.
  if (grepl("^.*\\.whl$", version))
    return(normalizePath(version))

  if (grepl("nightly", version)) {
    if(!startsWith(version, "tf-"))
      version <- paste0("tf-", version)
    return(version)
  }

  package <- "tensorflow"
  if(grepl(".*(cpu|gpu)$", version)) {
    # append {-cpu,-gpu} suffix to package
    package <- sprintf("%s-%s", package, sub(".*-(cpu|gpu)$", "\\1", version))

    # strip -?{cpu,gpu} suffix from version
    version <- sub("(.*?)-?([cg]pu)$", "\\1", version)
  }

  if(version %in% c("default", ""))
    version <- default_version

  if(!grepl("[><=]", version))
    version <- sprintf("==%s.*", version)

  paste0(package, version)
}


#' (Defunct) Install additional Python packages alongside TensorFlow
#'
#' This function is deprecated. Use the `extra_packages` argument to
#' `install_tensorflow()` or `reticulate::py_install()` to install additional packages.
#'
#' @param packages Python packages to install
#' @param conda Path to conda executable (or "auto" to find conda using the PATH
#'   and other conventional install locations). Only used when TensorFlow is
#'   installed within a conda environment.
#'
#' @export
install_tensorflow_extras <- function(packages, conda = "auto") {
     stop("Extra packages not installed (this function is deprecated). \n",
          "Use the extra_packages argument to install_tensorflow() or reticulate::py_install() to ",
          "install additional packages.")
}
