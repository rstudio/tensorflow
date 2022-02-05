#' Install TensorFlow and its dependencies
#'
#' `install_tensorflow()` installs just the tensorflow python package and it's
#' direct dependencies. For a more complete installation that includes
#' additional optional dependencies, use [`keras::install_keras()`].
#'
#' @details You may be prompted you if you want it to download and install
#'   miniconda if reticulate did not find a non-system installation of python.
#'   Miniconda is the recommended installation method for most users, as it
#'   ensures that the R python installation is isolated from other python
#'   installations. All python packages will by default be installed into a
#'   self-contained conda or venv environment named "r-reticulate". Note that
#'   "conda" is the only supported method on Windows.
#'
#'   If you initially declined the miniconda installation prompt, you can later
#'   manually install miniconda by running [`reticulate::install_miniconda()`].
#'
#' @section Custom Installation: `install_tensorflow()` or
#'   `keras::install_keras()` isn't required to use tensorflow with the package.
#'   If you manually configure a python environment with the required
#'   dependencies, you can tell R to use it by pointing reticulate at it,
#'   commonly by setting an environment variable:
#'
#'   ``` R
#'   Sys.setenv("RETICULATE_PYTHON" = "~/path/to/python-env/bin/python")
#'   ```
#'
#' @section Apple Silicon: Tensorflow on Apple Silicon is not officially
#'   supported by the Tensorflow maintainers. However Apple has published a
#'   custom version of Tensorflow compatible with Arm Macs.
#'   `install_tensorflow()` will install the special packages `tensorflow-macos`
#'   and `tensorflow-metal` on Arm Macs. See
#'   \url{https://developer.apple.com/metal/tensorflow-plugin/} for instructions
#'   on how to do the equivalent manually. Please note that this is an
#'   experimental build of both Python and Tensorflow, with known issues. In
#'   particular, certain operations will cause errors, but can often be remedied
#'   by pinning them to the CPU. For example:
#'
#'   ```` R
#'   x <- array(runif(64*64), c(1, 64, 64))
#'   keras::layer_random_rotation(x, .5)  # Error:
#'   # No registered 'RngReadAndSkip' OpKernel for 'GPU' devices
#'   # Pin the operation to the CPU to avoid the error
#'   with(tf$device("CPU"), keras::layer_random_rotation(x, .5) ) # No Error
#'   ````
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
#'
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
#' @param python_version,conda_python_version Pass a string like "3.8" to
#'   request that conda install a specific Python version. This is ignored when
#'   attempting to install in a Python virtual environment. Note that the Python
#'   version must be compatible with the requested Tensorflow version, documented
#'   here: <https://www.tensorflow.org/install/pip#system-requirements>
#'
#' @param pip_ignore_installed Whether pip should ignore installed python
#'   packages and reinstall all already installed python packages. This defaults
#'   to `TRUE`, to ensure that TensorFlow dependencies like NumPy are compatible
#'   with the prebuilt TensorFlow binaries.
#'
#' @param ... other arguments passed to [`reticulate::conda_install()`] or
#'   [`reticulate::virtualenv_install()`], depending on the `method` used.
#'
#' @seealso [`keras::install_keras()`]
#'
#' @export
install_tensorflow <- function(method = c("auto", "virtualenv", "conda"),
                               conda = "auto",
                               version = "default",
                               envname = NULL,
                               extra_packages = NULL,
                               restart_session = TRUE,
                               conda_python_version = NULL,
                               ...,
                               pip_ignore_installed = TRUE,
                               python_version = conda_python_version) {

  method <- match.arg(method)

  if(is_mac_arm64())
    return(install_tensorflow_mac_arm64(
      method = method,
      conda = conda,
      version = version,
      envname = envname,
      extra_packages = extra_packages,
      restart_session = restart_session,
      python_version = python_version
    ))


  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop("Unable to install TensorFlow on this platform.",
         "Binary installation is only available for 64-bit platforms.")
  }


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
    pip_ignore_installed = pip_ignore_installed,
    ...
  )

  cat("\nInstallation complete.\n\n")

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)
}


default_version <- numeric_version("2.8")

parse_tensorflow_version <- function(version) {
  # returns unquoted string directly passable to pip, e.g 'tensorflow==2.5.*'

  if(is.null(version) || is.na(version) || version %in% c("", "release"))
    return("tensorflow")

  version <- as.character(version) # if numeric_version()

  if(version == "release-cpu")
    return("tensorflow-cpu")

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

install_tensorflow_mac_arm64 <- function(method = c("auto", "virtualenv", "conda"),
                                         conda = "auto",
                                         version = "default",
                                         envname = NULL,
                                         extra_packages = NULL,
                                         restart_session = TRUE,
                                         python_version = NULL) {
  stopifnot(is_mac_arm64())
  method <- match.arg(method)

  if(method == "virtualenv")
    stop("Tensorflow on Arm Macs only supported in Conda Python.",
         " Install Miniconda with `reticulate::install_miniconda()`")

  have_conda <- !is.null(tryCatch(conda_binary(conda), error = function(e) NULL))
  if (!have_conda) {
    stop("Tensorflow installation failed (no conda binary found)\n\n",
         "Install Miniconda by running `reticulate::install_miniconda()`",
         "before installing Tensorflow.\n",
         call. = FALSE)
  }

  if(!version %in% c("default", "release"))
    warning(
      "Only `version = 'default'` supported on Arm Macs at this time.",
      "Request for Tensorflow version '", version, "' ignored.")

  conda <- conda_binary(conda)

  install <- function(pkg, ...) {
    message("Installing: ", paste(pkg, collapse = ", "))
    py_install(pkg, ...,  method = "conda", conda = conda,
               envname = envname, python_version = python_version)
  }

  extra_packages <- setdiff(extra_packages, c("scipy", "requests", "h5py"))
  # "scipy","requests", "h5py" # pinned versions pulled in by "tensorflow-deps" already
  # "pyyaml" no longer needed

  # split extra_packages into pre and post install lists
  # dangerous to invoke conda after pip, so everything conda goes first,
  # with official tensorflow-* packages going last so their pinned dependancy version wins.
  # packages like h5py o Pillow not available via pip yet for arm mac
  # and tensorflow-hub via conda pulls in the wrong dependencies, so must come from pip.
  extra_packages_pip <- intersect(extra_packages, c("tensorflow-hub",
                                                    "tensorflow-datasets"))
  extra_packages_conda <- setdiff(extra_packages, extra_packages_pip)

  if(length(extra_packages_conda))
    install(extra_packages_conda) # keras passes: c("pandas", "Pillow")

  os_ver <- numeric_version(gsub("macOS [a-zA-Z ]*([0-9.]+)", "\\1",
                                 utils::sessionInfo()$running, perl = TRUE))

  if (os_ver < "12") {
    install("tensorflow-deps==2.6", channel = c("apple", "conda-forge"))
    install("tensorflow-macos==2.6.0", pip = TRUE)
    install("tensorflow-metal==0.2.0", pip = TRUE)
  } else {
    install("tensorflow-deps", channel = c("apple", "conda-forge"))
    install("tensorflow-macos", pip = TRUE)
    install("tensorflow-metal", pip = TRUE)
  }

  if(length(extra_packages_pip))
    install(extra_packages_pip, pip = TRUE)

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible()
}
