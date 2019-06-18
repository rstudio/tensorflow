#' Install TensorFlow and its dependencies
#'
#' @inheritParams reticulate::conda_list
#'
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method is not
#'   available on Windows (as this isn't supported by TensorFlow). Note also
#'   that since this command runs without privillege the "system" method is
#'   available only on Windows.
#'
#' @param version TensorFlow version to install. Specify "default" to install
#'   the CPU version of the latest release. Specify "gpu" to install the GPU
#'   version of the latest release.
#'
#'   You can also provide a full major.minor.patch specification (e.g. "1.1.0"),
#'   appending "-gpu" if you want the GPU version (e.g. "1.1.0-gpu").
#'
#'   Alternatively, you can provide the full URL to an installer binary (e.g.
#'   for a nightly binary).
#'
#' @param envname Name of Python environment to install within
#'
#' @param extra_packages Additional Python packages to install along with
#'   TensorFlow.
#'
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio).
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
install_tensorflow <- function(method = c("auto", "virtualenv", "conda"),
                               conda = "auto",
                               version = "default",
                               envname = "r-tensorflow",
                               extra_packages = NULL,
                               restart_session = TRUE) {

  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop("Unable to install TensorFlow on this platform.",
         "Binary installation is only available for 64-bit platforms.")
  }

  method <- match.arg(method)

  # unroll version
  ver <- parse_tensorflow_version(version)

  version <- ver$version
  gpu <- ver$gpu
  package <- ver$package

  # Packages in this list should always be installed.

  default_packages <- c("tensorflow-hub")

  # Resolve TF probability version.
  if (!is.na(version) && substr(version, 1, 4) %in% c("1.12", "1.13", "1.14")) {
    default_packages <- c(default_packages, "tensorflow-probability")
  # install tfp-nightly
  } else if (is.na(version) ||(substr(version, 1, 4) %in% c("2.0.") || version == "nightly")) {
    default_packages <- c(default_packages, "tfp-nightly")
  }

  extra_packages <- unique(c(default_packages, extra_packages))

  # Main OS verification.
  if (is_osx() || is_linux()) {

    if (method == "conda") {
      install_conda(
        package = package,
        extra_packages = extra_packages,
        envname = envname,
        conda = conda
      )
    } else if (method == "virtualenv" || method == "auto") {
      install_virtualenv(
        package = package,
        extra_packages = extra_packages,
        envname = envname
      )
    }

  } else if (is_windows()) {

    if (method == "virtualenv") {
      stop("Installing TensorFlow into a virtualenv is not supported on Windows",
           call. = FALSE)
    } else if (method == "conda" || method == "auto") {

      install_conda(
        package = package,
        extra_packages = extra_packages,
        envname = envname,
        conda = conda
      )

    }

  } else {
    stop("Unable to install TensorFlow on this platform. ",
         "Binary installation is available for Windows, OS X, and Linux")
  }

  cat("\nInstallation complete.\n\n")

  if (restart_session && rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)
}

install_conda <- function(package, extra_packages, envname, conda) {



    # find if environment exists
    envname_exists <- envname %in% reticulate::conda_list(conda = conda)$name

    # remove environment
    if (envname_exists) {
      cat("Removing ", envname, " conda environment... \n")
      reticulate::conda_remove(envname = envname, conda = conda)
    }


    cat("Creating ", envname, " conda environment... \n")
    reticulate::conda_create(envname = envname, conda = conda)

    cat("Installing python modules...\n")
    reticulate::conda_install(
      envname = envname,
      packages = c(package, extra_packages),
      conda = conda,
      pip = TRUE # always use pip since it's the recommend way.
    )

}

install_virtualenv <- function(package, extra_packages, envname) {

    # find if environment exists
    envname_exists <- envname %in% reticulate::virtualenv_list()

    # remove environment
    if (envname_exists)
      reticulate::virtualenv_remove(envname = envname, confirm = FALSE)

    reticulate::virtualenv_create(envname = envname)

    reticulate::virtualenv_install(
      envname = envname,
      packages = c(package, extra_packages)
    )

}

parse_tensorflow_version <- function(version) {

  default_version <- "1.13.1"

  ver <- list(
    version = default_version,
    gpu = FALSE,
    package = NULL
  )

  if (version == "default") {

    ver$package <- paste0("tensorflow==", ver$version)

    # default gpu version
  } else if (version == "gpu") {

    ver$gpu <- TRUE
    ver$package <- paste0("tensorflow-gpu==", ver$version)

    # gpu qualifier provided
  } else if (grepl("-gpu$", version)) {

    split <- strsplit(version, "-")[[1]]
    ver$version <- split[[1]]
    ver$gpu <- TRUE

    # full path to whl.
  } else if (grepl("^.*\\.whl$", version)) {

    ver$gpu <- NA
    ver$version <- NA

    if (grepl("^http", version))
      ver$package <- version
    else
      ver$package <- normalizePath(version)

    # another version
  } else {

    ver$version <- version

  }

  # find the right package for nightly and other versions
  if (is.null(ver$package)) {

    if (ver$version == "nightly") {

      if (ver$gpu) {
        ver$package <- "tf-nightly-gpu"
      } else {
        ver$package <- "tf-nightly"
      }

    } else {

      if (ver$gpu) {
        ver$package <- paste0("tensorflow-gpu==", ver$version)
      } else {
        ver$package <- paste0("tensorflow==", ver$version)
      }

    }

  }

  ver
}


#' Install additional Python packages alongside TensorFlow
#'
#' This function is deprecated. Use the `extra_packages` argument to
#' `install_tensorflow()` to install additional packages.
#'
#' @param packages Python packages to install
#' @param conda Path to conda executable (or "auto" to find conda using the PATH
#'   and other conventional install locations). Only used when TensorFlow is
#'   installed within a conda environment.
#'
#' @export
install_tensorflow_extras <- function(packages, conda = "auto") {
  message("Extra packages not installed (this function is deprecated). \n",
          "Use the extra_packages argument to install_tensorflow() to ",
          "install additional packages.")
}
