

#' Install TensorFlow and it's dependencies
#'
#' @inheritParams reticulate::conda_list
#'
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that since this command runs without
#'   privillege the "system" method is available only on Windows.
#' @param package_url URL of the TensorFlow package to install (if not
#'   specified this is determined automatically)
#' @param gpu Install the GPU version of TensorFlow
#'
#' @export
install_tensorflow <- function(method = c("auto", "virtualenv", "conda", "system"),
                               package_url = NULL,
                               gpu = FALSE,
                               conda = "auto") {

  # verify os
  if (!is_windows() && !is_osx() && !is_ubuntu()) {
    stop("Unable to install TensorFlow on this platform.",
         "Binary installation is available for Windows, Linux, and Ubuntu")
  }

  # resolve and validate method
  method <- match.arg(method)
  if (identical(method, "system") && !is_windows()) {
    stop("Installing TensorFlow into the system library is only supported on Windows",
         call. = FALSE)
  }

  # flags indicating what methods are available
  method_available <- function(name) method %in% c("auto", name)
  virtualenv_available <- method_available("virtualenv")
  conda_available <- method_available("conda")
  system_available <- is_windows() && method_available("site")

  # resolve and look for conda
  conda <- conda_binary(conda)
  have_conda <- conda_available && !is.null(conda)

  # mac and linux
  if (is_unix()) {

    # check for explicit conda method
    if (identical(method, "conda")) {

      # ensure we have conda
      if (!have_conda)
        stop("Conda installation failed (no conda binary found)\n", call. = FALSE)

      # do install
      install_tensorflow_conda(conda, package_url, gpu)

    } else {

      # find system python binary
      python <- python_unix_binary("python")
      if (is.null(python))
        stop("Unable to locate Python on this system.", call. = FALSE)

      # find other required tools
      pip <- python_unix_binary("pip")
      have_pip <- nzchar(pip)
      virtualenv <- python_unix_binary("virtualenv")
      have_virtualenv <- virtualenv_available && nzchar(virtualenv)

      # if we don't have pip and virtualenv then try for conda if it's allowed
      if ((!have_pip || !have_virtualenv) && have_conda) {

        install_tensorflow_conda(conda, package_url, gpu)


      # otherwise this is either an "auto" installation w/o working conda
      # or it's an explicit "virtualenv" installation
      } else {

        # validate that we have the required tools for the method
        install_commands <- NULL
        if (is_osx()) {
          if (!have_pip)
            install_commands <- c(install_commands, "$ sudo easy_install pip")
          if (!have_virtualenv)
            install_commands <- c(install_commands, "$ sudo pip install --upgrade virtualenv")
          if (!is.null(install_commands))
            install_commands <- paste(install_commands, collapse = "\n")
        } else {
          if (!have_pip)
            install_commands <- c(install_commands, "python-pip python-dev")
          if (!have_virtualenv)
            install_commands <- c(install_commands, "python-virtualenv")
          if (!is.null(install_commands))
            install_commands <- paste("$ sudo apt-get install", paste(install_commands))
        }
        if (!is.null(install_commands)) {
          stop("Prerequisites for installing TensorFlow not available.\n\n",
               "Execute the following at a terminal to install the prerequisites:\n\n",
               install_commands, "\n\n", call. = FALSE)
        }

        # do the install
        install_tensorflow_virtualenv(python, pip, virtualenv, package_url, gpu)

      }
    }

  # windows installation
  } else {


    # confirm python 3.5

    # python -m pip install --upgrade pip
    # pip install tensorflow

    # http://eddiejackson.net/wp/?p=10276
    # /passive

  }

}

install_tensorflow_conda <- function(conda, package_url, gpu) {

  # create conda environment if we need to
  envname <- "r-tensorflow"
  conda_envs <- conda_list(conda = conda)
  conda_env <- subset(conda_envs, conda_envs$name == envname)
  if (nrow(conda_env) == 1)
    python <- conda_env$python
  else
    python <- conda_create(envname, packages = "numpy", conda = conda)

  # determine package_url
  # TODO: change for linux
  if (is.null(package_url)) {
    tf_version <- "1.1.0"
    platform <- ifelse(is_osx(), "mac", "linux")
    package_url <- sprintf(
      "https://storage.googleapis.com/tensorflow/%s/%s/tensorflow-%s-%s-none-any.whl",
      platform,
      ifelse(gpu, "gpu", "cpu"),
      tf_version,
      ifelse(python_version(python) >= "3.0", "py3", "py2")
    )
  }

  # install base tensorflow using pip
  conda_install(envname, package_url, pip = TRUE, conda = conda)

  # install additional packages
  conda_install(envname, .tf_extra_pkgs, conda = conda)

}

install_tensorflow_virtualenv <- function(python, pip, virtualenv, package_url, gpu) {

  # determine pip version to use
  is_python3 <- python_version(python) >= "3.0"
  pip_version <- ifelse(is_python3, "pip3", "pip")

  # create virtualenv
  virtualenv_root <- "~/.virtualenvs"
  if (!file.exists(virtualenv_root))
    dir.create(virtualenv_root)
  virtualenv_path <- file.path(virtualenv_root, "r-tensorflow")
  cat("Creating virtualenv for TensorFlow at ", virtualenv_path, "\n")
  result <- system2(virtualenv, shQuote(c(
    "--system-site-packages",
    path.expand(virtualenv_path)))
  )
  if (result != 0L)
    stop("Error ", result, " occurred creating virtualenv at ", virtualenv_path,
         call. = FALSE)

  # install tensorflow and related dependencies
  virtualenv_bin <- function(bin) path.expand(file.path(virtualenv_path, "bin", bin))
  package <- package_url
  if (is.null(package))
    package <- sprintf("tensorflow%s", ifelse(gpu, "-gpu", ""))
  pkgs <- c(tf_pkg, .tf_extra_pkgs)
  cmd <- sprintf("source %s && %s install --ignore-installed --upgrade %s",
                 shQuote(path.expand(virtualenv_bin("activate"))),
                 shQuote(path.expand(virtualenv_bin(pip_version))),
                 paste(shQuote(pkgs), collapse = " "))
  cat("Installing TensorFlow...\n")
  result <- system(cmd)
  if (result != 0L)
    stop("Error ", result, " occurred installing TensorFlow", call. = FALSE)
}



python_unix_binary <- function(bin) {
  locations <- file.path(c("/usr/local/bin", "/usr/bin"), bin)
  locations <- locations[file.exists(locations)]
  if (length(locations) > 0)
    locations[[1]]
  else
    NULL
}

python_version <- function(python) {

  # check for the version
  result <- system2(python, "--version", stdout = TRUE, stderr = TRUE)

  # check for error
  error_status <- attr(result, "status")
  if (!is.null(error_status))
    stop("Error ", error_status, " occurred while checking for python version", call. = FALSE)

  # parse out the major and minor version numbers
  matches <- regexec("^[^ ]+\\s+(\\d+)\\.(\\d+).*$", result)
  matches <- regmatches(result, matches)[[1]]
  if (length(matches) != 3)
    stop("Unable to parse Python version '", result[[1]], "'", call. = FALSE)

  # return as R numeric version
  numeric_version(paste(matches[[2]], matches[[3]], sep = "."))
}


# additional dependencies to install (required by some features of keras)
.tf_extra_pkgs <- c("scipy", "h5py", "pyyaml",  "requests",  "Pillow")


