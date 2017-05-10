

#' Install TensorFlow and it's dependencies
#'
#' @param gpu Install the GPU version of TensorFlow
#'
#' @export
install_tensorflow <- function(gpu = FALSE) {

  # verify os
  if (!is_windows() && !is_osx() && !is_ubuntu()) {
    stop("Unable to install TensorFlow on this platform.",
         "Binary installation is available for Windows, Linux, and Ubuntu")
  }

  # on mac and linux we'll create a virtualenv
  if (is_unix()) {

    # find required tools
    python <- python_unix_binary("python")
    if (is.null(python))
      stop("Unable to location Python on this system.", call. = FALSE)
    is_python3 <- python_version(python) >= "3.0"

    # find other required tools
    pip_version <- ifelse(is_python3, "pip3", "pip")
    pip <- python_unix_binary("pip")
    have_pip <- nzchar(pip)
    virtualenv <- python_unix_binary("virtualenv")
    have_virtualenv <- nzchar(virtualenv)

    # print install commands and abort if we don't have both pip and virtualenv
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

    # create virtualenv
    virtualenv_root <- "~/.virtualenvs"
    if (!file.exists(virtualenv_root))
      dir.create(virtualenv_root)
    virtualenv_path <- file.path(virtualenv_root, "tensorflow")
    cat("Creating virtualenv for TensorFlow at ", virtualenv_path, "\n")
    result <- system2(virtualenv, shQuote(c(
      "--system-site-packages",
      path.expand(file.path(virtualenv_root, "tensorflow"))))
    )
    if (result != 0L)
      stop("Error ", result, " occurred creating virtualenv at ", virtualenv_path,
           call. = FALSE)

    # install tensorflow
    virtualenv_bin <- function(bin) path.expand(file.path(virtualenv_path, "bin", bin))
    pkgs <- c(sprintf("tensorflow%s", ifelse(gpu, "-gpu", "")),
              "scipy", "h5py", "pyyaml",  "requests",  "Pillow")
    cmd <- sprintf("source %s && %s install --upgrade %s",
                   shQuote(path.expand(virtualenv_bin("activate"))),
                   shQuote(path.expand(virtualenv_bin(pip_version))),
                   paste(shQuote(pkgs), collapse = " "))
    cat("Installing TensorFlow...\n")
    result <- system(cmd)
    if (result != 0L)
      stop("Error ", result, " occurred installing TensorFlow", call. = FALSE)



  # on windows we'll just install into the user's library
  } else {



  }

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




