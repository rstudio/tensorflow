

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
install_tensorflow <- function(method = c("auto", "virtualenv", "conda", "system"),
                               conda = "auto",
                               version = "default",
                               envname = "r-tensorflow",
                               extra_packages = NULL,
                               restart_session = TRUE) {

  # verify os
  if (!is_windows() && !is_osx() && !is_linux()) {
    stop("Unable to install TensorFlow on this platform. ",
         "Binary installation is available for Windows, OS X, and Linux")
  }

  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop("Unable to install TensorFlow on this platform.",
         "Binary installation is only available for 64-bit platforms.")
  }

  # resolve and validate method
  method <- match.arg(method)
  if (identical(method, "system") && !is_windows()) {
    stop("Installing TensorFlow into the system library is only supported on Windows",
         call. = FALSE)
  }
  if (identical(method, "virtualenv") && is_windows()) {
    stop("Installing TensorFlow into a virtualenv is not supported on Windows",
         call. = FALSE)
  }

  # unroll version
  ver <- parse_tensorflow_version(version)
  version <- ver$version
  gpu <- ver$gpu
  packages <- ver$packages

  # flags indicating what methods are available
  method_available <- function(name) method %in% c("auto", name)
  virtualenv_available <- method_available("virtualenv")
  conda_available <- method_available("conda")
  system_available <- is_windows() && method_available("site")

  # resolve and look for conda
  conda_bin <- tryCatch(conda_binary(conda), error = function(e) NULL)
  have_conda <- conda_available && !is.null(conda_bin)

  # mac and linux
  if (is_unix()) {

    # check for explicit conda method
    if (identical(method, "conda")) {

      # validate that we have conda
      if (!have_conda)
        stop("Conda installation failed (no conda binary found)\n", call. = FALSE)

      # do install
      install_tensorflow_conda(conda, version, gpu, envname, packages, extra_packages)

    } else {

      # find system python binary
      pyver <- ""
      python <- python_unix_binary("python")
      if (is.null(python)) {
        # try for python3 if we are on linux
        if (is_linux()) {
          python <- python_unix_binary("python3")
          if (is.null(python))
            stop("Unable to locate Python on this system.", call. = FALSE)
          pyver <- "3"
        }
      }

      # find other required tools
      pip <- python_unix_binary(paste0("pip", pyver))
      have_pip <- !is.null(pip)
      virtualenv <- python_unix_binary("virtualenv")
      have_virtualenv <- virtualenv_available && !is.null(virtualenv)

      # if we don't have pip and virtualenv then try for conda if it's allowed
      if ((!have_pip || !have_virtualenv) && have_conda) {

        install_tensorflow_conda(conda, version, gpu, envname, packages, extra_packages)


      # otherwise this is either an "auto" installation w/o working conda
      # or it's an explicit "virtualenv" installation
      } else {

        # validate that we have the required tools for the method
        install_commands <- NULL
        if (is_osx()) {
          if (!have_pip)
            install_commands <- c(install_commands, "$ sudo /usr/bin/easy_install pip")
          if (!have_virtualenv) {
            if (is.null(pip))
              pip <- "/usr/local/bin/pip"
            install_commands <- c(install_commands, sprintf("$ sudo %s install --upgrade virtualenv", pip))
          }
          if (!is.null(install_commands))
            install_commands <- paste(install_commands, collapse = "\n")
        } else if (is_ubuntu()) {
          if (!have_pip) {
            install_commands <- c(install_commands, paste0("$ sudo apt-get install python", pyver ,"-pip"))
            pip <- paste0("/usr/bin/pip", pyver)
          }
          if (!have_virtualenv) {
            if (identical(pyver, "3"))
              install_commands <- c(install_commands, paste("$ sudo", pip, "install virtualenv"))
            else
              install_commands <- c(install_commands, "$ sudo apt-get install python-virtualenv")
          }
          if (!is.null(install_commands))
            install_commands <- paste(install_commands, collapse = "\n")
        } else {
          if (!have_pip)
            install_commands <- c(install_commands, "pip")
          if (!have_virtualenv)
            install_commands <- c(install_commands, "virtualenv")
          if (!is.null(install_commands)) {
            install_commands <- paste("Please install the following Python packages before proceeding:",
                                      paste(install_commands, collapse = ", "))
          }
        }
        if (!is.null(install_commands)) {

          # if these are terminal commands then add special preface
          if (grepl("^\\$ ", install_commands)) {
            install_commands <- paste0(
              "Execute the following at a terminal to install the prerequisites:\n\n",
              install_commands
            )
          }

          stop("Prerequisites for installing TensorFlow not available.\n\n",
               install_commands, "\n\n", call. = FALSE)
        }

        # do the install
        install_tensorflow_virtualenv(python, virtualenv, version, gpu, envname, packages, extra_packages)

      }
    }

  # windows installation
  } else {

    # determine whether we have system python
    python_versions <- py_versions_windows()
    python_versions <- python_versions[python_versions$type == "PythonCore",]
    python_versions <- python_versions[python_versions$version %in% c("3.5","3.6"),]
    python_versions <- python_versions[python_versions$arch == "x64",]
    have_system <- nrow(python_versions) > 0
    if (have_system)
      python_system_version <- python_versions[1,]

    # resolve auto
    if (identical(method, "auto")) {

        if (!have_system && !have_conda) {
          stop("Installing TensorFlow requires a 64-bit version of Python 3.5 or 3.6\n\n",
               "Please install 64-bit Python 3.5 or 3.6 to continue, supported versions include:\n\n",
               " - Anaconda Python (Recommended): https://www.anaconda.com/download/#windows\n",
               " - Python Software Foundation   : https://www.python.org/downloads/\n\n",
               call. = FALSE)
        } else if (have_conda) {
          method <- "conda"
        } else if (have_system) {
          method <- "system"
        }
    }

    if (identical(method, "conda")) {

      # validate that we have conda
      if (!have_conda) {
        stop("Conda installation failed (no conda binary found)\n\n",
             "Install Anaconda 3.x for Windows (https://www.anaconda.com/download/#windows)\n",
             "before installing TensorFlow.",
             call. = FALSE)
      }

      # do the install
      install_tensorflow_conda(conda, version, gpu, envname, packages, extra_packages)

    } else if (identical(method, "system")) {

      # if we don't have it then error
      if (!have_system) {
        stop("Installing TensorFlow requires a 64-bit version of Python 3.5 or 3.6\n\n",
             "Please install 64-bit Python 3.5 or 3.6 this location to continue:\n\n",
             " - https://www.python.org/downloads/\n\n",
             call. = FALSE)
      }

      # do system installation
      python <- python_system_version$executable_path
      pip <- file.path(python_system_version$install_path, "Scripts", "pip.exe")
      install_tensorflow_windows_system(python, pip, version, gpu, packages, extra_packages)

    } else {
      stop("Invalid/unexpected installation method '", method, "'",
           call. = FALSE)
    }
  }

  cat("\nInstallation complete.\n\n")

  if (restart_session && rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)
}

install_tensorflow_conda <- function(conda, version, gpu, envname, packages, extra_packages = NULL) {

  # remove existing conda environment if we need to (necessary to work around TF upgrade bugs)
  conda_envs <- conda_list(conda = conda)
  conda_env <- subset(conda_envs, conda_envs$name == envname)
  if (nrow(conda_env) == 1)
    conda_remove(envname, conda = conda)

  # create conda environment
  cat("Creating", envname, "conda environment for TensorFlow installation...\n")
  python_packages <- ifelse(is_windows(), "python=3.6", "python")
  python <- conda_create(envname, packages = python_packages, conda = conda)


  # Short circuit to install everything with conda when no custom packages
  # (typically tf-nightly or a daily build URL) and no gpu-enabled build
  # is requested (conda doesn't currently have GPU enabled builds.
  #
  # This avoids any use of pip, which addresses the following issue:
  # https://github.com/rstudio/keras/issues/147
  #
  # This issue is in turn created by two other issues:
  #
  # 1) TensorBoard appears to rely on an older version of html5lib which is
  #    force installed, and which as a result breaks pip:
  #    https://github.com/tensorflow/tensorboard/issues/588
  #
  # 2) Anaconda 5.0.0 is unable to recover from this because the installation
  #    of the old version of html5lib actually propagates to the root
  #    environment, which permantely breaks pip for *all* conda environments:
  #    https://github.com/conda/conda/issues/6079
  #
  # Hopefully these two issues will be addressed and we can return to using
  # pip in all scenarios (as that is the officially supported version)
  #
  if (is_windows() && is.null(packages) && gpu == FALSE) {
    conda_forge_install(
      envname,
      tf_pkgs(version, gpu, packages, extra_packages = extra_packages),
      conda = conda
    )
    return(invisible(NULL))
  }

  # determine tf version
  if (version == "latest") {
    cat("Determining latest installable release of TensorFlow...")
    releases <- fromJSON("https://api.github.com/repos/tensorflow/tensorflow/releases")
    latest <- subset(releases, grepl("^v\\d+\\.\\d+\\.\\d+$", releases$tag_name))$tag_name[[1]]
    version <- sub("v", "", latest)
    # workaround the fact that v1.X.1 releases often have no tarball
    version_split <- strsplit(version, ".", fixed = TRUE)[[1]]
    if (length(version_split) > 2 && version_split[[length(version_split)]] != "0")
      version_split <- c(version_split[-length(version_split)], "0")
    version <- paste(version_split, collapse = ".")
    cat("done\n")
  }

  # determine python version
  py_version <- python_version(python)
  py_version_str <- if (is_osx()) {
    if (py_version >= "3.0")
      "py3-none"
    else
      "py2-none"
  } else {
    if (py_version >= "3.0") {
      ver <- gsub(".", "", as.character(py_version), fixed = TRUE)
      sprintf("cp%s-cp%sm", ver, ver)
    } else {
      "cp27-none"
    }
  }

  # determine arch
  arch <- ifelse(is_windows(), "win_amd64", ifelse(is_osx(), "any", "linux_x86_64"))

  # determine packages url if necessary
  if (is.null(packages)) {
    # version must have 3 digits
    if (grepl("^\\d+\\.\\d+$", version))
      version <- paste0(version, ".0")
    platform <- ifelse(is_windows(), "windows", ifelse(is_osx(), "mac", "linux"))
    packages <- sprintf(
      "https://storage.googleapis.com/tensorflow/%s/%s/tensorflow%s-%s-%s-%s.whl",
      platform,
      ifelse(gpu, "gpu", "cpu"),
      ifelse(gpu, "_gpu", ""),
      version,
      py_version_str,
      arch
    )
  }

  # install base tensorflow using pip
  cat("Installing TensorFlow...\n")
  conda_install(envname, packages, pip = TRUE, conda = conda)

  # install additional packages
  conda_install(envname, tf_extra_pkgs(), conda = conda)

  # install extra packages (use pip to ensure we don't get legacy versions, set
  # pip_ignore_installed to FALSE to ensure that pip source installs on
  # windows don't attempt to override conda binary packages (e.g. SciPy which
  # will likely fail to install via pip due to compilation dependencies)
  if (!is.null(extra_packages)) {
    conda_install(
      envname,
      extra_packages,
      pip = TRUE,
      pip_ignore_installed = FALSE,
      conda = conda)
  }
}


conda_forge_install <- function(envname, packages, conda = "auto") {

  # resolve conda binary
  conda <- conda_binary(conda)

  # use native conda package manager with conda forge enabled
  result <- system2(conda, shQuote(c("install", "-c", "conda-forge", "--yes", "--name", envname, packages)))

  # check for errors
  if (result != 0L) {
    stop("Error ", result, " occurred installing packages into conda environment ",
         envname, call. = FALSE)
  }

  invisible(NULL)
}



install_tensorflow_virtualenv <- function(python, virtualenv, version, gpu, envname, packages, extra_packages = NULL) {

  # determine python version to use
  is_python3 <- python_version(python) >= "3.0"
  pip_version <- ifelse(is_python3, "pip3", "pip")

  # create virtualenv
  virtualenv_root <- Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
  if (!file.exists(virtualenv_root))
    dir.create(virtualenv_root, recursive = TRUE)

  # helper to construct paths to virtualenv binaries
  virtualenv_bin <- function(bin) path.expand(file.path(virtualenv_path, "bin", bin))

  # destroy and re-create virtualenv (necessary to work around tf upgrade bugs)
  virtualenv_path <- file.path(virtualenv_root, envname)
  if (dir_exists(virtualenv_path))
    unlink(virtualenv_path, recursive = TRUE)
  cat("Creating virtualenv for TensorFlow at ", virtualenv_path, "\n")
  result <- system2(virtualenv, shQuote(c(
    "--system-site-packages",
    "--python", python,
    path.expand(virtualenv_path)))
  )
  if (result != 0L)
    stop("Error ", result, " occurred creating virtualenv at ", virtualenv_path,
         call. = FALSE)

  # function to call pip within virtual env
  pip_install <- function(pkgs, message) {
    cmd <- sprintf("%ssource %s && %s install --ignore-installed --upgrade %s%s",
                   ifelse(is_osx(), "", "/bin/bash -c \""),
                   shQuote(path.expand(virtualenv_bin("activate"))),
                   shQuote(path.expand(virtualenv_bin(pip_version))),
                   paste(shQuote(pkgs), collapse = " "),
                   ifelse(is_osx(), "", "\""))
    cat(message, "...\n")
    result <- system(cmd)
    if (result != 0L)
      stop("Error ", result, " occurred installing TensorFlow", call. = FALSE)
  }

  # upgrade pip so it can find tensorflow
  pip_install("pip==9.0.3", "Upgrading pip")

  # install updated version of the wheel package
  pip_install("wheel", "Upgrading wheel")

  # upgrade setuptools so it can use wheels
  pip_install("setuptools", "Upgrading setuptools")

  # install tensorflow and related dependencies
  pkgs <- tf_pkgs(version, gpu, packages, scipy = TRUE, extra_packages = extra_packages)
  pip_install(pkgs, "Installing TensorFlow")
}


install_tensorflow_windows_system <- function(python, pip, version, gpu, packages, extra_packages = NULL) {

  # ensure pip is up to date
  cat("Preparing for installation (updating pip if necessary)\n")
  result <- system2(python, c("-m", "pip", "install", "--upgrade", "pip"))
  if (result != 0L)
    stop("Error ", result, " occurred updating pip", call. = FALSE)

  # install tensorflow and dependencies (don't install scipy b/c it requires
  # native code compilation)
  cat("Installing TensorFlow...\n")
  pkgs <- tf_pkgs(version, gpu, packages, scipy = FALSE, extra_packages = extra_packages)
  result <- system2(pip, c("install", "--upgrade --ignore-installed",
                           paste(shQuote(pkgs), collapse = " ")))
  if (result != 0L)
    stop("Error ", result, " occurred installing tensorflow package", call. = FALSE)

  cat("\nInstallation of TensorFlow complete.\n\n")
}


parse_tensorflow_version <- function(version) {

  # defaults
  ver <- list(
    version = "latest",
    gpu = FALSE,
    packages = NULL
  )

  # full url provided
  if (identical(version, "default")) {

    # default, no changes required

  # gpu version
  } else if (identical(version, "gpu")) {

    ver$gpu <- TRUE

  # gpu qualifier provided
  } else if (grepl("-gpu$", version)) {

    split <- strsplit(version, "-")[[1]]
    ver$version <- split[[1]]
    ver$gpu <-TRUE

  # full path to installer binary
  } else if (grepl("^.*\\.whl$", version)) {

    if (grepl("^http", version))
      ver$packages <- version
    else
      ver$packages <- normalizePath(version)

  # another version
  } else {

    ver$version <- version

  }

  # if it's the nightly version then set packages to nightly[-gpu]
  # (also add tensorboard since it's not included in tf-nightly)
  version <- sub("^tf-nightly$", "nightly", version)
  if (identical(version, "nightly")) {
    ver$packages <- c(sprintf("tf-nightly%s", ifelse(ver$gpu, "-gpu", "")),
                      "tensorflow-tensorboard")
  }

  # return
  ver
}

python_unix_binary <- function(bin) {
  locations <- file.path(c("/usr/bin", "/usr/local/bin", path.expand("~/.local/bin")), bin)
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


# form list of tf pkgs
tf_pkgs <- function(version, gpu, packages, scipy = TRUE, extra_packages = NULL) {
  if (is.null(packages))
    packages <- sprintf("tensorflow%s%s",
                       ifelse(gpu, "-gpu", ""),
                       ifelse(version == "latest", "", paste0("==", version)))
  c(packages, tf_extra_pkgs(scipy = scipy, extra_packages = extra_packages))
}

# additional dependencies to install (required by some features of keras)
tf_extra_pkgs <- function(scipy = TRUE, extra_packages = NULL) {
  pkgs <- c("h5py", "pyyaml",  "requests",  "Pillow")
  pkgs <- c(pkgs, extra_packages)
  if (scipy)
    c(pkgs, "scipy")
  else
    pkgs
}


virtualenv_install <- function(envname, packages) {

  # TODO: refactor to share code between this and install_tensorflow_virtualenv
  # (we added this code late in the v1.0 cycle so didn't want to do the
  # refactor then)

  # determine path to virtualenv
  virtualenv_root <- Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
  virtualenv_path <- file.path(virtualenv_root, envname)

  # helper to construct paths to virtualenv binaries
  virtualenv_bin <- function(bin) path.expand(file.path(virtualenv_path, "bin", bin))

  # determine pip version to use
  python <- virtualenv_bin("python")
  is_python3 <- python_version(python) >= "3.0"
  pip_version <- ifelse(is_python3, "pip3", "pip")

  # build and execute install command
  cmd <- sprintf("%ssource %s && %s install --ignore-installed --upgrade %s%s",
                 ifelse(is_osx(), "", "/bin/bash -c \""),
                 shQuote(path.expand(virtualenv_bin("activate"))),
                 shQuote(path.expand(virtualenv_bin(pip_version))),
                 paste(shQuote(packages), collapse = " "),
                 ifelse(is_osx(), "", "\""))
  result <- system(cmd)
  if (result != 0L)
    stop("Error ", result, " occurred installing packages", call. = FALSE)
}


windows_system_install <- function(python, packages) {

  # TODO: refactor to share code with install_tensorflow_windows_system

  # determine pip location from python binary location
  pip <- file.path(dirname(python), "Scripts", "pip.exe")

  # execute the installation
  result <- system2(pip, c("install", "--upgrade --ignore-installed",
                           paste(shQuote(packages), collapse = " ")))
  if (result != 0L)
    stop("Error ", result, " occurred installing tensorflow package", call. = FALSE)
}


