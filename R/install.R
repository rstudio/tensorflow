#' Install TensorFlow and its dependencies
#'
#' `install_tensorflow()` installs just the tensorflow python package and it's
#' direct dependencies. For a more complete installation that includes
#' additional optional dependencies, use [`keras::install_keras()`].
#'
#' @details You may be prompted to download and install miniconda if reticulate
#'   did not find a non-system installation of python. Miniconda is the
#'   recommended installation method for most users, as it ensures that the R
#'   python installation is isolated from other python installations. All python
#'   packages will by default be installed into a self-contained conda or venv
#'   environment named "r-reticulate". Note that "conda" is the only supported
#'   method on M1 Mac.
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
#' @section Apple Silicon: Beginning with Tensorflow version 2.13, the default
#'   tensorflow package now works on Apple Silicon. See
#'   \url{https://developer.apple.com/metal/tensorflow-plugin/} for instructions
#'   on how to install older versions of Tensorflow on macOS. Please note that
#'   not all operations are supported on Arm Mac GPUs. You can work around the
#'   missing operations by pinning operations to CPU. For example:
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
#'   version must be compatible with the requested Tensorflow version,
#'   documented here:
#'   <https://www.tensorflow.org/install/pip#system-requirements>
#'
#' @param configure_cudnn If `install_tensorflow()` detects the platform is
#'   Linux, an Nvidia GPU is available, and the TensorFlow version is 2.13 (the
#'   default), it will install also install the pip package
#'   "nvidia-cudnn-cu11==8.6.*", symlink the cudnn ".so" files so they can be
#'   found by tensorflow, and emit instructions for how to install Nvidia
#'   drivers to enable GPU usage.
#'
#' @param pip_ignore_installed Whether pip should ignore installed python
#'   packages and reinstall all already installed python packages. This defaults
#'   to `TRUE`, to ensure that TensorFlow dependencies like NumPy are compatible
#'   with the prebuilt TensorFlow binaries.
#'
#' @param new_env If `TRUE`, any existing Python virtual environment and/or
#'   conda environment specified by `envname` is deleted first.
#'
#' @param ... other arguments passed to [`reticulate::conda_install()`] or
#'   [`reticulate::virtualenv_install()`], depending on the `method` used.
#'
#' @seealso
#' -  [`keras::install_keras()`]
#' -  <https://tensorflow.rstudio.com/reference/tensorflow/install_tensorflow>
#'
#' @export
install_tensorflow <-
function(method = c("auto", "virtualenv", "conda"),
         conda = "auto",
         version = "default",
         envname = "r-tensorflow",
         extra_packages = NULL,
         restart_session = TRUE,
         conda_python_version = NULL,
         ...,
         configure_cudnn = NULL,
         pip_ignore_installed = FALSE,
         new_env = identical(envname, "r-tensorflow"),
         python_version = NULL) {

  method <- match.arg(method)

  if(is_mac_arm64()) {
    if(!as.character(version) %in% c("default", "release") &&
       !isTRUE(tryCatch(numeric_version(version) >= "2.13.0",
                       error = function(e) NULL)))
      stop("Only tensorflow>=2.13 supported on Arm Macs.")
  }

  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop("Unable to install TensorFlow on this platform.",
         "Binary installation is only available for 64-bit platforms.")
  }

  # some special handling for windows
  if (is_windows()) {

    # avoid DLL in use errors
    if (py_available()) {
      stop("You should call install_tensorflow()/install_keras() only in a fresh ",
           "R session that has not yet initialized Keras and TensorFlow (this is ",
           "to avoid DLL in use errors during installation)")
    }
  }

  packages <- unique(c(
    parse_tensorflow_version(version),
    as.character(extra_packages)
  ))

  has_gpu <- FALSE
  if (is.null(configure_cudnn)) {
    configure_cudnn <- FALSE
    if (is_linux() &&
        (version %in% c("default", "2.13") || grepl("^2\\.13", version)) &&
        !grepl("cpu", version)) {
      configure_cudnn <- has_gpu <- tryCatch(
        as.logical(length(system("lspci | grep -i nvidia", intern = TRUE))),
        warning = function(w) FALSE) # warning emitted by system for non-0 exit status
    }
  }

  if(configure_cudnn)
    packages <- c(packages, "nvidia-cudnn-cu11==8.6.*")

  if (isTRUE(new_env)) {

    if (method %in% c("auto", "virtualenv") &&
        reticulate::virtualenv_exists(envname))
      reticulate::virtualenv_remove(envname = envname, confirm = FALSE)

    if (method %in% c("auto", "conda")) {
      if (!is.null(tryCatch(conda_python(envname, conda = conda),
                            error = function(e) NULL)))
        reticulate::conda_remove(envname, conda = conda)
    }

  }


  python_version <- python_version %||% conda_python_version
  if(method %in% c("auto", "virtualenv") &&
     is.null(python_version)) {

    # virtualenv_starter() picks the most recent version available, but older
    # versions of tensorflow typically don't work with the latest Python
    # release. In general, we're better off picking the oldest Python version available
    # that works with the current release of tensorflow.
    # TF 2.13 is compatible with Python <=3.11,>=3.8

    available <- reticulate::virtualenv_starter(version = ">=3.8", all = TRUE)
    # pick the smallest minor version, ignoring patchlevel
    if(nrow(available))
      python_version <- min(available$version[, 1:2])
  }

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


  if(!file.exists(python <- virtualenv_python(envname))) {
    # only configure if venv, not conda
    configure_cudnn <- FALSE
  }

  if(isTRUE(configure_cudnn)) {

    python <-  "/home/tomasz/.virtualenvs/r-tensorflow/bin/python"
    # "~/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/nvidia/cudnn"
    cudnn_path <- get_cudnn_path(python)

    # [1] "~/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/nvidia/cudnn/lib/libcudnn_adv_infer.so.8"
    # [2] "~/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/nvidia/cudnn/lib/libcudnn_adv_train.so.8"
    # [3] "~/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/nvidia/cudnn/lib/libcudnn_cnn_infer.so.8"
    # [4] "~/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/nvidia/cudnn/lib/libcudnn_cnn_train.so.8"
    # [5] "~/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/nvidia/cudnn/lib/libcudnn_ops_infer.so.8"
    # [6] "~/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/nvidia/cudnn/lib/libcudnn_ops_train.so.8"
    # [7] "~/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/nvidia/cudnn/lib/libcudnn.so.8"
    cudnn_sos <- Sys.glob(paste0(cudnn_path, "/lib/*.so*"))

    # "/home/tomasz/.virtualenvs/r-tensorflow/lib/python3.8/site-packages/tensorflow/__init__.py"
    tf_lib_path <- system2(python, c("-c", shQuote("import tensorflow as tf; print(tf.__file__)")),
                       stderr = FALSE, stdout = TRUE)
    tf_lib_path <- dirname(tf_lib_path)

    from <- sub("^.*/site-packages/", "../", cudnn_sos)
    to <- file.path(tf_lib_path, basename(cudnn_sos))
    writeLines("creating symlinks:")
    writeLines(paste("-", shQuote(to), "->", shQuote(from)))
    file.symlink(from = from, to = to)


    message("- To install the necessary drivers to enable GPU usage, ", appendLF = FALSE)
    if (is_ubuntu()) {
      is_wsl <- tryCatch(as.logical(length(system("uname -r | grep -i microsoft", intern = TRUE))),
                         warning = function(w) FALSE)
      keyring_url <- if(is_wsl)
        "https://developer.download.nvidia.com/compute/cuda/repos/wsl-ubuntu/x86_64/cuda-keyring_1.0-1_all.deb"
      else {
        version_id <- system(". /etc/os-release && echo $VERSION_ID", intern = TRUE)
        version_id <- sub(".", "", version_id, fixed = TRUE)
        if(!version_id %in% c("1804", "2004", "2204")) {
          warning("Unsupported Ubuntu version for CUDA 11.8. Only LTS version 18.04, 20.04 and 22.04 are supported")
          version_id <- "2204"
        }
        sprintf("https://developer.download.nvidia.com/compute/cuda/repos/ubuntu%s/x86_64/cuda-keyring_1.0-1_all.deb", version_id)
      }
      instructions <- paste0(c(
          "run the following terminal commands:",
          paste("wget", keyring_url),
          "sudo dpkg -i cuda-keyring_1.1-1_all.deb",
          "sudo apt-get update",
          "sudo apt-get -y install cuda-toolkit-11-8"
        ), collapse = "\n" )
    } else {
      instructions <- c(
          "follow the instructions for your Linux platform and install 'cuda-toolkit-11-8':\n",
          "  https://developer.nvidia.com/cuda-11-8-0-download-archive?target_os=Linux&target_arch=x86_64")
    }
    message(instructions)
  }


  cat("\nInstallation complete.\n\n")

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)
}


get_cudnn_path <- function(python = py_discover_config()$python) {

  # For TF 2.13, this assumes that someone already has cudn 11-8 installed,
  # e.g., on ubuntu:
  # sudo apt install cuda-toolkit-11-8
  # also, that `python -m pip install 'nvidia-cudnn-cu11==8.6.*'`

  force(python)
  cudnn_module_path <- suppressWarnings(system2(
    python, c("-c", shQuote("import nvidia.cudnn;print(nvidia.cudnn.__file__)")),
    stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(cudnn_module_path, "status")) ||
      !is_string(cudnn_module_path) ||
      !file.exists(cudnn_module_path))
    return()

  dirname(cudnn_module_path)

}


default_version <- numeric_version("2.13")

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
