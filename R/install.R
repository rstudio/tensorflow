#' Install TensorFlow and its dependencies
#'
#' @description
#'
#' This function installs TensorFlow into a persistant virtual environment.
#' Beginning with reticulate version 1.41, in most circumstances, creating a
#' persistent virtual environment by calling the `install_tensorflow()` function
#' is no longer necessary, because reticulate automatically will resolve a
#' python environment that satisfies all python requirements declared with
#' `reticulate::py_require()`.
#'
#' New code is recommended to call `py_require_tensorflow()` at the start of an
#' R session to declare tensorflow requirements via `py_requore()`. In a future
#' package update this will by default be done in tensorflow's `.onLoad` hook.
#'
#' The `py_require_tensorflow()` function that can dynamically modify the python
#' requirements to enable usage of a GPU if one is available and usable by the R
#' session.
#'
#' The Python packages registered with `py_require()` by
#' `py_require_tensorflow()`:
#'
#' - On Linux: if a GPU is detected: `"tensorflow[and-cuda]"`, otherwise,
#' `"tensorflow-cpu"`.
#'
#' - On macOS: `"tensorflow"` is declared. The default package is not capable
#' of using the GPU. To enable TensorFlow usage of the GPU, call
#' `reticulate::py_require("tensorflow-metal")` before reticulate has
#' initialized Python. Note that not all features of TensorFlow work correctly
#' if `tensorflow-metal` is installed. There are known issues with random number
#' generators like `tf$random$stateless_uniform()`, likely others as well.
#'
#' - On Windows: `"tensorflow"` and `"numpy<2"` are declared. Note that
#' TensorFlow GPU usage on Windows is no longer supported (Since TensorFlow
#' 2.10). To use a GPU on windows, use TensorFlow via WSL. `"numpy<2"` is
#' declared because at the time of this publishing, the pre-built binaries of
#' `tensorflow` for Windows are not compatible with `numpy>2`.
#'
#' `install_tensorflow()` creates a new virtual environment containing the
#' `tensorflow` python package and it's direct dependencies. For creating a
#' virtual environment with more complete set packages that includes additional
#' optional dependencies, use [`keras3::install_keras()`].
#'
#' @section Custom Installation: `install_tensorflow()` or
#'   `keras3::install_keras()` isn't required to use tensorflow with the
#'   package. If you manually configure a python environment with the required
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
#'   keras3::layer_random_rotation(x, .5)  # Error:
#'   # No registered 'RngReadAndSkip' OpKernel for 'GPU' devices
#'   # Pin the operation to the CPU to avoid the error
#'   with(tf$device("CPU"), keras3::layer_random_rotation(x, .5) ) # No Error
#'   ````
#'
#' @section Additional Packages:
#'
#'   If you wish to add additional PyPI packages to your Keras / TensorFlow
#'   environment you can either specify the packages in the `extra_packages`
#'   argument of `install_tensorflow()` or `install_keras()`, or alternatively
#'   install them into an existing environment using the
#'   [reticulate::py_install()] function. Note that `install_keras()` includes a
#'   set of additional python packages by default, see `?keras3::install_keras`
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
#' @param python_version Select the Python that will be used to create the
#'   virtualenv. Pass a string with version constraints like `"3.8"`, or
#'   `">=3.9,<=3.11"` or a file path to a `python` executable like
#'   `"/path/to/bin/python3"`. The supplied value is passed on to
#'   `reticulate::virtualenv_starter()`. Note that the Python version must be
#'   compatible with the requested TensorFlow version, documented here:
#'   <https://www.tensorflow.org/install/pip#system-requirements>
#'
#' @param conda_python_version Passed to conda (only applicable if `method =
#'   "conda"`)
#'
#' @param cuda logical `TRUE` or `FALSE`. If `install_tensorflow()` detects the
#'   platform is Linux, an Nvidia GPU is available, and the TensorFlow version
#'   is 2.14 (the default), it will install also install the required CUDA
#'   libraries through pip.
#'
#' @param metal Whether to install `tensorflow-metal` pip package on Arm Macs.
#'   This enables tensorflow to use the GPU. Pass a string to install a specific
#'   version like `"tensorflow-metal==0.7.*`.
#'
#' @param pip_ignore_installed Whether pip should ignore installed python
#'   packages and reinstall all already installed python packages.
#'
#' @param new_env If `TRUE`, any existing Python virtual environment and/or
#'   conda environment specified by `envname` is deleted first.
#'
#' @param use_gpu Only consulted if on Linux. It has no effect on macOS or
#'   Windows. If `NA`, the R package will attempt to detect a GPU. If a GPU is
#'   detected, then this is taken as `TRUE`, `FALSE` otherwise. If `TRUE`, then
#'   `tensorflow[and-cuda]` is declared, otherwise, `tensorflow-cpu` is declared
#'   via `py_require()`.
#'
#' @param ... other arguments passed to [`reticulate::conda_install()`] or
#'   [`reticulate::virtualenv_install()`], depending on the `method` used.
#'
#' @seealso
#' -  [`keras3::install_keras()`]
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
         cuda = NULL,
         # tensorflow-metal broken w/ TF v2.16 and default keras:
         # https://github.com/tensorflow/tensorflow/issues/63854#issuecomment-2011725507
         metal = FALSE, #is_mac_arm64(),
         pip_ignore_installed = FALSE,
         new_env = identical(envname, "r-tensorflow"),
         python_version = NULL) {

  method <- match.arg(method)

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

    if(grepl("gpu", as.character(version), ignore.case = TRUE))
      warning("Caution: TensorFlow 2.10 was the last TensorFlow release that supported GPU on native-Windows. Starting with TensorFlow 2.11, you will need to install TensorFlow in WSL2, or install a CPU-only version of TensorFlow.",
              if(identical(.Platform$GUI, "RStudio")) " For a guide on how to use RStudio with WSL2, see https://support.posit.co/hc/en-us/articles/360049776974-Using-RStudio-Server-in-Windows-WSL2")

  }


  can_use_gpu <- FALSE
  if (is.null(cuda)) {

    has_nvidia_gpu <- function() {
      lspci_listed <- tryCatch(
        {
          lspci <- system("lspci", intern = TRUE, ignore.stderr = TRUE)
          any(grepl("nvidia", lspci, ignore.case = TRUE))
        },
        warning = function(w) FALSE,
        error = function(e) FALSE
      )

      if (lspci_listed)
        return(TRUE)

      # lspci doens't list GPUs on WSL Linux, but nvidia-smi does.
      nvidia_smi_listed <- tryCatch(
        system("nvidia-smi -L", intern = TRUE),
        warning = function(w) character(),
        error = function(e) character()
      )
      if (isTRUE(any(grepl("^GPU [0-9]: ", nvidia_smi_listed))))
        return(TRUE)
      FALSE
    }

    can_use_gpu <-
      is_linux() &&
      (version %in% c("default", "release") ||
         isTRUE(extract_numeric_version(version) >= "2.14")) &&
      has_nvidia_gpu()

    cuda <- can_use_gpu

  }

  tf_package_spec <- parse_tensorflow_version(version)

  if(isTRUE(cuda) && !grepl("^.*\\.whl$", tf_package_spec)) {
    tf_package_spec <- sub("([^=<>!]*)(.*)", "\\1[and-cuda]\\2",
                           tf_package_spec)
  }

  packages <- unique(c(
    tf_package_spec,
    as.character(extra_packages)
  ))

  if (isTRUE(extract_numeric_version(version) <= "2.17"))
    packages <- c("numpy<2", packages)

  if (isTRUE(metal)) repeat {
    tf_ver <- extract_numeric_version(tf_package_spec)
    if(is.na(tf_ver))
      break

    if(tf_ver >= "2.14")
      metal <- "tensorflow-metal>1.0.1"
    else if (tf_ver >= "2.13")
      metal <- "tensorflow-metal>=1.0.1"
    else if (tf_ver >= "2.12")
      metal <- "tensorflow-metal==0.8.*"
    else
      # https://pypi.org/project/tensorflow-metal/
      metal <- "tensorflow-metal"

    break
  }

  python_version <- python_version %||% conda_python_version
  if(method %in% c("auto", "virtualenv") &&
     is.null(python_version)) {

    # virtualenv_starter() picks the most recent version available, but older
    # versions of tensorflow typically don't work with the latest Python
    # release. In general, we're better off picking the oldest Python version available
    # that works with the current release of tensorflow.
    # TF 2.13 is compatible with Python <=3.11,>=3.8

    # prefer 3.10 if we have it, otherwise, find 3.9
    available <- reticulate::virtualenv_starter(version = ">=3.10,<=3.12", all = TRUE)
    if(!nrow(available))
      available <- reticulate::virtualenv_starter(version = ">=3.9,<=3.12", all = TRUE)
    # pick the smallest minor version, ignoring patchlevel
    if(nrow(available)) {
      python_version <- min(available$version[, 1:2])
      ## tf 2.16 supports python 3.12
    #   if(python_version >= "3.12" && isTRUE(grepl("default", version)))
    #     stop(
    #       "The current release version of TensorFlow requires a Python version between 3.9 and 3.11. ",
    #       "Python versions >=3.12 are not supported. Please use ",
    #       "`reticulate::install_python('3.10:latest')` or manually install an older version of Python from www.python.org/downloads"
    #       )
    }
  }

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

  py_install_args <- list(
    packages       = packages,
    envname        = envname,
    method         = method,
    conda          = conda,
    python_version = python_version,
    pip            = TRUE,
    pip_ignore_installed = pip_ignore_installed,
    ...
  )

  # now ignored, superseded by `cuda`
  py_install_args$configure_cudnn <- NULL

  do.call(reticulate::py_install, py_install_args)

  if(is_string(metal)) {
    py_install_args$packages <- metal
    tryCatch(do.call(reticulate::py_install, py_install_args),
             error = function(e) {
               message(e)
               message("No suitable version of the 'tensorflow-metal' found. You can ",
                       "use TensorFlow with CPU only, or install a previous release ",
                       "of tensorflow that has GPU support on ARM macs with ",
                       "`tensorflow::install_tensorflow(version = '2.13')`")
             })
  }

  if(cuda && is_linux()) {
    configure_nvidia_symlinks(envname = envname)
    configure_ptxas_symlink(envname = envname)
  }

  cat("\nInstallation complete.\n\n")

  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()

  invisible(NULL)
}


has_gpu <- function() {

  has_nvidia_gpu <- function() {

    # oop <- options(warn = 2L)
    # on.exit(options(oop), add = TRUE)

    lspci_listed <- tryCatch(
      {
        lspci <- system("lspci", intern = TRUE, ignore.stderr = TRUE)
        any(grepl("nvidia", lspci, ignore.case = TRUE))
      },
      warning = function(w) FALSE,
      error = function(e) FALSE
    )

    if (lspci_listed)
      return(TRUE)

    # lspci doens't list GPUs on WSL Linux, but nvidia-smi does.
    nvidia_smi_listed <- tryCatch(
      system("nvidia-smi -L", intern = TRUE, ignore.stderr = TRUE),
      warning = function(w) character(),
      error = function(e) character()
    )
    if (isTRUE(any(grepl("^GPU [0-9]: ", nvidia_smi_listed))))
      return(TRUE)

    FALSE
  }

  is_linux() && has_nvidia_gpu()

}


get_py_requirements <- function(use_gpu = NA) {
  python_version <- NULL
  packages <- "tensorflow"

  if(is_linux()) {
    if(is.na(use_gpu))
      use_gpu <- has_gpu()

    if(use_gpu) {
      packages <- "tensorflow[and-cuda]"
    } else {
      packages <- "tensorflow-cpu"
    }

  } else if (is_mac_arm64()) {

    use_gpu <- FALSE
    # https://pypi.org/project/tensorflow-metal/#history
    # https://pypi.org/project/tensorflow-macos/#history
    if (use_gpu) {
      packages <- c("tensorflow", "tensorflow-metal")
    }

  } else if (is_windows()) {

    packages <- c(packages, "numpy<2")

  }

  list(packages = packages, python_version = python_version)
}

default_version <- numeric_version("2.20")

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

  if(nchar(gsub("[^.]", "", version)) < 2L)
    version <- paste0(version, ".*")

  if(!grepl("[><=]", version))
    version <- sprintf("==%s", version)

  paste0(package, version)
}


extract_numeric_version <- function(x, strict = FALSE) {
  x <- as.character(x)
  x <- gsub("-", ".", x, fixed = TRUE)
  x <- gsub("[^0-9.]", "", x, perl = TRUE)
  x <- gsub("\\.+", ".", x)
  x <- sub("^\\.+", "", x)
  x <- sub("\\.+$", "", x)
  numeric_version(x, strict = strict)
}


python_module_dir <- function(python, module, stderr = TRUE) {

  force(python)
  py_cmd <- sprintf("import %s; print(%1$s.__file__)", module)

  module_file <- suppressWarnings(system2(
    python, c("-c", shQuote(py_cmd)),
    stdout = TRUE, stderr = stderr))

  if (!is.null(attr(module_file, "status")) ||
      !is_string(module_file) ||
      !file.exists(module_file))
    return()

  dirname(module_file)

}


configure_nvidia_symlinks <- function(envname) {
  if(!is_linux()) return()
  python <- reticulate::virtualenv_python(envname)

  nvidia_path <- python_module_dir(python, "nvidia")
  if(is.null(nvidia_path)) return()
  # "~/.virtualenvs/r-tensorflow/lib/python3.9/site-packages/nvidia/cudnn"

  nvidia_sos <- Sys.glob(paste0(nvidia_path, "/*/lib/*.so*"))
  if(!length(nvidia_sos)) return()
  # [1] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cublas/lib/libcublas.so.12"
  # [2] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cublas/lib/libcublasLt.so.12"
  # [3] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cublas/lib/libnvblas.so.12"
  # [4] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cuda_cupti/lib/libcheckpoint.so"
  # [5] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cuda_cupti/lib/libcupti.so.12"
  # [6] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cuda_cupti/lib/libnvperf_host.so"
  # [7] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cuda_cupti/lib/libnvperf_target.so"
  # [8] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cuda_cupti/lib/libpcsamplingutil.so"
  # [9] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cuda_nvrtc/lib/libnvrtc-builtins.so.12.3"
  # [10] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cuda_nvrtc/lib/libnvrtc.so.12"
  # [11] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cuda_runtime/lib/libcudart.so.12"
  # [12] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cudnn/lib/libcudnn.so.8"
  # [13] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cudnn/lib/libcudnn_adv_infer.so.8"
  # [14] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cudnn/lib/libcudnn_adv_train.so.8"
  # [15] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cudnn/lib/libcudnn_cnn_infer.so.8"
  # [16] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cudnn/lib/libcudnn_cnn_train.so.8"
  # [17] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cudnn/lib/libcudnn_ops_infer.so.8"
  # [18] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cudnn/lib/libcudnn_ops_train.so.8"
  # [19] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cufft/lib/libcufft.so.11"
  # [20] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cufft/lib/libcufftw.so.11"
  # [21] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/curand/lib/libcurand.so.10"
  # [22] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cusolver/lib/libcusolver.so.11"
  # [23] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cusolver/lib/libcusolverMg.so.11"
  # [24] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/cusparse/lib/libcusparse.so.12"
  # [25] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/nccl/lib/libnccl.so.2"
  # [26] "~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/nvidia/nvjitlink/lib/libnvJitLink.so.12"
  ## we don't need *all* of these, but as of 2.16, in addition to cudnn, we need
  ## libcusparse.so.12 libnvJitLink.so.12 libcusolver.so.11 libcufft.so.11 libcublasLt.so.12 libcublas.so.12
  ## We symlink all of them to (try to be) future proof

  # "~/.virtualenvs/r-tensorflow/lib/python3.9/site-packages/tensorflow"
  tf_lib_path <- python_module_dir(python, "tensorflow", stderr = FALSE)

  from <- sub("^.*/site-packages/", "../", nvidia_sos)
  to <- file.path(tf_lib_path, basename(nvidia_sos))
  writeLines("creating symlinks:")
  writeLines(paste("-", shQuote(to), "->", shQuote(from)))
  # creating symlinks:
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcublas.so.12' -> '../nvidia/cublas/lib/libcublas.so.12'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcublasLt.so.12' -> '../nvidia/cublas/lib/libcublasLt.so.12'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libnvblas.so.12' -> '../nvidia/cublas/lib/libnvblas.so.12'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcheckpoint.so' -> '../nvidia/cuda_cupti/lib/libcheckpoint.so'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcupti.so.12' -> '../nvidia/cuda_cupti/lib/libcupti.so.12'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libnvperf_host.so' -> '../nvidia/cuda_cupti/lib/libnvperf_host.so'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libnvperf_target.so' -> '../nvidia/cuda_cupti/lib/libnvperf_target.so'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libpcsamplingutil.so' -> '../nvidia/cuda_cupti/lib/libpcsamplingutil.so'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libnvrtc-builtins.so.12.3' -> '../nvidia/cuda_nvrtc/lib/libnvrtc-builtins.so.12.3'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libnvrtc.so.12' -> '../nvidia/cuda_nvrtc/lib/libnvrtc.so.12'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcudart.so.12' -> '../nvidia/cuda_runtime/lib/libcudart.so.12'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcudnn.so.8' -> '../nvidia/cudnn/lib/libcudnn.so.8'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcudnn_adv_infer.so.8' -> '../nvidia/cudnn/lib/libcudnn_adv_infer.so.8'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcudnn_adv_train.so.8' -> '../nvidia/cudnn/lib/libcudnn_adv_train.so.8'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcudnn_cnn_infer.so.8' -> '../nvidia/cudnn/lib/libcudnn_cnn_infer.so.8'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcudnn_cnn_train.so.8' -> '../nvidia/cudnn/lib/libcudnn_cnn_train.so.8'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcudnn_ops_infer.so.8' -> '../nvidia/cudnn/lib/libcudnn_ops_infer.so.8'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcudnn_ops_train.so.8' -> '../nvidia/cudnn/lib/libcudnn_ops_train.so.8'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcufft.so.11' -> '../nvidia/cufft/lib/libcufft.so.11'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcufftw.so.11' -> '../nvidia/cufft/lib/libcufftw.so.11'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcurand.so.10' -> '../nvidia/curand/lib/libcurand.so.10'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcusolver.so.11' -> '../nvidia/cusolver/lib/libcusolver.so.11'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcusolverMg.so.11' -> '../nvidia/cusolver/lib/libcusolverMg.so.11'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libcusparse.so.12' -> '../nvidia/cusparse/lib/libcusparse.so.12'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libnccl.so.2' -> '../nvidia/nccl/lib/libnccl.so.2'
  # - '~/.virtualenvs/r-tensorflow/lib/python3.10/site-packages/tensorflow/libnvJitLink.so.12' -> '../nvidia/nvjitlink/lib/libnvJitLink.so.12'
  # - '~/.virtualenvs/r-tensorflow/bin/ptxas' -> '../../lib/python3.10/site-packages/nvidia/cuda_nvcc/bin/ptxas'
  file.symlink(from = from, to = to)

}

configure_ptxas_symlink <- function(envname = "r-tensorflow") {
  if(!is_linux()) return()
  python <- reticulate::virtualenv_python(envname)

  nvcc_path <- python_module_dir(python, "nvidia.cuda_nvcc")
  if(is.null(nvcc_path)) return()

  # configure a link so that ptxas can be found on the PATH
  # when the venv is activated.
  # https://discuss.tensorflow.org/t/tensorflow-version-2-16-just-released/23140/6#resolving-the-ptxas-issue-3
  nvcc_bins <- Sys.glob(file.path(nvcc_path, "bin/*"))
  if(!length(nvcc_bins)) return()
  # "~/.virtualenvs/r-tensorflow/lib/python3.9/site-packages/nvidia/cuda_nvcc/bin/ptxas"

  to <- file.path(dirname(python), basename(nvcc_bins))
  # "~/.virtualenvs/r-tensorflow/bin/ptxas"

  # fs::path_rel(nvcc_bins, to)
  from <- sub(dirname(dirname(python)), "../..", nvcc_bins)
  # "../../lib/python3.9/site-packages/nvidia/cuda_nvcc/bin/ptxas"

  # writeLines("creating symlinks:")
  writeLines(paste("-", shQuote(to), "->", shQuote(from)))
  # '~/.virtualenvs/r-tensorflow/bin/ptxas' -> '../../lib/python3.9/site-packages/nvidia/cuda_nvcc/bin/ptxas'

  file.symlink(from = from, to = to)

}
