# tensorflow 2.15.0

- `install_tensorflow()` installs TensorFlow v2.15 by default
- Added compatibility with the latest release of reticulate (> 1.34).

# tensorflow 2.14.0

- `install_tensorflow()` changes:
  - Installs TensorFlow v2.14 by default.
  - Now will automatically install the required Nvidia CUDA runtime as a pip
    package if on Linux and a GPU is detected. You can opt-out by passing
    `install_tensorflow(cuda = FALSE)`. Aside from the Nvidia driver, no other
    pre-existing Nvidia CUDA packages are now necessary.
  - The `configure_cudnn` argument is now superseded by the new argument `cuda`.
  - New argument `metal`, for specifying if the `tensorflow-metal` pip package
    should be installed on Arm Macs. Defaults to `TRUE` on Arm Macs.

- Fixed an issue where `as.array()` and other methods might fail if the tensor
  had conversion disabled via `r_to_py()` or `convert = FALSE`.
- Fixed an issue where Ops group generic dispatch would error one object was a tensor
  and the other was a non-tensor Python object (e.g., a numpy array).
- Removed long deprecated symbols: 
    `install_tensorflow_extras()`, `tfe_enable_eager_execution()`
- tfestimator generics `train()` and `train_and_evaluate()` now warn about 
  their deprecation status when called. The will be removed in a future release. 

# tensorflow 2.13.0

- `install_tensorflow()` changes:
     - Installs TensorFlow v2.13 by default now.
     - The `envname` argument new default is `"r-tensorflow"`. This means that
       unless the `envname` argument supplied, `install_tensorflow()` will now
       install into the `"r-tensorflow"` environment, bootstrapping a venv of
       that name if necessary.
     - gains a `new_env` argument. If `TRUE`, any existing environment
       specified by `envname` is deleted and created anew. Defaults to `TRUE` if
       envname is `"r-tensorflow"`, `FALSE` otherwise.
     - If running on Linux, now detects if NVIDIA GPUs on Linux are installed,
       and if so, and installs cuDNN (via pip), configures symlinks for tensorflow
       to find cuDNN, and emits additional instructions for how to install the necessary CUDA
       drivers to enable GPU usage. Set new arg `configure_cudnn = FALSE` to disable.
     - `pip_ignore_installed` default is now `FALSE` again.
     - On Arm Macs (M1/M2), the default tensorflow package is once again installed,
       rather than `tensorflow-macos` and `tensorflow-metal`.

- New `pillar:type_sum()` method for Tensors, giving a
  more informative printout of Tensors in R tracebacks and tibbles.

# tensorflow 2.11.0

- `install_tensorflow()` now installs TF v2.11 by default.

- `as_tensor()` now coerces bare R atomic vectors to R arrays before conversion.
  As a consequence, by default, R atomic double vectors now coerce to
  'float64' dtype tensors instead of 'float32'.

- `shape()` gains the ability to accept vectors of length > 1 in `...`,
  including other `tf.TensorShape`s. Shapes are automatically flattened.

- Fixed an issue where a `ListWrapper` object of trackable keras layers
  (e.g., as part of a keras model) would not convert to an R list.

# tensorflow 2.9.0

- Generic method updates:
  - New methods:
      all(), any(), sum(), prod(), min(), max(), mean(), range(),
      cbind(), rbind(), t(), aperm(), sort(),
      as.vector(), as.character(), as.raster(),
      is.infinite(), is.finite(), is.nan()
  - `^` will now invoke `tf.square()` or `tf.sqrt()` directly when appropriate
  - `|`, `&`, and `!` now cast arguments to 'bool' dtype.
  - `print()` now shows 1d shapes without a trailing commas.
  - `str()` method for tensors now returns only a single compact line;
    `str()` on a list of tensors now does something sensible.

- `install_tensorflow()` now install TensorFlow 2.9 by default.

- `install_tensorflow()` no longer requires conda on Windows, now works in a regular venv.

- Comparing two partially-defined `TensorShape` now returns TRUE if each dimension matches.
  e.g.: `shape(NA, 4) == shape(NA, 4)` now returns TRUE, previously FALSE.

- Tensors with dtype 'string' now convert to R character vectors by methods
  `as.array()` and `as.matrix()`. (previously they converted to python.builtin.bytes,
  or an R list of python.builtin.bytes objects)

- `as_tensor()`:
  - atomic R integer vectors now convert to 'int32', not 'int64'
  - casting between integer and floating dtypes is now done via
   `tf$dtypes$saturate_cast()` instead of `tf$cast()`.
  - `shape` argument now accepts a tensor.
  - fixed issue where expanding a scalar tensor to an nd-array with
    `shape` provided as a tensor would raise an error.

- `tf.SparseTensor` objects now inherit from `"tensorflow.tensor"`.

# tensorflow 2.8.0

- Updated default Tensorflow version installed by `install_tensorflow()` to 2.8.

- `as_tensor()` gains a `shape` argument, can be used to fill or reshape tensors.
  Scalars can be recycled to a tensor of arbitrary `shape`, otherwise
  supplied objects are reshaped using row-major (C-style) semantics.

- `install_tensorflow()` now provides experimental support for Arm Macs,
  with the following restrictions:
    - "conda" is the only supported installation method.
    - requests for non-default or older tensorflow versions are not supported.

- `install_tensorflow()` default conda_python_version changes from 3.7 to NULL.

- `tf.TensorShape()`'s gain `format()` and `print()` S3 methods.

- `[` method for slicing tensors now accepts `NA` as a synonym for a missing or `NULL` spec.
  For example `x[NA:3]` is now valid, equivalent to `x[:3]` in Python.

# tensorflow 2.7.0

- Default Tensorflow version installed by `install_tensorflow()` updated to 2.7

- Breaking changes:
  - `shape()` now returns a `tf.TensorShape()` object
    (Previously an R-list of `NULL`s or integers).
  - `[` method for `tf.TensorShape()` objects also now returns a `tf.TensorShape()`.
    Use `[[`, `as.numeric`, `as.integer`, and/or `as.list` to convert to R objects.
  - `length()` method for `tensorflow.tensor` now returns `NA_integer_` for
    tensors with not fully defined shapes. (previously a zero length integer vector).
  - `dim()` method for `tensorflow.tensor` now returns an R integer vector
    with `NA` for dimensions that are undefined.
    (previously an R list with `NULL` for undefined dimension)

- New S3 generics for `tf.TensorShape()`'s:
  `c`, `length`, `[<-`, `[[<-`, `merge`, `==`, `!=`, `as_tensor()`,
  `as.list`, `as.integer`, `as.numeric`, `as.double`, `py_str`
  (joining previous generics `[` and `[[`).
  See `?shape` for extended examples.

- Ops S3 generics for `tensorflow.tensor`s that take two arguments now
  automatically cast a supplied non-tensor to the dtype of the supplied tensor
  that triggered the S3 dispatch. Casting is done via `as_tensor()`.
  e.g., this now works:
    ```
    as_tensor(5L) - 2     # now returns tf.Tensor(3, shape=(), dtype=int32)
    ```
  previously it would raise an error:
    ```
    TypeError: `x` and `y` must have the same dtype, got tf.int32 != tf.float32
    ```
  Generics that now do autocasting:
    +, -, *, /, %/%, %%, ^, &, |, ==, !=, <, <=, >, >=

- `install_tensorflow()`: new argument with default `pip_ignore_installed = TRUE`.
  This ensures that all Tensorflow dependencies like Numpy are installed by pip
  rather than conda.

- A message with the Tensorflow version is now shown when the
  python module is loaded, e.g: "Loaded Tensorflow version 2.6.0"

# tensorflow 2.6.0

- Updated default Tensorflow version to 2.6.

- Changed default in `tf_function()` to `autograph=TRUE`.

- Added S3 generic `as_tensor()`.

- tfautograph added to Imports

- jsonlite removed from Imports, tfestimators removed from Suggests

- Refactored `install_tensorflow()`.
  - Potentially breaking change: numeric versions supplied without a patchlevel now automatically pull the latest patch release.
    (e.g. `install_tensorflow(version="2.4")` will install `"2.4.2"`. Previously it would install "2.4.0")

- Removed "Config/reticulate" declaration from DESCRIPTION.
  - Setting `RETICULATE_AUTOCONFIGURE=FALSE` environment variable when using non-default tensorflow installations (e.g., 'tensorflow-cpu') no longer required.
  - Users will have to call `install_tensorflow()` for automatic installation.

- Refactored automated tests to closer match the default installation procedure
  and compute environment of most user.

- Expanded CI test coverage to include R devel, oldrel and 3.6.

- Fixed an issue where extra packages with version constraints like
  `install_tensorflow(extra_packages = "Pillow<8.3")` were not quoted properly.

- Fixed an issue where valid tensor-like objects supplied to
  `log(x, base)`, `cospi()`, `tanpi()`, and `sinpi()` would raise an error.


# tensorflow 2.5.0

- Updated default Tensorflow version to 2.5.
- Added support for additional arguments in `tf_function()` (e.g., `jit_compile`)
- Added support for `expm1` S3 generic.
- `tfe_enable_eager_execution` is deprecated. Eager mode has been the default since TF version 2.0.
- Improved error message in `tf_config()` on unsuccessful installation.

# tensorflow 2.4.0

- Fixed error with `use_session_with_seed` (#428)
- Added a new `set_random_seed` function that makes more sense for TensorFlow >= 2.0 (#442)
- Updated the default version of TensorFlow to 2.4 as well as the default Python to 3.7 (#454)

# TensorFlow 2.2.0 (CRAN)

- Bugfix with `all_dims` (#398)

- Indexing for TensorShape & `py_to_r` conversion (#379, #388)

# TensorFlow 2.0.0 (CRAN)

- Upgraded default installed version to 2.0.0.

- Tensorboard log directory path fixes (#360).

- Allow for `v1` and `v2` compat (#358).

- `install_tensorflow` now does not installs `tfprobability`, `tfhub` and other
 related packages.

# TensorFlow 1.14.1 (CRAN)

- Upgraded default installed version to 1.14.0

- Refactored the `install_tensorflow` code delegating to `reticulate` (#333, #341): We completely delegate to installation to `reticulate::py_install`, the main difference is that now the default environment name to install is `r-reticulate` and not `r-tensorflow`.

# TensorFlow 1.13.1 (CRAN)

- added option to silence TF CPP info output

- `tf_gpu_configured` function to check if GPU was correctly
