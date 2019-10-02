## TensorFlow 2.0.0 (CRAN)

- Upgraded default installed version to 2.0.0.

- Tensorboard log directory path fixes (#360).

- Allow for `v1` and `v2` compat (#358).

- `install_tensorflow` now does not installs `tfprobability`, `tfhub` and other
 related packages.


## TensorFlow 1.14.1 (CRAN)

- Upgraded default installed version to 1.14.0

- Refactored the `install_tensorflow` code delegating to `reticulate` (#333, #341): We completely delegate to installation to `reticulate::py_install`, the main difference is that now the default environment name to install is `r-reticulate` and not `r-tensorflow`.


## TensorFlow 1.13.1 (CRAN)

- added option to silence TF CPP info output

- `tf_gpu_configured` function to check if GPU was correctly


