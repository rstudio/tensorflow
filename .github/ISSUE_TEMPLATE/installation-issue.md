---
name: Installation Issue
about: Trouble installing Tensorflow
title: Installation Issue
labels: 'Installation'
assignees: ''
---

Many installation issues are resolved by running the following in a **fresh R session** (you can restart R in Rstudio with Ctrl+Shift+F10) :
```R
# install the development version of packages, in case the
# issue is already fixed but not on CRAN yet.
install.packages("remotes")
remotes::install_github(sprintf("rstudio/%s", c("reticulate", "tensorflow", "keras")))
reticulate::miniconda_uninstall() # start with a blank slate
reticulate::install_miniconda()
keras::install_keras()
```

Test to see if installation was successful.
```R
tensorflow::as_tensor("Hello World")
```

If the above snippet succeeded and you saw something like `tf.Tensor(b'Hello World', shape=(), dtype=string)`, then :tada:, you've successfully installed Tensorflow.

If the above installation failed, please gather some diagnostic info:
```R
reticulate::py_config()
tensorflow::tf_config()
reticulate::import("tensorflow")
reticulate::py_last_error()
sessionInfo()
```

Please copy and paste the FULL OUTPUT of running all three snippets, and be sure to enclose the output lines with three backticks (```) for monospace formatting.
