if (!requireNamespace("yaml", quietly = TRUE))
  return()

# generate a dummy configuration
config <- list(
  default = list(
    number   = 42,
    string   = "hello",
    list     = list(42, "hello", list()),
    override = "old"
  )
)

# write configuration to file
yaml <- yaml::as.yaml(config)
file <- file.path(tempdir(), "config.yml")
writeLines(yaml, con = file)
on.exit(unlink(file))

# invoke 'tensorflow::config()' with the generated
# config file, and override a value with command line
# arguments
tensorflow::config(
  config = "default",
  file = file,
  arguments = "--override=new"
)
