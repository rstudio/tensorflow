# examine an example configuration file provided by tensorflow
file <- system.file("examples/config/config.yml", package = "tensorflow")
cat(readLines(file), sep = "\n")

# read the default configuration
config <- tensorflow::config("default", file = file)
str(config)

# read the alternate configuration: note that
# the default configuration is inherited, but
# we override the 'string' configuration here
config <- tensorflow::config("alternate", file = file)
str(config)

# override configuration values using command
# line arguments (normally, these would be
# passed in through the command line invocation
# used to start the process)
config <- tensorflow::config(
  "alternate",
  file = file,
  arguments = c("--foo=1")
)
str(config)
