# examine an example configuration file provided by tensorflow
file <- system.file("examples/config/config.yml", package = "tensorflow")
file.edit(file)

# read the default configuration
tensorflow::config("default", file = file)

# read the alternate configuration: note that
# the default configuration is inherited, but
# we override the 'string' configuration here
tensorflow::config("alternate", file = file)

# override configuration values using command
# line arguments (normally, these would be
# passed in through the command line invocation
# used to start the process)
tensorflow::config("alternate",
                   file = file,
                   arguments = c("--foo=1"))
