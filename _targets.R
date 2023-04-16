library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tidyverse",
               "readxl",
               "here",
               "skimr",
               "qs"),
  format = "qs",
  seed = 54147
)

# Set target-specific options such as packages:
# tar_option_set(packages = "utils") # nolint

# End this file with a list of target objects.
list(
  tar_target(data, data.frame(x = sample.int(100), y = sample.int(100))),
  tar_target(summary, summ(data)) # Call your custom functions as needed.
)
