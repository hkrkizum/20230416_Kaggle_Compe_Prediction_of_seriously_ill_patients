# 0. load package -------------------------
library(targets)


# 1. set optins --------------------------------------------------------------
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tidyverse",
               "data.table",
               "here",
               "skimr",
               "qs"),
  format = "qs",
  seed = 54147
)

# 2. set functions ----------------------------------------------------------


# 3. define pipeline --------------------------------------------------------
list(
  tar_target(
    name = in_f_train,
    command = {
      here::here("Rawdata", "train_df.csv")
    }
  ),
  tar_target(
    name = in_f_test,
    command = {
      here::here("Rawdata", "test_df.csv")
    }
  ),
  tar_target(
    name = in_f_submitt,
    command = {
      here::here("Rawdata", "submission.csv")
    }
  ),
  tar_target(
    name = df_train,
    command = {
      data.table::fread(in_f_train)
    }
  ),
  tar_target(
    name = df_test,
    command = {
      data.table::fread(in_f_test)
    }
  ),
  tar_target(
    name = df_submitt,
    command = {
      data.table::fread(in_f_submitt)
    }
  )
)
