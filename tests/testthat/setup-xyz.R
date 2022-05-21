cat(banner("Unit Tests"))

local_path <- fs::path_temp("testthat")
remote_path <- "s3://tidylab/github.observatory/testthat/"
