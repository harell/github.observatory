print(bannerCommenter::banner("Unit Tests", numLines = 2, maxChar = 40))

local_path <- fs::path_temp("testthat")
remote_path <- "s3://tidylab/github.observatory/testthat/"
