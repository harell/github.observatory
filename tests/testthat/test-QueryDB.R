# Setup -------------------------------------------------------------------
path <- fs::file_temp("QueryDB")


# Tests -------------------------------------------------------------------
test_that("QueryDB constructor works", {
  expect_s3_class(QueryDB$new(path, immediate = FALSE), "R6")
})
