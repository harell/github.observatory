test_that("get user starred repos", {
  expect_type(gh$user$get_starred("nz-stefan"), "character")
})
