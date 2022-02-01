test_that("get user starred repos", {
  expect_type(query$user$starred("nz-stefan"), "character")
})
