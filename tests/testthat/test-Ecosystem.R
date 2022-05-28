# Ecosystem --------------------------------------------------------------
test_that("Ecosystem constructor works", {
    expect_s3_class(eco <<- Ecosystem$new(local_path = local_path, remote_path = remote_path), "R6")
})

test_that("Ecosystem write functions work", {
    # skip_localy()
    expect_s3_class(eco$overwrite_USER(mtcars), "R6")
    expect_s3_class(eco$overwrite_REPO(mtcars), "R6")
    expect_s3_class(eco$overwrite_PACKAGE(mtcars), "R6")
    expect_s3_class(eco$overwrite_FOLLOWING(mtcars), "R6")
    expect_s3_class(eco$overwrite_SPECTATOR(mtcars), "R6")
})

test_that("Ecosystem read functions work", {
    # skip_localy()
    expect_s3_class(eco$read_USER(), "data.frame")
    expect_s3_class(eco$read_REPO(), "data.frame")
    expect_s3_class(eco$read_PACKAGE(), "data.frame")
    expect_s3_class(eco$read_FOLLOWING(), "data.frame")
    expect_s3_class(eco$read_SPECTATOR(), "data.frame")
})
