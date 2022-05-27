# Ecosystem --------------------------------------------------------------
test_that("Ecosystem constructor works", {
    expect_s3_class(depo <<- Ecosystem$new(local_path = local_path, remote_path = remote_path), "R6")
})

test_that("Ecosystem write functions work", {
    # skip_localy()
    expect_s3_class(depo$overwrite_USER(mtcars), "R6")
    expect_s3_class(depo$overwrite_REPO(mtcars), "R6")
    expect_s3_class(depo$overwrite_PACKAGE(mtcars), "R6")
    expect_s3_class(depo$overwrite_FOLLOWING(mtcars), "R6")
    expect_s3_class(depo$overwrite_SPECTATOR(mtcars), "R6")
})

test_that("Ecosystem read functions work", {
    # skip_localy()
    expect_s3_class(depo$read_USER(), "data.frame")
    expect_s3_class(depo$read_REPO(), "data.frame")
    expect_s3_class(depo$read_PACKAGE(), "data.frame")
    expect_s3_class(depo$read_FOLLOWING(), "data.frame")
    expect_s3_class(depo$read_SPECTATOR(), "data.frame")
})
