# Depository --------------------------------------------------------------
test_that("Depository constructor works", {
    expect_s3_class(depo <<- Depository$new(
        local_path = fs::path_temp("testthat"),
        remote_path = "s3://tidylab/github.observatory/testthat/"
    ), "R6")
})

test_that("Depository write functions work", {
    # skip_localy()
    expect_s3_class(depo$overwrite_USER(mtcars), "R6")
    expect_s3_class(depo$overwrite_REPO(mtcars), "R6")
    expect_s3_class(depo$overwrite_PACKAGE(mtcars), "R6")
    expect_s3_class(depo$overwrite_FOLLOWING(mtcars), "R6")
    expect_s3_class(depo$overwrite_SPECTATOR(mtcars), "R6")
})

test_that("Depository read functions work", {
    # skip_localy()
    expect_s3_class(depo$read_USER(), "data.frame")
    expect_s3_class(depo$read_REPO(), "data.frame")
    expect_s3_class(depo$read_PACKAGE(), "data.frame")
    expect_s3_class(depo$read_FOLLOWING(), "data.frame")
    expect_s3_class(depo$read_SPECTATOR(), "data.frame")
})
