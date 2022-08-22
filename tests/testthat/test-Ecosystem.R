# Ecosystem --------------------------------------------------------------
test_that("Ecosystem constructor works", {
    expect_s3_class(ecos <<- Ecosystem$new(local_path = local_path, remote_path = remote_path), "R6")
})

test_that("Ecosystem write functions work", {
    expect_s3_class(ecos$overwrite_USER(USER), "R6")
    expect_s3_class(ecos$overwrite_REPO(REPO), "R6")
    expect_s3_class(ecos$overwrite_PACKAGE(PACKAGE), "R6")
    expect_s3_class(ecos$overwrite_TASK_VIEW(TASK_VIEW), "R6")
    expect_s3_class(ecos$overwrite_DEPENDENCY(mtcars), "R6")
    expect_s3_class(ecos$overwrite_FOLLOWING(FOLLOWING), "R6")
    expect_s3_class(ecos$overwrite_SPECTATOR(mtcars), "R6")
})

test_that("Ecosystem read functions work", {
    expect_s3_class(ecos$read_USER(), "data.frame")
    expect_s3_class(ecos$read_REPO(), "data.frame")
    expect_s3_class(ecos$read_PACKAGE(), "data.frame")
    expect_s3_class(ecos$read_DEPENDENCY(), "data.frame")
    expect_s3_class(ecos$read_FOLLOWING(), "data.frame")
    expect_s3_class(ecos$read_SPECTATOR(), "data.frame")
})
