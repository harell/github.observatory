test_that("QueryQueue works", {
    expect_s3_class(queue <- QueryQueue$new(), "R6")
    expect_type(queue$REPO, "environment")
    expect_type(queue$USER, "environment")

    # expect_s3_class(depo$read_REPO(), "data.frame")
    # expect_s3_class(depo$read_PACKAGE(), "data.frame")
    # expect_s3_class(depo$read_FOLLOWING(), "data.frame")
    # expect_s3_class(depo$read_SPECTATOR(), "data.frame")
})
