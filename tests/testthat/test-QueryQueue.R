test_that("QueryQueue works", {
    expect_s3_class(depo <- QueryQueue$new(), "R6")
    # expect_s3_class(depo$read_USER(), "data.frame")
    # expect_s3_class(depo$read_REPO(), "data.frame")
    # expect_s3_class(depo$read_PACKAGE(), "data.frame")
    # expect_s3_class(depo$read_FOLLOWING(), "data.frame")
    # expect_s3_class(depo$read_SPECTATOR(), "data.frame")
})
