# GDrive ------------------------------------------------------------------
test_that("GDrive Repository works", {
    expect_s3_class(grepo <- GDrive$new(), "R6")
    expect_s3_class(grepo$read_USER(), "data.frame")
    expect_s3_class(grepo$read_REPO(), "data.frame")
    expect_s3_class(grepo$read_PACKAGE(), "data.frame")
    expect_s3_class(grepo$read_FOLLOWING(), "data.frame")
    expect_s3_class(grepo$read_SPECTATOR(), "data.frame")
})
