# GDrive ------------------------------------------------------------------
test_that("GDrive Repository works", {
    expect_s3_class(grepo <- GDrive$new(), "R6")
    expect_s3_class(grepo$read_USER(), "data.frame")
})
