test_that("QueryQueue works", {
    expect_s3_class(queue <- QueryQueue$new(), "R6")

    skip_localy()

    expect_type(queue$REPO, "environment")
    expect_type(queue$USER, "environment")

    expect_gte(QueryQueue$new()$REPO$size(), 0)
})
