test_that("QueryQueue works", {
    expect_s3_class(queue <- QueryQueue$new(path = local_path), "R6")

    expect_type(queue$REPO, "environment")
    expect_type(queue$USER, "environment")

    expect_gte(QueryQueue$new()$REPO$size(), 0)
    expect_gte(QueryQueue$new()$USER$size(), 0)
})
