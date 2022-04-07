test_that("QueryQueue works", {
    expect_s3_class(queue <- QueryQueue$new(), "R6")
    expect_type(queue$REPO, "environment")
    expect_type(queue$USER, "environment")

})

test_that("QueryQueue generates queue object", {
    expect_gte(QueryQueue$new()$REPO$size(), 0)
})
