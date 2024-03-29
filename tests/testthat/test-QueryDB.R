# Tests -------------------------------------------------------------------
test_that("QueryDB constructor works", {
    fs::dir_delete(local_path)
    expect_s3_class(query_db <<- QueryDB$new(local_path, immediate = FALSE), "R6")
})


# Save and Load -----------------------------------------------------------
test_that("QueryDB save() works", {
    expect_s3_class(query_db$save(
        data = query_data,
        entity = "repo",
        type = "overview",
        id = query_data$id,
        alias = query_data$name
    ), "R6")
})

test_that("QueryDB save() works", {
    expect_s3_class(query_db$load(), "data.frame")
})


# rollback and commit -----------------------------------------------------
test_that("QueryDB commit() works", {
    expect_equal(nrow(query_db$load()), 0L)
    expect_s3_class(query_db$commit(), "R6")
    expect_equal(nrow(query_db$load()), 1L)
})

test_that("QueryDB rollback() works", {
    expect_s3_class(query_db$rollback(), "R6")
})
