# Setup -------------------------------------------------------------------
path <- fs::file_temp("QueryDB")
data <- list(
    id = 280924484,
    name = "clintools",
    owner = list(login = "lilleoel", id = 68481897),
    stargazers_count = 1
)


# Tests -------------------------------------------------------------------
test_that("QueryDB constructor works", {
    expect_s3_class(query_db <<- QueryDB$new(path, immediate = FALSE), "R6")
})


# Save and Load -----------------------------------------------------------
test_that("QueryDB save() works", {
    expect_s3_class(query_db$save(
        data = data,
        entity = "repo",
        type = "overview",
        id = data$id
    ), "R6")
})

test_that("QueryDB save() works", {
    expect_s3_class(query_db$load(), "data.frame")
})


# rollback and commit -----------------------------------------------------
test_that("QueryDB commit() works", {
    expect_equal(nrow(query_db$load()), 0L)
    # expect_s3_class(query_db$commit(), "R6")
    # expect_equal(nrow(query_db$load()), 1L)
})


# Teardown ----------------------------------------------------------------
unlink(path, T, T)
query_db <<- NULL
