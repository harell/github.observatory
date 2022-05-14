# Setup -------------------------------------------------------------------
path <- fs::file_temp("QueryDB")
db_path <- fs::path(path, "db", ext = "csv")
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

    expect_true(fs::file_exists(db_path))
})

