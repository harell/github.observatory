# Setup -------------------------------------------------------------------
path <- fs::file_temp("QueryDB")
repo_query <- list(
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
        data = repo_query,
        entity = "repo",
        type = "overview",
        id = repo_query$id
    ), "R6")
})

