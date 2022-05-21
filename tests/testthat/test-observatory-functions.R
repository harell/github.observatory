# Queries -----------------------------------------------------------------
test_that("user queries work", {
    skip_localy()
    expect_type(query_by_login <- observatory$query$user$by_login("nz-stefan"), "list")
    expect_type(query_by_id <- observatory$query$user$by_id(5642464), "list")
    expect_identical(query_by_login |> select(-queried_at), query_by_id |> select(-queried_at))
})
