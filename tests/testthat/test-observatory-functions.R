# Queries -----------------------------------------------------------------
test_that("user queries work", {
    skip_localy()
    expect_type(query_by_login <- observatory$query$user$by_login(user_login), "list")
    expect_type(query_by_id <- observatory$query$user$by_id(user_id), "list")
    expect_identical(query_by_login |> select(-queried_at), query_by_id |> select(-queried_at))
})
