# Queries -----------------------------------------------------------------
test_that("user queries work", {
    skip("Github Queries")
    expect_type(observatory$query$user$starred("nz-stefan"), "character")
    expect_identical(
        observatory$query$user$by_login("nz-stefan"),
        observatory$query$user$by_id(5642464)
    )
})
