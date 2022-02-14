test_that("user qureis work", {
    skip("Github Queries")
    expect_type(query$user$starred("nz-stefan"), "character")
    expect_identical(query$user$by_login("nz-stefan"), query$user$by_id(5642464))
})
