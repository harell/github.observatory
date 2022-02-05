# predicates --------------------------------------------------------------
test_that("predicates work", {
    urls <- c("https://github.com/harell/github.explorer", "Tidyverse/dplyr")

    expect_identical(github$is_valid_url(urls), c(TRUE, FALSE))
})


# parsers -----------------------------------------------------------------
test_that("extract functions work", {
    urls <- c("https://github.com/harell/github.explorer", "Tidyverse/dplyr")

    expect_identical(github$extract$slug(urls), c("harell/github.explorer", "Tidyverse/dplyr"))
    expect_identical(github$extract$owner(urls), c("harell", "Tidyverse"))
    expect_identical(github$extract$repo(urls), c("github.explorer", "dplyr"))
})


# compose -----------------------------------------------------------------
test_that("compose functions work", {
    urls <- c("https://github.com/harell/github.explorer", "Tidyverse/dplyr")
    owners <- github$extract$owner(urls)
    repos <- github$extract$repo(urls)
    slugs <- github$extract$slug(urls)

    expect_identical(github$compose$slug(owners, repos), slugs)
})


