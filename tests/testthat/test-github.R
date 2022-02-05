# parsers -----------------------------------------------------------------
test_that("extract functions work", {
    urls <- c("https://github.com/harell/github.explorer", "Tidyverse/dplyr")

    expect_identical(github$extract$slug(urls[1]), "harell/github.explorer")
    expect_identical(github$extract$slug(urls), c("harell/github.explorer", "Tidyverse/dplyr"))


    # expect_identical(github$extract$owner(urls[1]), "harell")
    # expect_identical(github$extract$owner(urls), c("harell", "dplyr"))
})
