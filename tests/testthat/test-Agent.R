test_that("Agent constructor works", {
    expect_s3_class(agent <<- Agent$new(ecos = Ecosystem$new()), "R6")
})
