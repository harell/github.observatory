test_that("Agent constructor works", {
    expect_s3_class(ecos <<- Ecosystem$new(local_path = local_path, remote_path = remote_path), "R6")
    expect_s3_class(agent <<- Agent$new(ecos = ecos), "R6")
})
