test_that("Agent constructor works", {
    expect_s3_class(ecos <<- Ecosystem$new(local_path = local_path, remote_path = remote_path), "R6")
    expect_s3_class(agent <<- Agent$new(ecos = ecos), "R6")
})


# Packages Recommendation -------------------------------------------------
test_that("recommend_packages_to_user works", {
    expect_s3_class(agent$recommend_packages_to_user(user_id = user_login), "data.frame")
})

