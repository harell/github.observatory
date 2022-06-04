test_that("Agent constructor works", {
    expect_s3_class(ecos <<- Ecosystem$new(local_path = local_path, remote_path = remote_path), "R6")
    expect_s3_class(agent <<- Agent$new(ecos = ecos), "R6")
})

test_that("recommend_repos_to_user works", {
    n <- 2
    expect_s3_class(recommend_packages <- agent$recommend_repos_to_user(user_id = user_login, n = n, method = "random"), "data.frame")
    expect_equal(nrow(recommend_packages), n)
})

test_that("query_repos_graph works", {
    existing_repo_id <- 318095552
    nonexisting_repo_id <- 1

    expect_s3_class(
        agent$query_repos_graph(repo_id = existing_repo_id, degrees = 1, method = "depends"),
        "data.frame"
    )
})


