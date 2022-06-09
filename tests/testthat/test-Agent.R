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
    repo_id <- 318095552 # R6P
    expect_s3_class(
        agent$query_repos_graph(repo_id = repo_id, degrees = 2, method = "depends"),
        "data.frame"
    )

    repo_id <- 19521307 # R6
    expect_s3_class(
        agent$query_repos_graph(repo_id = repo_id, degrees = 2, method = "reverse depends"),
        "data.frame"
    )
})

test_that("query_users_graph works", {
    # repo_id <- 7226303 # harell
    # expect_s3_class(
    #     agent$query_users_graph(user_id = user_id, degrees = 2, method = "followers"),
    #     "data.frame"
    # )

    repo_id <- 4196 # hadley
    expect_s3_class(
        agent$query_users_graph(user_id = user_id, degrees = 2, method = "following"),
        "data.frame"
    )
})


