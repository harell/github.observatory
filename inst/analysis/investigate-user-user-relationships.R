# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()
output_path <- usethis::proj_path("_cache", "user_user_sim_mat", ext = "rds")


# Compose Stargazer-Package Matrix ----------------------------------------
invisible(
    repo_desc <- repository$read_repo_desc()
    |> tidyr::unnest(stargazers_login)
    |> dplyr::transmute(user = stargazers_login, item = repo)
    |> dplyr::distinct()
    |> dplyr::add_count(user, name = "stargazed")
    |> dplyr::filter(stargazed >= 10)
    |> dplyr::mutate(
        user_id = user |> factor() |> as.integer(),
        item_id = item |> factor() |> as.integer()
    )
    ## Sample data for testing purposes
    # |> dplyr::filter(item_id %in% 1:100)
    # |> dplyr::mutate(
    #     user_id = user |> factor() |> as.integer(),
    #     item_id = item |> factor() |> as.integer()
    # )
)

smatrix <- Matrix::sparseMatrix(
    i = repo_desc$user_id,
    j = repo_desc$item_id,
    dimnames = list(unique(repo_desc$user), unique(repo_desc$item))
)
dim(smatrix)


# Statistics --------------------------------------------------------------
(
    repo_desc
    |> dplyr::distinct(user, stargazed)
    |> dplyr::pull(stargazed)
    |> table()
    |> cumsum() / 147963
)


# Model data --------------------------------------------------------------
system.time(user_user_sim_mat <- proxyC::simil(smatrix, margin = 1, method = "jaccard", min_simil = 0.1, diag = FALSE))
readr::write_rds(user_user_sim_mat, output_path)


# Recommend users to users ------------------------------------------------
index <- which(user_user_sim_mat@Dimnames[[1]] %in%  c("harell","nz-stefan")[1])

(
    user_user_sim_mat[,index]
    |> sort(TRUE)
    |> tibble::enframe("user", "rating")
    |> dplyr::slice(-1)
    |> dplyr::slice_head(n = 10)
)


