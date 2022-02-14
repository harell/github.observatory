# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()
output_path <- usethis::proj_path("_cache", "user_user_matrix", ext = "rds")


# Compose Stargazer-Package Matrix ----------------------------------------
invisible(
    repo_desc <- repository$read_repo_desc()
    |> tidyr::unnest(stargazers_login)
    |> dplyr::transmute(user = stargazers_login, item = repo)
    |> dplyr::distinct()
    |> dplyr::mutate(
        user_id = user |> factor() |> as.integer(),
        item_id = item |> factor() |> as.integer()
    )
    ## Sample data for testing purposes
    |> dplyr::filter(item_id %in% 1:100)
    |> dplyr::mutate(
        user_id = user |> factor() |> as.integer(),
        item_id = item |> factor() |> as.integer()
    )
)

smatrix <- Matrix::sparseMatrix(
    i = repo_desc$user_id,
    j = repo_desc$item_id,
    dimnames = list(unique(repo_desc$user), unique(repo_desc$item))
)
dim(smatrix)


# Model data --------------------------------------------------------------
system.time(user_user_sim_mat <- proxyC::simil(smatrix, margin = 1, method = "jaccard", min_simil = 0.1, diag = !TRUE))
readr::write_rds(user_user_sim_mat, output_path)


# index <- which(user_dist@Dimnames[[1]] %in%  c("harell","nz-stefan")[1])
# user_dist[,index] |> table()

# Recommend users to users ------------------------------------------------


