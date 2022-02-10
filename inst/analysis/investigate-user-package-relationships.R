# Setup -------------------------------------------------------------------
# remotes::install_cran("recommenderlab")
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()


# Compose Stargazer-Package Matrix ----------------------------------------
(
    repo_desc <- repository$read_repo_desc()
    |> tidyr::unnest(stargazers)
    |> dplyr::distinct()
)

