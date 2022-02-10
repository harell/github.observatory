# Setup -------------------------------------------------------------------
# remotes::install_cran("recommenderlab")
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()


# Compose Stargazer-Package Matrix ----------------------------------------
invisible(
    repo_desc <- repository$read_repo_desc()
    |> tidyr::unnest(stargazers)
    |> dplyr::rename(stargazer_id = stargazers)
    |> dplyr::distinct()
)

r_verse <- Matrix::sparseMatrix(repo_desc$stargazer_id, repo_desc$repo_id)


# Developer Zone ----------------------------------------------------------
data("MovieLense", package = "recommenderlab")
class(MovieLense)
recommenderlab::getRatingMatrix()

recommenderlab::dropNA()
