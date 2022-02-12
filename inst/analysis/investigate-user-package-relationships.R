# Setup -------------------------------------------------------------------
# remotes::install_cran(c("recommenderlab", "arules"), quiet = TRUE)
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()


# Compose Stargazer-Package Matrix ----------------------------------------
invisible(
    repo_desc <- repository$read_repo_desc()
    |> tidyr::unnest(stargazers)
    |> dplyr::rename(stargazer_id = stargazers)
    |> dplyr::distinct()
)

(
    user_pkg_matrix <- repo_desc
    # |> dplyr::sample_n(1e3)
    |> dplyr::select(stargazer_id, repo_id)
    |> dplyr::transmute(user = stargazer_id, package = repo_id)
    |> purrr::modify(factor)
    |> as("transactions")
    |> as("itemMatrix")
    |> as("binaryRatingMatrix")
)


# Statistics --------------------------------------------------------------
a <- recommenderlab::rowCounts(user_pkg_matrix)


# Model data --------------------------------------------------------------




# Developer Zone ----------------------------------------------------------
data("MovieLense", package = "recommenderlab")
class(MovieLense)
recommenderlab::getRatingMatrix()

recommenderlab::dropNA()
