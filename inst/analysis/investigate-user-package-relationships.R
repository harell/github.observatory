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

invisible(
    user_pkg_matrix <- repo_desc
    # |> dplyr::sample_n(1e3)
    |> dplyr::select(stargazer_id, repo_id)
    |> dplyr::transmute(user = stargazer_id, package = repo_id)
    |> tidyr::nest(data = package)
)

imatrix <- purrr::map(user_pkg_matrix$data, unlist)
names(imatrix) <- user_pkg_matrix$user
imatrix <- imatrix |> as("itemMatrix") |> as("binaryRatingMatrix")


# Statistics --------------------------------------------------------------
plot(density(recommenderlab::rowCounts(imatrix), from = 1, to = 30))


# Evaluate models ---------------------------------------------------------
eval_sets <- recommenderlab::evaluationScheme(
    data = imatrix,
    method = "split",
    train = 0.7,
    given = -1
)

models_to_evaluate <- list()
models_to_evaluate[["UBCF Cosinus"]] <- list(name = "UBCF", param = list(method = "cosine"))
models_to_evaluate[["Baseline"]] <- list(name = "RANDOM", param = list(method = "cosine"))

list_results <- recommenderlab::evaluate(
    x = eval_sets,
    method = models_to_evaluate,
    n = c(1, 5)
)


# Model data --------------------------------------------------------------
mdl <- recommenderlab::Recommender(data = imatrix, method = "UBCF")



# Recommend packages to users ---------------------------------------------
pre <- recommenderlab::predict(object = mdl, newdata = imatrix[1], n = 10)



# Developer Zone ----------------------------------------------------------
# data("MovieLense", package = "recommenderlab")
# class(MovieLense)
# recommenderlab::getRatingMatrix()
#
# recommenderlab::dropNA()
