# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()


# Compose Stargazer-Package Matrix ----------------------------------------
invisible(
    repo_desc <- repository$read_repo_desc()
    |> tidyr::unnest(stargazers_login)
    |> dplyr::transmute(user = stargazers_login, item = repo)
    |> dplyr::distinct()
)

invisible(
    user_pkg_matrix <- repo_desc
    # |> dplyr::sample_n(1e3)
    |> dplyr::select(user, item)
    |> tidyr::nest(item = item)
)

imatrix <- purrr::map(user_pkg_matrix$item, unlist)
names(imatrix) <- user_pkg_matrix$user
imatrix <- imatrix |> as("itemMatrix") |> as("binaryRatingMatrix")


# Statistics --------------------------------------------------------------
# round(recommenderlab::rowCounts(imatrix) |> table() |> prop.table() * 100)


# Evaluate models ---------------------------------------------------------
# eval_sets <- recommenderlab::evaluationScheme(
#     data = imatrix,
#     method = "bootstrap",
#     train = 0.687,
#     k = 10,
#     given = -1
# )
#
# models_to_evaluate <- list()
# models_to_evaluate[["UBCF Cosinus"]] <- list(name = "UBCF", param = list(method = "cosine"))
# models_to_evaluate[["Baseline"]] <- list(name = "RANDOM", param = list(method = "cosine"))
#
# list_results <- recommenderlab::evaluate(
#     x = eval_sets,
#     method = models_to_evaluate,
#     n = c(1, 5)
# )
#
# recommenderlab::plot(list_results)


# Model data --------------------------------------------------------------
mdl <- recommenderlab::Recommender(data = imatrix, method = "UBCF")


# Recommend packages to users ---------------------------------------------
index <- which(dplyr::pull(imatrix@data@itemsetInfo, itemsetID) %in% c("harell","nz-stefan"))

recommendations <- recommenderlab::predict(object = mdl, newdata = imatrix[index], n = 10, type = "topNList")

user_pkg_df <- dplyr::bind_cols(
    as(recommendations, "list") |> tibble::enframe("user", "item")|> tidyr::unnest(item),
    recommendations@ratings |> tibble::enframe("user", "rating") |> tidyr::unnest(rating) |> dplyr::select(rating)
)


