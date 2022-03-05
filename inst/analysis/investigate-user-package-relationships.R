# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- InterimRepository$new()


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


# Model data --------------------------------------------------------------
mdl <- recommenderlab::Recommender(data = imatrix, method = "UBCF")


# Recommend packages to users ---------------------------------------------
index <- which(dplyr::pull(imatrix@data@itemsetInfo, itemsetID) %in% c("harell","nz-stefan"))
system.time(recommendations <- recommenderlab::predict(object = mdl, newdata = imatrix[index], n = 100, type = "topNList"))
withr::with_seed(
    1928,
    user_pkg_df <- dplyr::bind_cols(
        as(recommendations, "list") |> tibble::enframe("user", "item")|> tidyr::unnest(item),
        recommendations@ratings |> tibble::enframe("user", "rating") |> tidyr::unnest(rating) |> dplyr::select(rating)
    )
    |> purrr::modify(rating, round, 3)
    |> dplyr::group_by(user, rating)
    |> dplyr::sample_n(size = dplyr::n())
    |> dplyr::ungroup()
    |> dplyr::arrange(user, -rating)
    |> dplyr::group_by(user)
    |> dplyr::slice_head(n = 10)
    |> dplyr::ungroup()
)



