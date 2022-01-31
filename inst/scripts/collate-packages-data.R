# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
repo <- Repository$new()


# Package Description Table -----------------------------------------------
pkg_desc <- tools::CRAN_package_db()

(
    tidy_pkg_desc <- pkg_desc
    |> standardise_col_names()
    |> dplyr::transmute(
        package = as.character(package),
        url = as.character(bug_reports)
    )
    |> dplyr::rowwise()
    |> dplyr::mutate(
        github_url   = dplyr::if_else(ge$github$is_valid_url(url), url, ge$github$compose_cran_slug(package)),
        github_slug  = ge$github$parse_slug(github_url),
        github_owner = github_slug |> stringr::str_split("/") |> purrr::pluck(1, 1),
        github_repo  = github_slug |> stringr::str_split("/") |> purrr::pluck(1, 2)
    )
    |> dplyr::ungroup()
)

repo$write_package_description_table(tidy_pkg_desc)
repo$commit()
