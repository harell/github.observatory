# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
result <- new.env()


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
        github_url   = dplyr::if_else(ge$github$is_valid_url(url), url, as.character(stringr::str_glue("https://github.com/cran/{package}/issues", package = package))),
        github_slug  = ge$github$parse_slug(github_url),
        github_owner = github_slug |> stringr::str_split("/") |> purrr::pluck(1, 1),
        github_repo  = github_slug |> stringr::str_split("/") |> purrr::pluck(1, 2)
    )
    |> dplyr::ungroup()
)
