# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
repo <- Repository$new()


# Description -------------------------------------------------------------
pkg_desc <- tools::CRAN_package_db()

(
    tidy_pkg_desc <- pkg_desc
    |> standardise_col_names()
    |> dplyr::rowwise()
    |> dplyr::transmute(
        package = as.character(package),
        github_url = dplyr::if_else(ge$github$is_valid_url(bug_reports), ge$github$extract_root(bug_reports), ge$github$compose_cran_slug(package))
    )
    |> dplyr::ungroup()
)

repo$write_package_description_table(tidy_pkg_desc)


# Github Stats ------------------------------------------------------------
package <- "dplyr"


(
    tidy_pkg_github_stats <- repo$read_package_description_table()
    |> dplyr::filter(package %in% !!package)
    # |>
        # github_owner = github_slug |> stringr::str_split("/") |> purrr::pluck(1, 1),
    # github_repo  = github_slug |> stringr::str_split("/") |> purrr::pluck(1, 2)
)

result$package$meta <- gh::gh(glue("/repos/{owner}/{repo}"))


repo$read_package_description_table()


# Teardown ----------------------------------------------------------------
repo$commit()

