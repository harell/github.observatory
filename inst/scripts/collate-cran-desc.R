# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()


# Download CRAN packages list ---------------------------------------------
pkg_desc <- tools::CRAN_package_db()

invisible(
    tidy_pkg_desc <- pkg_desc
    |> standardise_col_names()
    |> dplyr::rowwise()
    |> dplyr::transmute(
        package = as.character(package),
        github_slug = dplyr::if_else(
            github$is_valid_url(bug_reports),
            github$compose$slug(owner = github$extract$owner(bug_reports), repo = package),
            github$compose$slug(owner = "cran", repo = package)
        )
    )
    |> dplyr::ungroup()
)


# Teardown ----------------------------------------------------------------
repository$write_cran_desc(tidy_pkg_desc)
repository$commit()































