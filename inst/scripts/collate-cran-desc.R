# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- InterimRepository$new()


# Download CRAN packages list ---------------------------------------------
pkg_desc <- tools::CRAN_package_db()

invisible(
    tidy_pkg_desc <- pkg_desc
    |> ge$standardise$col_names()
    |> dplyr::transmute(
        package = as.character(package),
        github_slug = dplyr::if_else(
            github$is_valid_url(bug_reports),
            github$compose$slug(owner = github$extract$owner(bug_reports), repo = package),
            github$compose$slug(owner = "cran", repo = package)
        )
    )
    |> dplyr::distinct(package, .keep_all = TRUE)
)


# Teardown ----------------------------------------------------------------
repository$write_cran_desc(tidy_pkg_desc)
repository$commit()

