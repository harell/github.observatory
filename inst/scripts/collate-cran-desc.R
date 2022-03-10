# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("intr_repo")) intr_repo <- InterimRepository$new()


# Download CRAN packages list ---------------------------------------------
pkg_desc <- tools::CRAN_package_db()

invisible(
    tidy_pkg_desc <- pkg_desc
    |> ge$standardise$col_names()
    |> dplyr::transmute(
        package = as.character(package),
        full_name = dplyr::if_else(
            github$is_valid_url(bug_reports),
            github$extract$full_name(bug_reports),
            github$compose$slug(owner = "cran", repo = package)
        ),
        full_name = tolower(full_name)
    )
    |> dplyr::distinct(package, .keep_all = TRUE)
)


# Teardown ----------------------------------------------------------------
intr_repo$write_cran_desc(tidy_pkg_desc)
intr_repo$commit()

