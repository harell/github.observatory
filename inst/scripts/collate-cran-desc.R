# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("gdrive_repo")) gdrive_repo <- GDrive$new()


# Download CRAN packages list ---------------------------------------------
pkg_desc <- tools::CRAN_package_db()

invisible(
    tidy_packages <- pkg_desc
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
gdrive_repo$overwrite_PACKAGE(tidy_packages)
