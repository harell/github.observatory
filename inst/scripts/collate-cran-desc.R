# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("depo_repo")) depo_repo <- Depository$new()


# Download CRAN packages list ---------------------------------------------
pkg_desc <- tools::CRAN_package_db()

invisible(
    packages <- pkg_desc
    |> observatory$standardise$col_names()
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

## Create Bijection: One-to-One Relationship between 'package' and 'full_name'
invisible(
    tidy_packages <- packages
    |> dplyr::add_count(full_name, sort = TRUE)
    |> dplyr::mutate(n_char = nchar(package))
    |> dplyr::arrange(dplyr::desc(n), n_char)
    |> dplyr::group_by(full_name)
    |> dplyr::mutate(index = seq(1, dplyr::n()))
    |> dplyr::ungroup()
    |> dplyr::mutate(full_name = dplyr::if_else(index == 1, full_name, github$compose$slug(owner = "cran", repo = package)))
    |> dplyr::select(-n, -n_char, -index)
    |> dplyr::arrange(package)
)


# Teardown ----------------------------------------------------------------
depo_repo$overwrite_PACKAGE(tidy_packages)
