# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Helpers -----------------------------------------------------------------
extract_date <- function(x){ suppressWarnings(
    x
    |> substr(1, 10)
    |> lubridate::ymd()
    |> lubridate::format_ISO8601()
    |> tidyr::replace_na("1970-01-01")
) }


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
        cran_version = version,
        cran_date = packaged,
        title = title,
        description = description
    )
    |> dplyr::mutate(
        full_name = tolower(full_name),
        cran_date = extract_date(cran_date)
    )
    |> dplyr::distinct(package, .keep_all = TRUE)
)


# Process data ------------------------------------------------------------
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

## Format repo names to have packages names
invisible(
    tidy_packages <- tidy_packages
    |> dplyr::mutate(owner = github$extract$owner(full_name), repo = github$extract$repo(full_name))
    |> dplyr::mutate(full_name = dplyr::if_else(tolower(package) == tolower(repo), github$compose$slug(owner, package), full_name))
    |> dplyr::select(-owner, -repo)
)


# Teardown ----------------------------------------------------------------
ecos$overwrite_PACKAGE(tidy_packages)
