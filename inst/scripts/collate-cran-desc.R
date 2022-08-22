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
## Packages Task Views
views <- ctv::available.views()
packages_view <- tibble::tibble(task_view = NA_character_, package = NA_character_)[0,]

for(view in names(views)) invisible(
    packages_view <- views[[view]][["packagelist"]]
    |> dplyr::transmute(task_view = view, package = name)
    |> dplyr::bind_rows(packages_view)
)

## Packages Description
pkg_desc <- tools::CRAN_package_db()

invisible(
    packages_desc <- pkg_desc
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
## Bind Multiple Task Views
invisible(
    tidy_views <- packages_view
    |> dplyr::distinct()
    |> tidyr::nest(task_view = task_view)
    |> dplyr::rowwise()
    |> dplyr::mutate(task_view = task_view |> unlist() |> jsonlite::toJSON() |> as.character())
    |> dplyr::ungroup()
)

## Create Bijection: One-to-One Relationship between 'package' and 'full_name'
invisible(
    tidy_desc <- packages_desc
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
    tidy_desc <- tidy_desc
    |> dplyr::mutate(owner = github$extract$owner(full_name), repo = github$extract$repo(full_name))
    |> dplyr::mutate(full_name = dplyr::if_else(tolower(package) == tolower(repo), github$compose$slug(owner, package), full_name))
    |> dplyr::select(-owner, -repo)
)


# Consolidate Data --------------------------------------------------------
invisible(
    tidy_packages <- dplyr::left_join(tidy_desc, tidy_views, by = "package")
    |> tidyr::replace_na(list(task_view = "[]"))
)


# Teardown ----------------------------------------------------------------
ecos$overwrite_PACKAGE(tidy_packages)
