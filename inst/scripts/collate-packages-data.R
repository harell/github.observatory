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


















repo <- "dplyr"
owner <- "tidyverse"

gh$package$get_overview(owner, repo)
a <- gh$package$get_stargazers(owner, repo)


type <- "overview"
date <- Sys.Date() |> lubridate::format_ISO8601()
slug <- glue('{{type {type},repo {repo},date {date}}}')
file <- usethis::proj_path("_cache", slug, ext = "json")
dir.create(dirname(file), FALSE, TRUE)
(
    file_df <- tibble::tibble(file = file)
    dplyr::mutate()
)


decode_file_name <- function(file) return(
    file
    |> fs::path_ext_remove()
    |> basename()
    |> stringr::str_remove_all("\\{|\\}")
    |> stringr::str_split(",")
    |> purrr::pluck(1)
    |> tibble::as_tibble()
    |> tidyr::separate(value, c("key", "value"), " ")
    |> tidyr::pivot_wider(id_cols = NULL, names_from = key, values_from = value)
    |> tibble::add_column(file = as.character(file), .before = 1)
)



tidyr::separate(data.frame(file = file), col = file, sep = " ")

(
    tidy_pkg_github_stats <- repo$read_package_description_table()
    |> dplyr::filter(package %in% !!package)
    |> dplyr::mutate(
        github_owner = ge$github$extract_owner(github_url),
        github_repo = ge$github$extract_repo(github_url)
    )
    # |>
        # github_owner = github_slug |> stringr::str_split("/") |> purrr::pluck(1, 1),
    # github_repo  = github_slug |> stringr::str_split("/") |> purrr::pluck(1, 2)
)

result$package$meta <-


repo$read_package_description_table()


# Teardown ----------------------------------------------------------------
repo$commit()

