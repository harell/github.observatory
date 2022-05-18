# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_db")) repo_db <- RepoQueryDB$new()
if(does_not_exist("depo")) depo <- Depository$new()


# Load cached data --------------------------------------------------------
invisible(
    queries <- repo_db$load()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, .keep_all = TRUE)
)


# Parse repos -------------------------------------------------------------
invisible(
    unique_repos <- queries
    |> dplyr::rename(repo_id = id)
    |> dplyr::rowwise()
    |> dplyr::pull(data)
    |> purrr::map_dfr(~ .x |> jsonlite::fromJSON() |> unlist())
    |> observatory$standardise$col_names()
    |> dplyr::transmute(
        id               = as.integer(id),
        package          = as.character(package),
        full_name        = as.character(full_name) |> tolower(),
        owner_type       = as.character(owner_type),
        owner_id         = as.integer(owner_id),
        html_url         = as.character(html_url),
        stargazers_count = as.integer(stargazers_count),
        watchers_count   = as.integer(subscribers_count),
        forks_count      = as.integer(forks_count),
        language         = dplyr::if_else(is.na(language) | is.null(language), "", language),
        homepage         = dplyr::if_else(is.na(homepage) | is.null(homepage), "", homepage),
        created_at       = lubridate::ymd_hms(created_at) |> observatory$standardise$date(),
        updated_at       = lubridate::ymd_hms(updated_at) |> observatory$standardise$date(),
        queried_at       = lubridate::ymd_hms(queried_at) |> observatory$standardise$date(),
        processed_at     = Sys.Date() |> observatory$standardise$date()
    )
)


# Teardown ----------------------------------------------------------------
depo$overwrite_REPO(unique_repos)
