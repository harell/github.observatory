# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()
if(does_not_exist("depo")) depo <- Depository$new()


# Load cached data --------------------------------------------------------
invisible(
    artifacts <- repo_archive$show()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, .keep_all = TRUE)
)


# Parse repos -------------------------------------------------------------
invisible(
    unique_repos <- artifacts$artifact
    |> purrr::map_dfr(~.x |> repo_archive$load() |> unlist())
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
        language         = as.character(language %||% ""),
        homepage         = as.character(homepage %||% ""),
        created_at       = lubridate::ymd_hms(created_at) |> observatory$standardise$date(),
        updated_at       = lubridate::ymd_hms(updated_at) |> observatory$standardise$date(),
        queried_at       = lubridate::ymd_hms(queried_at) |> observatory$standardise$date(),
        processed_at     = Sys.Date() |> observatory$standardise$date()
    )
)


# Teardown ----------------------------------------------------------------
depo$overwrite_REPO(unique_repos)
