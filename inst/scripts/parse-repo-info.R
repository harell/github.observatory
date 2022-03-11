# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()
if(does_not_exist("gdrive_repo")) gdrive_repo <- GDrive$new()


# Load cached data --------------------------------------------------------
invisible(
    artifacts <- repo_archive$show()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, .keep_all = TRUE)
)


# Parse repos -------------------------------------------------------------
invisible(
    repos <- artifacts$artifact
    |> purrr::map_dfr(~.x |> repo_archive$load() |> unlist())
    |> ge$standardise$col_names()
    |> dplyr::transmute(
        id               = as.integer(id),
        full_name        = as.character(full_name) |> tolower(),
        owner_type       = as.character(owner_type),
        owner_id         = as.integer(owner_id),
        html_url         = as.character(html_url),
        created_at       = lubridate::ymd_hms(created_at) |> ge$standardise$date(),
        updated_at       = lubridate::ymd_hms(updated_at) |> ge$standardise$date(),
        stargazers_count = as.integer(stargazers_count),
        watchers_count   = as.integer(subscribers_count),
        forks_count      = as.integer(forks_count),
        language         = as.character(language %||% NA_character_),
        homepage         = as.character(homepage %||% NA_character_)
    )
)


# Teardown ----------------------------------------------------------------
gdrive_repo$overwrite_REPO(repos)
