# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()
if(does_not_exist("gdrive_repo")) gdrive_repo <- GDrive$new()


# Load cached data --------------------------------------------------------
invisible(
    artifacts <- user_archive$show()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, .keep_all = TRUE)
    |> dplyr::pull(artifact)
    |> user_archive$load()
)


# Profiler ----------------------------------------------------------------
profiles <- purrr::map_dfr(artifacts, purrr::flatten_dfr)

invisible(
    tidy_profiles <- profiles
    |> dplyr::transmute(
        id           = as.integer(id),
        login        = as.character(login),
        avatar_url   = as.character(avatar_url),
        html_url     = as.character(html_url),
        name         = as.character(name %||% NA_character_),
        public_repos = as.integer(public_repos),
        followers    = as.integer(followers),
        following    = as.integer(following),
        created_at   = lubridate::ymd_hms(created_at),
        updated_at   = lubridate::ymd_hms(updated_at)
    )
    |> ge$filter$ghosts()
)


# Teardown ----------------------------------------------------------------
gdrive_repo$overwrite_USER(tidy_profiles)
