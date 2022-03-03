# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()


# Load cached data --------------------------------------------------------
invisible(
    artifacts <- user_archive$show()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::slice_head(n=3000)
    |> dplyr::pull(artifact)
    |> user_archive$load()
)


# Profiler ----------------------------------------------------------------
profiles <- purrr::map_dfr(artifacts, ~tibble::tibble(
    id           = as.integer(.x[["id"]]),
    login        = as.character(.x[["login"]]),
    avatar_url   = as.character(.x[["avatar_url"]]),
    html_url     = as.character(.x[["html_url"]]),
    name         = as.character(.x[["name"]] %||% NA_character_),
    public_repos = as.integer(.x[["public_repos"]]),
    followers    = as.integer(.x[["followers"]]),
    following    = as.integer(.x[["following"]]),
    created_at   = lubridate::ymd_hms(.x[["created_at"]]),
    updated_at   = lubridate::ymd_hms(.x[["updated_at"]])
))

invisible(
    tidy_profiles <- profiles
    |> ge$filter$ghosts()
)


# Teardown ----------------------------------------------------------------
# repository$write_repo_desc(tidy_repo_desc)$commit()
