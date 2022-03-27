# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()
if(does_not_exist("depo")) depo <- Depository$new()


# Load cached data --------------------------------------------------------
users_todate <- depo$read_USER(filter = "everything")

invisible(
    artifacts <- user_archive$show()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, .keep_all = TRUE)
)


# Parse users -------------------------------------------------------------
invisible(
    users_update <- artifacts$artifact
    |> purrr::map_dfr(~.x |> user_archive$load() |> unlist())
    |> dplyr::transmute(
        id           = as.integer(id),
        login        = as.character(login),
        avatar_url   = as.character(avatar_url),
        html_url     = as.character(html_url),
        name         = as.character(name %||% NA_character_) |> observatory$standardise$name(),
        public_repos = as.integer(public_repos),
        followers    = as.integer(followers),
        following    = as.integer(following),
        created_at   = lubridate::ymd_hms(created_at) |> observatory$standardise$date(),
        updated_at   = lubridate::ymd_hms(updated_at) |> observatory$standardise$date(),
        queried_at   = lubridate::ymd_hms(queried_at) |> observatory$standardise$date(),
        processed_at = Sys.Date() |> observatory$standardise$date()
    )
)

invisible(
    tidy_users <- users_update
    |> observatory$discard$ghosts()
    |> observatory$discard$robots()
    |> tibble::add_column(
        r_followers = NA_integer_,
        r_following = NA_integer_,
        r_contributor_count = NA_integer_,
        r_stargazer_count = NA_integer_,
        r_watcher_count = NA_integer_,
        .after = "following"
    )
)


# Consolidate data --------------------------------------------------------
(
    users <- users_todate
    |> dplyr::bind_rows(tidy_users)
    |> dplyr::arrange(id, dplyr::desc(queried_at), processed_at)
    |> dplyr::group_by(id, queried_at)
    |> dplyr::slice_head(n = 1)
    |> dplyr::ungroup()
)


# Teardown ----------------------------------------------------------------
depo$overwrite_USER(users)
