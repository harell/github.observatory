# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_db")) user_db <- UserQueryDB$new()


# Load cached data --------------------------------------------------------
invisible(
    queries <- user_db$load()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, .keep_all = TRUE)
)


# Parse users -------------------------------------------------------------
invisible(
    users_info <- queries
    |> dplyr::rename(user_id = id)
    |> dplyr::rowwise()
    |> dplyr::pull(data)
    |> purrr::map_dfr(~ .x |> jsonlite::fromJSON() |> unlist())
    |> observatory$standardise$col_names()
    |> dplyr::transmute(
        id           = as.integer(id),
        login        = as.character(login),
        avatar_url   = as.character(avatar_url),
        html_url     = as.character(html_url),
        name         = dplyr::if_else(is.na(name) | is.null(name), "", observatory$standardise$name(name)),
        public_repos = as.integer(public_repos),
        followers    = as.integer(followers),
        following    = as.integer(following),
        created_at   = lubridate::ymd_hms(created_at) |> observatory$standardise$date(),
        updated_at   = lubridate::ymd_hms(updated_at) |> observatory$standardise$date(),
        queried_at   = lubridate::ymd_hms(queried_at) |> observatory$standardise$date(),
        processed_at = observatory$standardise$date(Sys.Date())
    )
    |> observatory$discard$ghosts()
    |> observatory$discard$robots()
)

invisible(
    users_extra_info <- users_info
    |> tibble::add_column(
        r_followers = NA_integer_,
        r_following = NA_integer_,
        r_contributor_count = NA_integer_,
        r_stargazer_count = NA_integer_,
        r_watcher_count = NA_integer_,
        .after = "following"
    )
)


# Teardown ----------------------------------------------------------------
depo$overwrite_USER(users_extra_info)
