# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_db")) user_db <- UserQueryDB$new()
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Load cached data --------------------------------------------------------
invisible(
    queries <- user_db$load()
    |> dplyr::filter(type %in% "following")
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, .keep_all = TRUE)
)


# Parse following ---------------------------------------------------------
invisible(
    spectators <- ecos$read_SPECTATOR()
    |> dplyr::filter(user_role %in% "stargazer")
    |> dplyr::distinct(user_id)
    |> dplyr::pull(user_id)
)


invisible(
    all_followers <- queries
    |> dplyr::rename(user_id = id)
    |> dplyr::rowwise()
    |> dplyr::mutate(obj = data |> jsonlite::fromJSON() |> list())
    |> tidyr::unnest(obj, keep_empty = TRUE, ptype = list(login = "character", id = "integer"))
    |> dplyr::ungroup()
    |> dplyr::transmute(from = as.integer(user_id), to = as.integer(id))
)

invisible(
    r_followers <- all_followers
    |> dplyr::filter(to %in% spectators, from %in% spectators)
)


# Teardown ----------------------------------------------------------------
ecos$overwrite_FOLLOWING(r_followers)
