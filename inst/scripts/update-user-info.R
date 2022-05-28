# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_db")) user_db <- UserQueryDB$new()
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Load data ---------------------------------------------------------------
FOLLOWING <- ecos$read_FOLLOWING()
SPECTATOR <- ecos$read_SPECTATOR()


# Summarise R statistics --------------------------------------------------
## Calculate following and followers
(
    fellowship <- FOLLOWING
    |> dplyr::add_count(from, name = "r_following")
    |> dplyr::add_count(to, name = "r_followers")
)

(
    followers <- fellowship
    |> dplyr::transmute(id = to, r_followers = r_followers)
    |> dplyr::distinct()
    |> dplyr::arrange(id)
)

(
    following <- fellowship
    |> dplyr::transmute(id = from, r_following = r_following)
    |> dplyr::distinct()
    |> dplyr::arrange(id)
)

## Calculate repo relationships
(
    relationships <- SPECTATOR
    |> dplyr::add_count(user_id, user_role)
    |> dplyr::select(-repo_id)
    |> dplyr::distinct()
    |> tidyr::pivot_wider(id_cols = "user_id", names_from = "user_role", values_from = "n", values_fill = 0)
    |> dplyr::rename_with(~paste0("r_", .x, "_count"), .cols = -user_id)
    |> dplyr::rename(id = user_id)
)


# Update users -------------------------------------------------------------
(
    users <- ecos$read_USER()
    |> dplyr::select(-dplyr::starts_with("r_"))
    |> dplyr::left_join(followers, by = "id")
    |> dplyr::left_join(following, by = "id")
    |> dplyr::left_join(relationships, by = "id")
    |> purrr::modify_at(dplyr::vars(dplyr::starts_with("r_")), tidyr::replace_na, replace = 0)
    |> dplyr::relocate(dplyr::starts_with("r_"), .after = "following")
    |> dplyr::mutate(processed_at = observatory$standardise$date(Sys.Date()))
)


# Teardown ----------------------------------------------------------------
ecos$overwrite_USER(users)
