# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()
if(does_not_exist("depo")) depo <- Depository$new()


# Load data ---------------------------------------------------------------
USER_all <- depo$read_USER(filter = "everything")
USER_new <- dplyr::filter(USER_all, is.na(r_stargazer_count))
USER_old <- dplyr::setdiff(USER_all, USER_new)
FOLLOWING <- depo$read_FOLLOWING()
SPECTATOR <- depo$read_SPECTATOR()


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
    USER_new <- USER_new
    |> dplyr::select(-dplyr::starts_with("r_"))
    |> dplyr::left_join(followers, by = "id")
    |> dplyr::left_join(following, by = "id")
    |> dplyr::left_join(relationships, by = "id")
    |> purrr::modify_at(dplyr::vars(dplyr::starts_with("r_")), tidyr::replace_na, replace = 0)
    |> dplyr::relocate(dplyr::starts_with("r_"), .after = "following")
    |> dplyr::mutate(processed_at = Sys.Date() |> observatory$standardise$date())
)


# Consolidate data --------------------------------------------------------
(
    users <- dplyr::bind_rows(USER_new, USER_old)
    |> dplyr::arrange(id)
)

# Teardown ----------------------------------------------------------------
depo$overwrite_USER(users)
