# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()
if(does_not_exist("gdrive")) gdrive <- GDrive$new()


# Load data ---------------------------------------------------------------
USER <- gdrive$read_USER(filter = "latest")
USER_complement <- dplyr::setdiff(gdrive$read_USER(filter = "everything"), USER)
FOLLOWING <- gdrive$read_FOLLOWING()
SPECTATOR <- gdrive$read_SPECTATOR()


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
    USER_STAR <- USER
    |> dplyr::select(-dplyr::starts_with("r_"))
    |> dplyr::left_join(followers, by = "id")
    |> dplyr::left_join(following, by = "id")
    |> dplyr::left_join(relationships, by = "id")
    |> purrr::modify_at(dplyr::vars(dplyr::starts_with("r_")), tidyr::replace_na, replace = 0)
    |> dplyr::relocate(dplyr::starts_with("r_"), .after = "following")
    |> dplyr::mutate(processed_at = Sys.Date() |> ge$standardise$date())
)


# Consolidate data --------------------------------------------------------
(
    users <- USER_complement
    |> dplyr::bind_rows(USER_STAR)
    |> dplyr::arrange(id, dplyr::desc(queried_at), processed_at)
    |> dplyr::group_by(id, queried_at)
    |> dplyr::slice_head(n = 1)
    |> dplyr::ungroup()
)

# Teardown ----------------------------------------------------------------
gdrive$overwrite_USER(users)
