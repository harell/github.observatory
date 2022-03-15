# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()
if(does_not_exist("gdrive_repo")) gdrive_repo <- GDrive$new()


# Load cached data --------------------------------------------------------
invisible(
    artifacts <- repo_archive$show()
    |> dplyr::filter(type %in% c("contributors", "stargazers", "watchers"))
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, type, .keep_all = TRUE)
)


# Parse spectators --------------------------------------------------------
invisible(
    artifacts$data <- artifacts$artifact
    |> purrr::map(~.x |> repo_archive$load() |> unlist())
    |> purrr::map(~as.integer(.x[names(.x) == "id"]))
)

invisible(
    spectators <- artifacts
    |> dplyr::transmute(
        repo_id = as.integer(id),
        user_id = data,
        user_role = stringr::str_remove(type, "s$")
    )
    |> tidyr::unnest(user_id)
)

invisible(
    tidy_spectators <- spectators
    |> ge$discard$robots(var = user_id)
)


# Statistics --------------------------------------------------------------
# length(unique(artifacts$id)) # How many packages have at least on spectator?
# janitor::tabyl(artifacts, type)
# janitor::tabyl(tidy_spectators, user_role)
# spectators_leaderboard <- dplyr::count(tidy_spectators, user_id, user_role, sort = TRUE)


# Teardown ----------------------------------------------------------------
gdrive_repo$overwrite_SPECTATOR(tidy_spectators)
