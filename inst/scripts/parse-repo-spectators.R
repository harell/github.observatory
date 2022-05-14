# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()
if(does_not_exist("depo_repo")) depo_repo <- Depository$new()


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
    |> observatory$discard$robots(var = user_id)
)


# Teardown ----------------------------------------------------------------
depo_repo$overwrite_SPECTATOR(tidy_spectators)
