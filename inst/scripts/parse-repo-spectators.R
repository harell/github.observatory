# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_db")) repo_db <- RepoQueryDB$new()
if(does_not_exist("depo_repo")) depo_repo <- Depository$new()


# Load cached data --------------------------------------------------------
invisible(
    queries <- repo_db$load()
    |> dplyr::filter(data %not_in% "[]")
    |> dplyr::filter(type %in% c("contributors", "stargazers", "watchers"))
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, type, .keep_all = TRUE)
)


# Parse spectators --------------------------------------------------------
invisible(
    queries$data <- queries$data
    |> purrr::map(~.x |> jsonlite::fromJSON() |> purrr::pluck("id", 1))
    |> as.integer()
)

invisible(
    spectators <- queries
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
