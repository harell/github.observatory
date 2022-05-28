# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_db")) repo_db <- RepoQueryDB$new()
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Load cached data --------------------------------------------------------
invisible(
    queries <- repo_db$load()
    |> dplyr::filter(type %in% c("contributors", "stargazers", "watchers"))
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, type, .keep_all = TRUE)
)


# Parse spectators --------------------------------------------------------
invisible(
    tidy_spectators <- queries
    |> dplyr::rename(repo_id = id)
    |> dplyr::rowwise()
    |> dplyr::mutate(obj = data |> jsonlite::fromJSON() |> list())
    |> tidyr::unnest(obj, keep_empty = TRUE, ptype = list(login = "character", id = "integer"))
    |> dplyr::ungroup()
    |> dplyr::transmute(
        repo_id = as.integer(repo_id),
        user_id = as.integer(id),
        user_role = stringr::str_remove(type, "s$")
    )
    |> observatory$discard$robots(user_id)
)


# Teardown ----------------------------------------------------------------
ecos$overwrite_SPECTATOR(tidy_spectators)
