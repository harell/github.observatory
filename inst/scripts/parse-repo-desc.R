# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repository")) repository <- Repository$new()
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()


# Load cached data --------------------------------------------------------
invisible(
    repo_dictionary <- repo_archive$show()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::pull(artifact)
    |> repo_archive$load()
    |> purrr::map_dfr(~tibble::tibble(id = .x[["id"]], repo = .x[["name"]]))
)

invisible(
    tidy_repo_desc <- repository$create_repo_desc()$read_repo_desc()
    |> dplyr::select(-id)
    |> dplyr::left_join(repo_dictionary, by = "repo")
    |> tidyr::drop_na(id)
    |> dplyr::distinct()
)


# Parse stargazers --------------------------------------------------------
tags <- repo_archive$show() |> dplyr::mutate(id = as.integer(id)) |> dplyr::filter(entity %in% "repo", type %in% "stargazers", id %in% tidy_repo_desc$id)
artifacts <- repo_archive$load(tags$artifact)
names(artifacts) <- as.character(tags$id)

pb <- progress::progress_bar$new(format = "Parsing Stargazers [:bar] :current/:total (:percent) eta: :eta", total = nrow(tidy_repo_desc), clear = FALSE)
for(id in tags$id) tryCatch({
    if((which(id %in% tags$id) - 1) %% 100 == 0) try(pb$tick(100), silent = TRUE)

    invisible(
        stargazers <- artifacts
        |> purrr::pluck(as.character(id))
        |> purrr::map_dfr(~tibble::tibble(id = .x[["id"]], login = .x[["login"]]))
        |> dplyr::summarise(dplyr::across(.fns = list))
    )

    if(nrow(stargazers) == 0) next
    tidy_repo_desc[tidy_repo_desc$id %in% id, "stargazers_id"][[1]] <- stargazers$id
    tidy_repo_desc[tidy_repo_desc$id %in% id, "stargazers_login"][[1]] <- stargazers$login
}, error = function(e) pb$message(glue("Failed to parse `{repo}`", repo = tidy_repo_desc |> dplyr::filter(id == !!id) |> dplyr::pull(repo))))


# Teardown ----------------------------------------------------------------
repository$write_repo_desc(tidy_repo_desc)$commit()
