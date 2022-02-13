# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()
repo_archive <- RepoArchive$new()


# Load cached data --------------------------------------------------------
tidy_repo_desc <- repository$create_repo_desc()$read_repo_desc()


# Parse stargazers --------------------------------------------------------
invisible(
    tags <- repo_archive$show()
    |> dplyr::filter(entity %in% "repo", type %in% "stargazers")
    |> dplyr::inner_join(repository$read_cran_desc(), by = c("repo" = "package"))
)
artifacts <- repo_archive$load(tags$artifact)
names(artifacts) <- tags$repo

pb <- progress::progress_bar$new(format = "Parsing Stargazers [:bar] :current/:total (:percent) eta: :eta", total = length(tags$repo), clear = FALSE)
for(repo in tags$repo) tryCatch({
    pb$tick(1)

    invisible(
        stargazers_id <- artifacts
        |> purrr::pluck(repo)
        |> purrr::map_chr(~purrr::pluck(.x, "id"))
        |> tibble::enframe("repo", "stargazer")
        |> dplyr::mutate(repo = !!repo)
        |> dplyr::group_by(repo)
        |> dplyr::summarise(stargazers = list(as.integer(stargazer)), .groups = "drop")
        |> dplyr::pull(stargazers)
    )

    invisible(
        stargazers_login <- artifacts
        |> purrr::pluck(repo)
        |> purrr::map_chr(~purrr::pluck(.x, "login"))
        |> tibble::enframe("repo", "stargazer")
        |> dplyr::mutate(repo = !!repo)
        |> dplyr::group_by(repo)
        |> dplyr::summarise(stargazers = list(as.character(stargazer)), .groups = "drop")
        |> dplyr::pull(stargazers)
    )

    if(length(stargazers_id) == 0) next
    tidy_repo_desc[tidy_repo_desc$repo %in% repo, "stargazers_id"][[1]] <- stargazers_id
    tidy_repo_desc[tidy_repo_desc$repo %in% repo, "stargazers_login"][[1]] <- stargazers_login

}, error = function(e) pb$message(glue("Failed to parse `{repo}`")))


# Parse package metadata --------------------------------------------------
invisible(
    tags <- repo_archive$show()
    |> dplyr::filter(entity %in% "repo", type %in% "overview")
    |> dplyr::inner_join(repository$read_cran_desc(), by = c("repo" = "package"))
)
artifacts <- repo_archive$load(tags$artifact)
names(artifacts) <- tags$repo

pb <- progress::progress_bar$new(format = "Parsing Metadata [:bar] :current/:total (:percent) eta: :eta", total = length(tags$repo), clear = FALSE)
for(repo in tags$repo) tryCatch({
    pb$tick(1)
    tidy_repo_desc[tidy_repo_desc$repo %in% repo, "repo_id"] <- artifacts |> purrr::pluck(repo, "id") |> as.integer()
}, error = function(e) pb$message(glue("Failed to parse `{repo}`")))


# Teardown ----------------------------------------------------------------
repository$write_repo_desc(tidy_repo_desc)$commit()
