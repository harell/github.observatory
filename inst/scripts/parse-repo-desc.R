# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()
archive <- Archive$new()


# Load cached data --------------------------------------------------------
tidy_repo_desc <- repository$create_repo_desc()$read_repo_desc()


# Parse stargazers --------------------------------------------------------
invisible(
    tags <- archive$show()
    |> dplyr::filter(type %in% "stargazers")
    |> dplyr::inner_join(repository$read_cran_desc(), by = c("repo" = "package"))
)
artifacts <- archive$load(tags$artifact)
names(artifacts) <- tags$repo

pb <- progress::progress_bar$new(format = "Parsing Stargazers [:bar] :current/:total (:percent) eta: :eta", total = length(tags$repo), clear = FALSE)
for(repo in tags$repo) tryCatch({
    pb$tick(1)
    invisible(
        stargazers <- artifacts
        |> purrr::pluck(repo)
        |> purrr::map_chr(~purrr::pluck(.x, "id"))
        |> tibble::enframe("repo", "stargazer")
        |> dplyr::mutate(repo = !!repo)
        |> dplyr::group_by(repo)
        |> dplyr::summarise(stargazers = list(as.integer(stargazer)), .groups = "drop")
        |> dplyr::pull(stargazers)
    )
    if(length(stargazers) == 0) next
    tidy_repo_desc[tidy_repo_desc$repo %in% repo, "stargazers"][[1]] <- stargazers
}, error = function(e) pb$message(glue("Failed to parse `{repo}` information")))


# Teardown ----------------------------------------------------------------
repository$write_repo_desc(tidy_repo_desc)$commit()
