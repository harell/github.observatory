# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()
archive <- Archive$new()


# Load cached data --------------------------------------------------------
tidy_repo_desc <- repository$read_repo_desc()


# Parse stargazers --------------------------------------------------------
tags <- archive$show() |> dplyr::filter(type %in% "stargazers")
artifacts <- archive$load(tags$artifact)
names(artifacts) <- tags$repo

pb <- progress::progress_bar$new(format = "Parsing Stargazers [:bar] :current/:total (:percent) eta: :eta", total = length(tags$repo), clear = FALSE)
entries <- tibble::tibble()
for(repo in tags$repo){
    pb$tick(1)
    invisible(
        new_entry <- artifacts
        |> purrr::pluck(repo)
        |> purrr::map_chr(~purrr::pluck(.x, "login"))
        |> tibble::enframe("repo", "stargazer")
        |> dplyr::mutate(repo = !!repo)
        |> dplyr::group_by(repo)
        |> dplyr::summarise(stargazer = list(stargazer), .groups = "drop")
    )
    if(nrow(new_entry) == 0) new_entry <- tibble::tibble(repo = !!repo, stargazer = NULL)
    entries <- dplyr::bind_rows(entries, new_entry)
}
entries |> dplyr::n_distinct("stargazer")



# Teardown ----------------------------------------------------------------
repository$write_repo_desc(tidy_pkg_desc)
repository$commit()
