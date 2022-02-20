# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repository")) repository <- Repository$new()
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()


# Query Github ------------------------------------------------------------
withr::with_seed(2212, suppressWarnings(
    packages <- repository$read_cran_desc()
    |> dplyr::pull("package")
    |> setdiff(repo_archive$show()$repo)
    |> sample()
))

pb <- progress::progress_bar$new(format = "Quering Github Repos [:bar] :current/:total (:percent) eta: :eta", total = length(packages), clear = FALSE)
for(package in packages) tryCatch({
    github$alter_PAT()
    if((which(packages %in% package) - 1) %% 10 == 0) while(github$return_remaining_quote() < 50) Sys.sleep(60)

    try(pb$tick(1), silent = TRUE)
    suppressMessages({
        github_slug <- repository$read_cran_desc() |> dplyr::filter(package %in% !!package) |> dplyr::pull(github_slug)
        owner <- github$extract$owner(github_slug)
        repo <- github$extract$repo(github_slug)
    })

    repo_overview <- query$package$overview(owner, repo)
    repo_archive$save(repo_overview, tags = c("entity:repo", "type:overview", paste0("id:", repo_overview$id)))

    repo_contributors <- query$package$contributors(owner, repo) |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
    repo_archive$save(repo_contributors, tags = c("entity:repo", "type:contributors", paste0("id:", repo_overview$id)))

    repo_stargazers <- if(repo_overview$stargazers_count == 0) list() else query$package$stargazers(owner, repo)
    repo_stargazers <- repo_stargazers |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
    repo_archive$save(repo_stargazers, tags = c("entity:repo", "type:stargazers", paste0("id:", repo_overview$id)))

    repo_watchers <- if(repo_overview$watchers_count == 0) list() else query$package$watchers(owner, repo)
    repo_watchers <- repo_watchers |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
    repo_archive$save(repo_watchers, tags = c("entity:repo", "type:watchers", paste0("id:", repo_overview$id)))

    suppressMessages(repo_archive$commit())
    try(pb$message(glue("Retrieved `{package}` information")), silent = TRUE)
}, error = function(e){
    suppressMessages(repo_archive$rollback())
    try(pb$message(glue("Failed to retrieve `{package}` information")), silent = TRUE)
})


# Teardown ----------------------------------------------------------------
repo_archive$clean()
