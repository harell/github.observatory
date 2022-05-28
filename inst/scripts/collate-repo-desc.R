# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()
if(does_not_exist("repo_db")) repo_db <- RepoQueryDB$new()
if(does_not_exist("repo_queue")) repo_queue <- QueryQueue$new(ecos)$REPO
pkgs_on_cran <- ecos$read_PACKAGE()


# Inquire -----------------------------------------------------------------
msg_bar <- "Quering Github Repos [:bar] :current/:total (:percent) eta: :eta"
pb <- progress::progress_bar$new(format = msg_bar, total = repo_queue$size(), clear = FALSE)
while(repo_queue$size() > 0) { try(pb$tick(1), silent = TRUE); tryCatch({
    github$alter_PAT()
    if((repo_queue$size() - 1) %% 10 == 0) while(github$return_remaining_quote() < 10) Sys.sleep(60)

    suppressMessages({
        package <- repo_queue$pop()
        full_name <- pkgs_on_cran |> dplyr::filter(package %in% !!package) |> dplyr::pull(full_name)
        owner <- github$extract$owner(full_name)
        repo <- github$extract$repo(full_name)
    })

    repo_overview <- observatory$query$package$overview(owner, repo)
    repo_overview <- purrr::list_modify(repo_overview, package = package)
    repo_db$save(data = repo_overview, entity = "repo", type = "overview", id = repo_overview$id, alias = package)

    repo_contributors <- observatory$query$package$contributors(owner, repo) |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
    if(length(repo_contributors) > 0) (
        repo_contributors
        |> repo_db$save(entity = "repo", type = "contributors", id = repo_overview$id, alias = package)
    )

    if(repo_overview$stargazers_count > 0) (
        observatory$query$package$stargazers(owner, repo)
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> repo_db$save(entity = "repo", type = "stargazers", id = repo_overview$id, alias = package)
    )

    if(repo_overview$subscribers_count > 0) (
        observatory$query$package$watchers(owner, repo)
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> repo_db$save(entity = "repo", type = "watchers", id = repo_overview$id, alias = package)
    )

    repo_db$commit()
    try(pb$message(events$SucceededToQueryRepo(package)), silent = TRUE)

}, error = function(e){
    repo_db$rollback()
    try(pb$message(events$FailedToQueryRepo(package)), silent = TRUE)
})}

