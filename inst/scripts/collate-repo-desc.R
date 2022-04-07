# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()
if(does_not_exist("depo_repo")) depo_repo <- Depository$new()


# Query Github ------------------------------------------------------------
pkgs_on_cran <- depo_repo$read_PACKAGE()

pkgs_in_cache <- tryCatch(
    repo_archive$show()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::pull(artifact)
    |> repo_archive$load()
    |> purrr::map_chr(~purrr::pluck(.x, "package"))
    |> unique()
    , error = function(e) return(character())
)

withr::with_seed(1943, pkgs_to_query <- setdiff(pkgs_on_cran$package, pkgs_in_cache) |> sample())

queue <- collections::priority_queue(items = pkgs_to_query[2], priorities = 2)


# Inquire -----------------------------------------------------------------
msg_bar <- "Quering Github Repos [:bar] :current/:total (:percent) eta: :eta"
pb <- progress::progress_bar$new(format = msg_bar, total = queue$size(), clear = FALSE)
while(queue$size() > 0) { try(pb$tick(1), silent = TRUE); tryCatch({
    github$alter_PAT()
    if((queue$size() - 1) %% 10 == 0) while(github$return_remaining_quote() < 10) Sys.sleep(60)

    suppressMessages({
        package <- queue$pop()
        full_name <- pkgs_on_cran |> dplyr::filter(package %in% !!package) |> dplyr::pull(full_name)
        owner <- github$extract$owner(full_name)
        repo <- github$extract$repo(full_name)
    })

    repo_overview <- observatory$query$package$overview(owner, repo)
    repo_overview <- purrr::list_modify(repo_overview, package = package)
    repo_archive$save(repo_overview, tags = c("entity:repo", "type:overview", paste0("id:", repo_overview$id)))

    repo_contributors <- observatory$query$package$contributors(owner, repo) |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
    if(length(repo_contributors) > 0) (
        repo_contributors
        |> repo_archive$save(tags = c("entity:repo", "type:contributors", paste0("id:", repo_overview$id)))
    )

    if(repo_overview$stargazers_count > 0) (
        observatory$query$package$stargazers(owner, repo)
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> repo_archive$save(tags = c("entity:repo", "type:stargazers", paste0("id:", repo_overview$id)))
    )

    if(repo_overview$subscribers_count > 0) (
        observatory$query$package$watchers(owner, repo)
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> repo_archive$save(tags = c("entity:repo", "type:watchers", paste0("id:", repo_overview$id)))
    )

    suppressMessages(repo_archive$commit())
    try(pb$message(events$SucceededToQueryRepo(package)), silent = TRUE)

}, error = function(e){
    suppressMessages(repo_archive$rollback())
    try(pb$message(events$FailedToQueryRepo(package)), silent = TRUE)
})}


# Teardown ----------------------------------------------------------------
repo_archive$clean()
