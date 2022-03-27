# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()
if(does_not_exist("depo_repo")) depo_repo <- Depository$new()


# Query Github ------------------------------------------------------------
pkgs_on_cran <- depo_repo$read_PACKAGE()

pkgs_to_skip <- tryCatch(
    repo_archive$show()
    |> dplyr::filter(type %in% "overview")
    |> dplyr::pull(artifact)
    |> repo_archive$load()
    |> purrr::map_chr(~purrr::pluck(.x, "name"))
    , error = function(e) return(character())
)

withr::with_seed(2212, suppressWarnings(
    packages <- pkgs_on_cran
    |> dplyr::pull("package")
    |> setdiff(pkgs_to_skip)
    |> sample()
))

pb <- progress::progress_bar$new(format = "Quering Github Repos [:bar] :current/:total (:percent) eta: :eta", total = length(packages), clear = FALSE)
for(package in packages){ try(pb$tick(1), silent = TRUE); tryCatch({
    github$alter_PAT()
    if((which(packages %in% package) - 1) %% 10 == 0) while(github$return_remaining_quote() < 10) Sys.sleep(60)

    suppressMessages({
        full_name <- pkgs_on_cran |> dplyr::filter(package %in% !!package) |> dplyr::pull(full_name)
        owner <- github$extract$owner(full_name)
        repo <- github$extract$repo(full_name)
    })

    repo_overview <- query$package$overview(owner, repo)
    repo_archive$save(repo_overview, tags = c("entity:repo", "type:overview", paste0("id:", repo_overview$id)))

    repo_contributors <- query$package$contributors(owner, repo) |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
    if(length(repo_contributors) > 0) (
        repo_contributors
        |> repo_archive$save(tags = c("entity:repo", "type:contributors", paste0("id:", repo_overview$id)))
    )

    if(repo_overview$stargazers_count > 0) (
        query$package$stargazers(owner, repo)
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> repo_archive$save(tags = c("entity:repo", "type:stargazers", paste0("id:", repo_overview$id)))
    )

    if(repo_overview$subscribers_count > 0) (
        query$package$watchers(owner, repo)
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> repo_archive$save(tags = c("entity:repo", "type:watchers", paste0("id:", repo_overview$id)))
    )

    suppressMessages(repo_archive$commit())
    try(pb$message(glue("Retrieved `{package}` information")), silent = TRUE)
}, error = function(e){
    suppressMessages(repo_archive$rollback())
    try(pb$message(glue("Failed to retrieve `{package}` information")), silent = TRUE)
})}


# Teardown ----------------------------------------------------------------
repo_archive$clean()
