# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()
archive <- Archive$new()


# Query Github ------------------------------------------------------------
withr::with_seed(2212, suppressWarnings(
    packages <- repository$read_cran_desc()
    |> dplyr::pull("package")
    |> setdiff(archive$show()$repo)
    |> sample()
))

pb <- progress::progress_bar$new(format = "Quering Github [:bar] :current/:total (:percent) eta: :eta", total = length(packages), clear = FALSE)
for(package in packages) tryCatch({
    if((which(packages %in% package) - 1) %% 10 == 0) if(github$return_remaining_quote() < 50) {pb$message(glue("Reached GitHub API call limit")); break}
    # if((which(packages %in% package) - 1) %% 10 == 0) while(github$return_remaining_quote() < 50) Sys.sleep(60)

    try(pb$tick(1), silent = TRUE)
    suppressMessages({
        github_slug <- repository$read_cran_desc() |> dplyr::filter(package %in% !!package) |> dplyr::pull(github_slug)
        owner <- github$extract$owner(github_slug)
        repo <- github$extract$repo(github_slug)
    })

    artifact <- query$package$overview(owner, repo)
    archive$save(artifact, tags = c("type:overview", paste0("owner:", owner), paste0("repo:", repo)))

    artifact <- query$package$stargazers(owner, repo)
    archive$save(artifact, tags = c("type:stargazers", paste0("owner:", owner), paste0("repo:", repo)))

    pb$message(glue("Retrieved `{package}` information"))
}, error = function(e) pb$message(glue("Failed to get `{package}` information")))


# Teardown ----------------------------------------------------------------
archive$clean()
