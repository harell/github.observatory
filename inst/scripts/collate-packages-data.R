# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
repository <- Repository$new()
archive <- Archive$new()


# Description -------------------------------------------------------------
pkg_desc <- tools::CRAN_package_db()

invisible(
    tidy_pkg_desc <- pkg_desc
    |> standardise_col_names()
    |> dplyr::rowwise()
    |> dplyr::transmute(
        package = as.character(package),
        github_slug = dplyr::if_else(
            github$is_valid_url(bug_reports),
            github$compose$slug(owner = github$extract$owner(bug_reports), repo = package),
            github$compose$slug(owner = "cran", repo = package)
        )
    )
    |> dplyr::ungroup()
)

repository$write_pkg_desc(tidy_pkg_desc)


# Github Stats ------------------------------------------------------------
## Download Packages Stats
withr::with_seed(2212, suppressWarnings(
    packages <- repository$read_pkg_desc()
    |> dplyr::pull("package")
    |> setdiff(archive$show()$repo)
    |> sample()
))

pb <- progress::progress_bar$new(format = "Quering Github [:bar] :current/:total (:percent) eta: :eta", total = length(packages), clear = FALSE)
for(package in packages) tryCatch({
    if(which(packages %in% package) - 1 %% 10 == 0) if(github$return_remaining_quote() < 50) {pb$message(glue("Reached GitHub API call limit")); break}

    pb$tick(1)
    suppressMessages({
        github_slug <- repository$read_pkg_desc() |> dplyr::filter(package %in% !!package) |> dplyr::pull(github_slug)
        owner <- github$extract$owner(github_slug)
        repo <- github$extract$repo(github_slug)
    })

    artifact <- query$package$overview(owner, repo)
    archive$save(artifact, tags = c("type:overview", paste0("owner:",owner), paste0("repo:",repo)))

    artifact <- query$package$stargazers(owner, repo)
    archive$save(artifact, tags = c("type:stargazers", paste0("owner:",owner), paste0("repo:",repo)))

    pb$message(glue("Retrieved {package} information"))
}, error = function(e) pb$message(glue("Failed to get {package} information")))

## Process Packages Stats
archive$show()


# Teardown ----------------------------------------------------------------
archive$clean()
repository$commit()
