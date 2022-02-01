# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
repository <- Repository$new()
archive <- Archive$new()


# Description -------------------------------------------------------------
pkg_desc <- tools::CRAN_package_db()

(
    tidy_pkg_desc <- pkg_desc
    |> standardise_col_names()
    |> dplyr::rowwise()
    |> dplyr::transmute(
        package = as.character(package),
        github_url = dplyr::if_else(github$is_valid_url(bug_reports), github$extract$root(bug_reports), ge$compose_cran_slug(package))
    )
    |> dplyr::ungroup()
)

repository$write_pkg_desc(tidy_pkg_desc)


# Github Stats ------------------------------------------------------------
## Download Packages Stats
withr::with_seed(2212, (
    packages <- repository$read_pkg_desc()
    |> dplyr::pull("package")
    |> setdiff(archive$show()$repo)
    |> sample()
))

for(package in packages) tryCatch({
    suppressMessages({
        github_url <- repository$read_pkg_desc() |> dplyr::filter(package %in% !!package) |> dplyr::pull(github_url)
        owner <- github$extract$owner(github_url)
        repo <- github$extract$repo(github_url)
    })

    artifact <- query$package$overview(github$extract$owner(github_url), repo)
    archive$save(artifact, tags = c("type:overview", paste0("owner:",owner), paste0("repo:",repo)))

    artifact <- query$package$stargazers(owner, repo)
    archive$save(artifact, tags = c("type:stargazers", paste0("owner:",owner), paste0("repo:",repo)))

    message(glue("Retrieved {package} information"))
}, error = function(e) message(glue("Failed to get {package} information")))

## Process Packages Stats
archive$show()


# Teardown ----------------------------------------------------------------
archive$clean()
repo$commit()

