# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
repo <- Repository$new()


# Description -------------------------------------------------------------
pkg_desc <- tools::CRAN_package_db()

(
    tidy_pkg_desc <- pkg_desc
    |> standardise_col_names()
    |> dplyr::rowwise()
    |> dplyr::transmute(
        package = as.character(package),
        github_url = dplyr::if_else(ge$github$is_valid_url(bug_reports), ge$github$extract_root(bug_reports), ge$github$compose_cran_slug(package))
    )
    |> dplyr::ungroup()
)

repo$write_package_description_table(tidy_pkg_desc)


# Github Stats ------------------------------------------------------------
archive <- Archive$new()

repo <- "dplyr"
owner <- "tidyverse"
artifact <- gh$package$get_stargazers(owner, repo)
archive$save(artifact, tags = c("type:overview", paste0("owner:",owner), paste0("repo:",repo)))
archive$show()


# Teardown ----------------------------------------------------------------
repo$commit()

