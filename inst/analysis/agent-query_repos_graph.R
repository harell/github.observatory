# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Config ------------------------------------------------------------------
repo_id <- 19521307 # R6
degrees <- 1


# Helpers -----------------------------------------------------------------
.recommenders <- new.env()

.recommenders$utils <- new.env()

.recommenders$utils$map_repo2package <- function(repo_id){
    return(
        ecos$read_REPO()
        |> dplyr::arrange(dplyr::desc(queried_at))
        |> dplyr::group_by(id)
        |> dplyr::slice_head(n = 1)
        |> dplyr::ungroup()
        |> dplyr::filter(id %in% repo_id)
        |> dplyr::pull(package)
    )
}

.recommenders$repos_graph <- new.env()

.recommenders$repos_graph$reverse_depends <- function(ecos, repo_id, degrees) {
    new_dependencies <- .recommenders$utils$map_repo2package(repo_id)
    dependencies <- ecos$read_DEPENDENCY()

    result <- tibble::tibble(from = NA_character_, to = NA_character_)[0,]
    while(degrees > 0){
        existing_dependencies <- unique(result$from)

        result <- result |>
            dplyr::bind_rows(dplyr::filter(dependencies, to %in% new_dependencies)) |>
            dplyr::distinct()

        all_dependencies <- unique(c(result$from, result$to))
        new_dependencies <- setdiff(result$from, existing_dependencies)
        if(all(is.na(new_dependencies))) break
        degrees <- degrees - 1
    }

    return(result)
}


# Control Logic -----------------------------------------------------------
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 1))
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 2))
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 4))

