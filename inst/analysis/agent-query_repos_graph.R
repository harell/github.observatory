# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Config ------------------------------------------------------------------
repo_id <- 318095552 # R6P
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

.recommenders$repos_graph$depends <- function(ecos, repo_id, degrees) {
    new_dependencies <- .recommenders$utils$map_repo2package(repo_id)
    dependencies <- ecos$read_DEPENDENCY()

    result <- tibble::tibble(from = NA_character_, to = NA_character_)[0,]
    while(degrees > 0){
        existing_dependencies <- unique(result$to)

        result <- result |>
            dplyr::bind_rows(dplyr::filter(dependencies, from %in% new_dependencies)) |>
            dplyr::distinct()

        all_dependencies <- unique(c(result$to, result$from))
        new_dependencies <- setdiff(result$to, existing_dependencies)

        degrees <- degrees - 1
    }

    return(result)
}


# Control Logic -----------------------------------------------------------
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 1))
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 2))
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 4))

