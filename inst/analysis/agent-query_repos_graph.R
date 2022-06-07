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

.recommenders$repos_graph$depends <- function(ecos, repo_id, degrees) {
    result <- tibble::tibble(from = NA_integer_, to = NA_integer_)[0,]

    tryCatch({
        new_dependencies <- repo_id
        repos <- ecos$read_REPO() |> dplyr::distinct(id, package)
        invisible(
            dependencies <- ecos$read_DEPENDENCY()
            |> dplyr::mutate(to = dplyr::if_else(is.na(to), from, to))
            |> dplyr::left_join(repos, by = c(from = "package"))
            |> dplyr::transmute(from = id, to = to)
            |> dplyr::left_join(repos, by = c(to = "package"))
            |> dplyr::transmute(from = from, to = id)
            |> tidyr::drop_na()
        )

        while(degrees > 0){
            existing_dependencies <- unique(result$to)

            result <- result |>
                dplyr::bind_rows(dplyr::filter(dependencies, from %in% new_dependencies)) |>
                dplyr::distinct()

            all_dependencies <- unique(c(result$to, result$from))
            new_dependencies <- setdiff(result$to, existing_dependencies)

            degrees <- degrees - 1
            if(all(is.na(new_dependencies))) break
        }

        return(result)

    }, error = function(e) return(result))
}



# Control Logic -----------------------------------------------------------
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 1))
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 2))
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 4))


