# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Config ------------------------------------------------------------------
user_id <- 7226303 # harell
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


fellowship <- ecos$read_FOLLOWING()

result <- tibble::tibble(from = NA_integer_, to = NA_character_)[0,]

degrees <- 1

while(degrees > 0){
    existing_users <- unique(result$to)

    result <- result |>
        dplyr::bind_rows(dplyr::filter(fellowship, from %in% user_id)) |>
        dplyr::distinct()

    all_users <- unique(c(result$to, result$from))
    new_users <- setdiff(result$to, existing_users)

    degrees <- degrees - 1
    if(all(is.na(new_users))) break
}



(
    fellowship
    |> dplyr::filter(from %in% user_id)
    |> dplyr::distinct()
)


# Control Logic -----------------------------------------------------------
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 1))
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 2))
(deps <- .recommenders$repos_graph$reverse_depends(ecos, repo_id, degrees = 4))

