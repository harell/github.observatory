#' @title Recommendation Engine for R Users and R Packages
#' @description
#' Recommend R Users on Github some R packages that they may like.
#'
#' @param user_id (`integer`) Github User ID.
#' @param repo_id (`integer`) Github Repo ID.
#' @param n (`integer`) How many recommendation should the function return.
#' @param degrees (`integer`) How many degrees of separation should be included? `1` are only the closest nodes. `Inf` returns all the graph.
#'
#' @section recommend_repos_to_user:
#'
#' * `random` returns the information of random packages
#'
#' @section recommend_users_to_user:
#'
#' * `random` returns the information of random users
#'
#' @family R Ecosystem classes
#' @export
Agent <- R6::R6Class(
    cloneable = FALSE,
    public = list(
        #' @description Instantiate an Agent object
        #' @param ecos (`Repository`) An \link{Ecosystem} object.
        initialize = function(ecos = Ecosystem$new()){
            private$ecos <- ecos
        },
        #' @description Given a `user_id` suggests `n` repos the user might like.
        #' @param method (`character`) The recommendation filtering technique to employ. See **recommend_repos_to_user** section for details.
        recommend_repos_to_user = function(user_id, n, method) { return(private$.recommend_repos_to_user(user_id, n, method)) },
        #' @description Given a `user_id` suggests `n` users the user might like.
        #' @param method (`character`) The recommendation filtering technique to employ. See **recommend_users_to_user** section for details.
        recommend_users_to_user = function(user_id, n, method) { stop() },
        #' @description Given a `repo_id` find all linked packages in `degrees` degrees of separation.
        #' @param method (`character`) The link type to employ. Either `depends` or `reverse depends`.
        query_repos_graph = function(repo_id, degrees = 1, method) { return(private$.query_repos_graph(repo_id, degrees, method)) },
        #' @description Given a `user_id` find all linked users in `degrees` degrees of separation.
        #' @param method (`character`) The link type to employ. Either `followers` or `following`.
        query_user_graph = function(user_id, degrees = 1, method) { stop() }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        ecos = new.env(),
        # Private Methods ---------------------------------------------------------
        .recommend_repos_to_user = function(...) { stop() },
        .query_repos_graph = function(...) { stop() }
    )
)


# Private Methods ---------------------------------------------------------
Agent$set(which = "private", name = ".recommend_repos_to_user", overwrite = TRUE, value = function(user_id, n, method) {
    method <- match.arg(tolower(method), c("random"))

    repos_to_exclude <- .recommenders$utils$get_repos2exclude(private$ecos, user_id)

    repos_id <- switch (method,
                        random = .recommenders$repos2users$random(private$ecos, user_id, n, repos_to_exclude)
    )

    return(
        repos_info <- private$ecos$read_PACKAGE()
        |> dplyr::select(-full_name)
        |> dplyr::inner_join(private$ecos$read_REPO() |> dplyr::filter(id %in% repos_id), by = "package")
    )
})

Agent$set(which = "private", name = ".query_repos_graph", overwrite = TRUE, value = function(repo_id, degrees = 1, method) {
    method <- match.arg(tolower(method), c("depends", "reverse depends"))

    if(method == "depends"){
        return(.recommenders$repos_graph$depends(private$ecos, repo_id, degrees))
    } else if (method == "reverse depends") {
        NULL
    }
})


# Recommenders ------------------------------------------------------------
.recommenders <- new.env()

.recommenders$utils <- new.env()


# recommend_repos_to_user -------------------------------------------------
.recommenders$repos2users <- new.env()

.recommenders$repos2users$random <- function(ecos, user_id, n, repos_to_exclude) {
    set.seed(2144)
    tryCatch(return(
        repos <- ecos$read_REPO()
        |> dplyr::rename(repo_id = id)
        |> dplyr::filter(repo_id %not_in% repos_to_exclude)
        |> dplyr::sample_n(size = dplyr::n())
        |> dplyr::pull(repo_id)
        |> head(n)
    ), error = function(e) return(integer(0)))
}


# query_repos_graph -------------------------------------------------------
.recommenders$repos_graph <- new.env()

.recommenders$repos_graph$depends <- function(ecos, repo_id, degrees) {
    result <- tibble::tibble(from = NA_character_, to = NA_character_)[0,]

    tryCatch({
        new_dependencies <- .recommenders$utils$map_repo2package(repo_id)
        dependencies <- ecos$read_DEPENDENCY()


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


# Utilities ---------------------------------------------------------------
.recommenders$utils$get_repos2exclude <- function(ecos, user_id) {
    tryCatch(return(
        ecos$read_SPECTATOR()
        |> dplyr::filter(user_id %in% !!user_id)
        |> dplyr::distinct(repo_id)
        |> dplyr::pull(repo_id)
    ), error = function(e) return(0L))
}

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
