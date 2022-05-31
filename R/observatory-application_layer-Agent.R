#' @title Recommendation Engine for R Users and R Packages
#' @description
#' Recommend R Users on Github some R packages that they may like.
#'
#' @param user_id (`integer`) Github User ID.
#' @param repo_id (`integer`) Github Repo ID.
#' @param n (`integer`) How many recommendation should the function return.
#' @param degrees (`integer`) How many degrees of separation should be included? `1` are only the closest nodes. `Inf` returns all the graph.
#'
#' @section packages to user methods:
#'
#' * `random` returns the information of random packages
#'
#' @section users to user methods:
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
        #' @description Given a `user_id` suggests `n` packages the user might like.
        #' @param method (`character`) The recommendation filtering technique to employ. See **packages to user methods** section for details.
        recommend_packages_to_user = function(user_id, n, method) { return(private$.recommend_packages_to_user(user_id, n, method)) },
        #' @description Given a `user_id` suggests `n` packages the user might like.
        #' @param method (`character`) The recommendation filtering technique to employ. See **users to user methods** section for details.
        recommend_users_to_user = function(user_id, n, method) { stop() },
        #' @description Given a `repo_id` find all linked packages in `degrees` degrees of separation.
        #' @param method (`character`) The link type to employ. Either `depends` or `reverse depends`.
        query_package_graph = function(repo_id, degrees = 1, method) { stop() },
        #' @description Given a `user_id` find all linked users in `degrees` degrees of separation.
        #' @param method (`character`) The link type to employ. Either `followers` or `following`.
        query_user_graph = function(user_id, degrees = 1, method) { stop() }
        ), private = list(
        # Private Fields ----------------------------------------------------------
        ecos = new.env(),
        # Private Methods ---------------------------------------------------------
        .recommend_packages_to_user = function(...) { stop() }
    )
)


# Private Methods ---------------------------------------------------------
Agent$set(which = "private", name = ".recommend_packages_to_user", overwrite = TRUE, value = function(user_id, n, method) {
    method <- match.arg(tolower(method), c("random"))

    repos_to_exclude <- .recommenders$utils$get_repos_to_exclude(ecos, user_id)

    repos_id <- switch (method,
        random = .recommenders$repos2users$random(ecos, user_id, n, repos_to_exclude)
    )

    return(
        repos_info <- ecos$read_PACKAGE()
        |> dplyr::select(-full_name)
        |> dplyr::inner_join(ecos$read_REPO() |> dplyr::filter(id %in% repos_id), by = "package")
    )
})


# Recommenders ------------------------------------------------------------
.recommenders <- new.env()

.recommenders$utils <- new.env()

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

.recommenders$utils$get_repos_to_exclude <- function(ecos, user_id) {
    tryCatch(return(
            ecos$read_SPECTATOR()
            |> dplyr::filter(user_id %in% !!user_id)
            |> dplyr::distinct(repo_id)
            |> dplyr::pull(repo_id)
    ), error = function(e) return(0L))
}
