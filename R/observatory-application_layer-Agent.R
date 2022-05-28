#' @title R Users and Packages Recommendation Engine
#' @description
#' Recommend R Users on Github some R packages that they may like.
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
        #' @param user_id (`integer`) Github User ID.
        #' @param n (`integer`) How many recommendation should the function return.
        #' @return (`data.frame`)
        recommend_packages_to_user = function(user_id, n = 10L) { return(private$.recommend_packages_to_user(user_id, n)) }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        ecos = new.env(),
        # Private Methods ---------------------------------------------------------
        .recommend_packages_to_user = function(...) { stop() }
    )
)


# Private Methods ---------------------------------------------------------
Agent$set(which = "private", name = ".recommend_packages_to_user", overwrite = TRUE, value = function(user_id, n) {
    repos_to_exclude <- .recommenders$utils$get_repos_to_exclude(ecos, user_id)

    repos_id <- .recommenders$repos2users$random(ecos, user_id, n, repos_to_exclude)

    head(mtcars, n)
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
    ), error = function(e) return(integer(0)))
}
