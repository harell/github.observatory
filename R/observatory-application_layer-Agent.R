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
        #' @param user_id (`integer`) Github User ID
        #' @return (`data.frame`)
        recommend_packages_to_user = function(user_id, n) { return(private$.recommend_packages_to_user(user_id, n)) }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        ecos = new.env(),
        # Private Methods ---------------------------------------------------------
        .recommend_packages_to_user = function(...) { stop() }
    )
)


# Private Methods ---------------------------------------------------------
Agent$set(which = "private", name = ".recommend_packages_to_user", overwrite = TRUE, value = function(user_id, n) {
    mtcars
})

