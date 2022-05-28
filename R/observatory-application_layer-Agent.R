#' @title Query Queue
#' @description
#' ID Queues for Github entities to query.
#' @family R Ecosystem classes
#' @export
Agent <- R6::R6Class(
    cloneable = FALSE,
    public = list(
        #' @description Instantiate an Agent object
        #' @param ecos (`Repository`) An \link{Ecosystem} object.
        initialize = function(ecos = Ecosystem$new()){
            private$ecos <- ecos
        }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        ecos = new.env()
        # Private Methods ---------------------------------------------------------
    )
)
