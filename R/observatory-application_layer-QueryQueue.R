#' @title Query Queue
#' @keywords internal
#' @export
#' @noRd
QueryQueue <- R6::R6Class(
   cloneable = FALSE, public = list(
        initialize = function(path = usethis::proj_path("_cache", "tables")){private$depo <- Depository$new(path)}
    ), private = list(
        # Private Fields ----------------------------------------------------------
        depo = new.env(),
        null_query = c()
        # Private Methods ---------------------------------------------------------
    ), active = list(
        REPO = function() {return(new.env())},
        USER = function() {return(new.env())}
    )
)
