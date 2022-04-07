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
        null_query = c(),
        # Private Methods ---------------------------------------------------------
        generate_REPO_queue = function() collections::priority_queue(),
        generate_USER_queue = function() collections::priority_queue()
    ), active = list(
        REPO = function() private$generate_REPO_queue(),
        USER = function() private$generate_USER_queue()
    )
)


# Class Methods -----------------------------------------------------------
# QueryQueue$set(which = "private", name = "generate_REPO_queue", overwrite = TRUE, value = function() {
#     collections::priority_queue
# })
