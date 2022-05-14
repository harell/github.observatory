#' @title Database for Queries
#'
#' @export
QueryDB <- R6::R6Class(cloneable = FALSE, public = list(
    # Public Methods ----------------------------------------------------------
    #' @param path (`character`) A character denoting an existing directory of the Archive for which metadata will be aved and returned.
    #' @param immediate (`logical`) Should queries be committed immediately?
    initialize = function(path, immediate = FALSE){
        fs::dir_create(path)
        private$path <- path
        private$immediate <- immediate
    }
), private = list(
    # Private Fields ----------------------------------------------------------
    path = character(),
    immediate = logical()
    # Private Methods ---------------------------------------------------------
))
