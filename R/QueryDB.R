#' @title Database for Queries
#'
#' @export
QueryDB <- R6::R6Class(class = "QueryDB", cloneable = FALSE, public = list(
    # Public Methods ----------------------------------------------------------
    #' @description Instantiate a QueryDB object
    #' @param path (`character`) A character denoting an existing directory of the Archive for which metadata will be aved and returned.
    #' @param immediate (`logical`) Should queries be committed immediately?
    initialize = function(path, immediate = FALSE){
        fs::dir_create(path)
        private$path <- path
        private$immediate <- immediate
    },
    #' @description Save a query in the database
    #' @param data (`list`) The query data.
    #' @param entity (`character`) The entity name. For example "repo" or "user".
    #' @param type (`character`) The query type. For example "overview" or "contributors".
    #' @param id (`character`) The entity id. For example "280924484" or "clintools".
    save = function(data, entity, type, id){
        private$.save(data, entity, type, id)
        invisible(self)
    }
), private = list(
    # Private Fields ----------------------------------------------------------
    path = character(),
    immediate = logical(),
    # Private Methods ---------------------------------------------------------
    .save = function(data, entity, type, id) { stop() }
))


# Private Methods ---------------------------------------------------------
QueryDB$set("private", ".save", overwrite = TRUE, value = function(data, entity, type, id){
    invisible()
})

