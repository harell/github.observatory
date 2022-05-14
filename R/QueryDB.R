#' @title Database for Queries
#'
#' @export
QueryDB <- R6::R6Class(classname = "QueryDB", cloneable = FALSE, public = list(
    # Public Methods ----------------------------------------------------------
    #' @description Instantiate a QueryDB object
    #' @param path (`character`) A character denoting an existing directory of the Archive for which metadata will be aved and returned.
    #' @param immediate (`logical`) Should queries be committed immediately?
    initialize = function(path, immediate = FALSE){
        assertthat::assert_that(assertthat::is.scalar(path), assertthat::is.string(path))
        assertthat::assert_that(assertthat::is.scalar(immediate), assertthat::is.flag(immediate))

        fs::dir_create(path)
        private$db_path <- fs::path(path, "db", ext = "csv")
        private$immediate <- immediate
    },
    #' @description Save a query in the database
    #' @param data (`list`) The query data.
    #' @param entity (`character`) The entity name. For example "repo" or "user".
    #' @param type (`character`) The query type. For example "overview" or "contributors".
    #' @param id (`character`) The entity id. For example "280924484" or "clintools".
    save = function(data = list(), entity = character(), type = character(), id = character()){
        private$.save(data, entity, type, id)
        invisible(self)
    },
    #' @description Load queries from database
    load = function() { private$.load() }
), private = list(
    # Private Fields ----------------------------------------------------------
    db_path = character(),
    immediate = logical(),
    null_table = tibble::tibble(
        date = as.Date(NA),
        entity = NA_character_,
        type = NA_character_,
        id = NA_character_,
        data = NA_character_
    )[0,],
    # Private Methods ---------------------------------------------------------
    .save = function(data, entity, type, id) { stop() },
    .load = function() { stop() }
))


# Private Methods ---------------------------------------------------------
QueryDB$set("private", ".save", overwrite = TRUE, value = function(data, entity, type, id){
    assertthat::assert_that(
        "list" %in% class(data),
        assertthat::is.scalar(entity),
        assertthat::is.scalar(type),
        assertthat::is.scalar(id)
    )

    invisible(
        entry <- private$null_table
        |> tibble::add_row(
            date   = lubridate::today("UTC"),
            entity = as.character(entity),
            type   = as.character(type),
            id     = as.character(id),
            data   = as.character(jsonlite::toJSON(data))
        )
    )

    readr::write_csv(entry, file = private$db_path, na = "", append = fs::file_exists(private$db_path), progress = FALSE)

    invisible()
})

QueryDB$set("private", ".load", overwrite = TRUE, value = function(data, entity, type, id){
    mtcars
})
