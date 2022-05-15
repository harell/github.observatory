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
    #' @param alias (`character`) The entity alias name. For example "dplyr" or "hadley".
    save = function(data = list(), entity = character(), type = character(), id = character(), alias = character()){
        entry <- private$.compose_row(data, entity, type, id, alias)

        if(private$immediate){
            private$.save(entry)
        } else {
            private$buffer_table <- dplyr::bind_rows(private$buffer_table, entry)
        }

        invisible(self)
    },
    #' @description Load queries from database
    load = function() { private$.load() },
    #' @description Commit entires to database
    commit = function() { private$.commit() ; invisible(self) },
    #' @description Rollback changes from the database
    rollback = function() {private$.rollback(); invisible(self) }
), private = list(
    # Private Fields ----------------------------------------------------------
    db_path = character(),
    immediate = logical(),
    buffer_table = tibble::tibble(),
    null_table = tibble::tibble(
        date = as.Date(NA),
        entity = NA_character_,
        type = NA_character_,
        id = NA_character_,
        alias = NA_character_,
        data = NA_character_
    )[0,],
    # Private Methods ---------------------------------------------------------
    .save = function(data, entity, type, id) { stop() },
    .load = function() { stop() },
    .commit = function() { stop() },
    .rollback = function() { stop() },
    .compose_row = function(data, entity, type, id) { stop() }
))


# Private Methods ---------------------------------------------------------
QueryDB$set("private", ".save", overwrite = TRUE, value = function(entry){
    readr::write_csv(entry, file = private$db_path, na = "", append = fs::file_exists(private$db_path), progress = FALSE)
})

QueryDB$set("private", ".load", overwrite = TRUE, value = function(){
    if(isFALSE(fs::file_exists(private$db_path))) return(private$null_table)
    return(
        private$db_path
        |> readr::read_csv(lazy = FALSE, show_col_types = FALSE)
        |> tibble::as_tibble()
        |> dplyr::distinct()
    )
})

QueryDB$set("private", ".commit", overwrite = TRUE, value = function(){
    private$.save(private$buffer_table)
    private$.rollback()
})

QueryDB$set("private", ".rollback", overwrite = TRUE, value = function(){
    private$buffer_table <- private$null_table
})

QueryDB$set("private", ".compose_row", overwrite = TRUE, value = function(data, entity, type, id, alias){
    assertthat::assert_that(
        "list" %in% class(data),
        assertthat::is.scalar(entity),
        assertthat::is.scalar(type),
        assertthat::is.scalar(id),
        assertthat::is.scalar(alias)
    )

    return(
        entry <- private$null_table
        |> tibble::add_row(
            date   = lubridate::today("UTC"),
            entity = as.character(entity),
            type   = as.character(type),
            id     = as.character(id),
            alias  = as.character(alias),
            data   = as.character(jsonlite::toJSON(data))
        )
    )
})

# Derivatives -------------------------------------------------------------
#' @describeIn QueryDB User QueryDB
UserQueryDB <- new.env()
UserQueryDB$new <- function(path = usethis::proj_path("_cache", "queries", "user"), immediate = FALSE) QueryDB$new(path, immediate)

#' @describeIn QueryDB Repo QueryDB
RepoQueryDB <- new.env()
RepoQueryDB$new <- function(path = usethis::proj_path("_cache", "queries", "repo"), immediate = FALSE) QueryDB$new(path, immediate)


