#' @title Github Explorer Depository
#' @param filter (`character`) either 'latest' or 'everything'. If 'latest' then
#'   return the latest query of each entity. Otherwise, return all all records
#'   (might include multiple records per entity).
#' @keywords internal
#' @export
#' @noRd
Depository <- R6::R6Class(
    classname = "DepositoryRepository", cloneable = FALSE, public = list(
        initialize = function(path = usethis::proj_path("_cache", "tables")){
            private$path <- path
            fs::dir_create(private$path)
        },
        read_USER      = function() { return(private$read("USER")) },
        read_REPO      = function() { return(private$read("REPO")) },
        read_PACKAGE   = function() { return(private$read("PACKAGE")) },
        read_FOLLOWING = function() { return(private$read("FOLLOWING")) },
        read_SPECTATOR = function() { return(private$read("SPECTATOR")) },
        overwrite_USER = function(value) { private$overwrite("USER", value); invisible(self) },
        overwrite_REPO = function(value) { private$overwrite("REPO", value); invisible(self) },
        overwrite_PACKAGE = function(value) { private$overwrite("PACKAGE", value); invisible(self) },
        overwrite_FOLLOWING = function(value) { private$overwrite("FOLLOWING", value); invisible(self) },
        overwrite_SPECTATOR = function(value) { private$overwrite("SPECTATOR", value); invisible(self) },
        snapshot_USER = function() { private$snapshot("USER"); invisible(self) }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        path = ".",
        null_table = tibble::tibble(),
        # Private Methods ---------------------------------------------------------
        read = function(key) { stop() },
        overwrite = function(key, value) { stop() },
        snapshot = function(key) { stop() }
    )
)


# Class Methods -----------------------------------------------------------
Depository$set(which = "private", name = "read", overwrite = TRUE, value = function(key) {
    file <- fs::path(private$path, key, ext = "csv")

    if(file_not_exists(file)) return(private$null_table)

    invisible(
        data <- readr::read_csv(file, show_col_types = FALSE, progress = TRUE, lazy = FALSE)
        |> purrr::modify_if(lubridate::is.Date, observatory$standardise$date)
        |> dplyr::distinct()
    )

    return(data)
})

Depository$set(which = "private", name = "overwrite", overwrite = TRUE, value = function(key, value) {
    stopifnot(is.data.frame(value))

    return(
        value
        |> dplyr::distinct()
        |> readr::write_csv(fs::path(private$path, key, ext = "csv"), append = FALSE)
    )
})

Depository$set(which = "private", name = "snapshot", overwrite = TRUE, value = function(key) {

    invisible()
})
