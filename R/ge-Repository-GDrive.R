#' @title Github Explorer GDrive Repository
#' @param filter (`character`) either 'latest' or 'everything'. If 'latest' then
#'   return the latest query of each entity. Otherwise, return all all records
#'   (might include multiple records per entity).
#' @keywords internal
#' @export
#' @noRd
GDrive <- R6::R6Class(
    classname = "GDriveRepository", cloneable = FALSE, public = list(
        initialize = function(){
            private$path <- usethis::proj_path("_cache", "tables")
            fs::dir_create(private$path)
        },
        read_USER      = function(filter = "latest") { return(private$read("USER", filter = filter)) },
        read_REPO      = function(filter = "latest") { return(private$read("REPO", filter = filter)) },
        read_PACKAGE   = function() { return(private$read("PACKAGE", filter = "everything")) },
        read_FOLLOWING = function() { return(private$read("FOLLOWING", filter = "everything")) },
        read_SPECTATOR = function() { return(private$read("SPECTATOR", filter = "everything")) },
        snapshot_USER = function(value) { private$snapshot("USER", value); invisible(self) },
        snapshot_REPO = function(value) { private$snapshot("REPO", value); invisible(self) },
        overwrite_PACKAGE = function(value) { private$overwrite("PACKAGE", value); invisible(self) },
        overwrite_FOLLOWING = function(value) { private$overwrite("FOLLOWING", value); invisible(self) },
        overwrite_SPECTATOR = function(value) { private$overwrite("SPECTATOR", value); invisible(self) }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        path = ".",
        null_table = tibble::tibble(),
        # Private Methods ---------------------------------------------------------
        read = function(key) { return(tibble::tibble()) },
        overwrite = function(key, value) { return(invisible()) },
        snapshot = function(key, value) { return(invisible()) }
    )
)


# Class Methods -----------------------------------------------------------
GDrive$set(which = "private", name = "read", overwrite = TRUE, value = function(key, filter) {
    filter <- match.arg(tolower(filter), c("latest", "everything"))
    file <- fs::path(private$path, key, ext = "csv")

    if(file_not_exists(file)) return(private$null_table)

    data <- readr::read_csv(file, show_col_types = FALSE, progress = TRUE, lazy = FALSE)

    if (filter == "everything") return(data)
    if (filter == "latest") return(
        data
        |> dplyr::group_by(id, queried_at)
        |> dplyr::arrange(queried_at)
        |> dplyr::slice_tail(n = 1)
        |> dplyr::ungroup()
    )
})

GDrive$set(which = "private", name = "overwrite", overwrite = TRUE, value = function(key, value) {
    stopifnot(is.data.frame(value))

    file_name <- key

    readr::write_csv(value, fs::path(private$path, file_name, ext = "csv"), na = "", append = FALSE)
    invisible()
})

GDrive$set(which = "private", name = "snapshot", overwrite = TRUE, value = function(key, value) {
    stopifnot(is.data.frame(value))

    timestamp <- value$queried_at |> max() |> lubridate$floor_week()
    file_name <- paste0(key, "_", timestamp)

    readr::write_csv(value, fs::path(private$path, file_name, ext = "csv"), na = "", append = FALSE)
    invisible()
})
