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
        overwrite_USER = function(value) { private$overwrite("USER", value); invisible(self) },
        overwrite_REPO = function(value) { private$overwrite("REPO", value); invisible(self) },
        overwrite_PACKAGE = function(value) { private$overwrite("PACKAGE", value); invisible(self) },
        overwrite_FOLLOWING = function(value) { private$overwrite("FOLLOWING", value); invisible(self) },
        overwrite_SPECTATOR = function(value) { private$overwrite("SPECTATOR", value); invisible(self) }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        path = ".",
        null_table = tibble::tibble(),
        # Private Methods ---------------------------------------------------------
        read = function(key) { return(tibble::tibble()) },
        overwrite = function(key, value) { return(invisible()) }
    )
)


# Class Methods -----------------------------------------------------------
GDrive$set(which = "private", name = "read", overwrite = TRUE, value = function(key, filter) {
    filter <- match.arg(tolower(filter), c("latest", "everything"))
    file <- fs::path(private$path, key, ext = "csv")

    if(file_not_exists(file)) return(private$null_table)

    invisible(
        data <- readr::read_csv(file, show_col_types = FALSE, progress = TRUE, lazy = FALSE)
        |> purrr::modify_if(lubridate::is.Date, ge$standardise$date)
        |> dplyr::distinct()
    )

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

    return(
        value
        |> dplyr::distinct()
        |> readr::write_csv(fs::path(private$path, key, ext = "csv"), append = FALSE)
    )
})
