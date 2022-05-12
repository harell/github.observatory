#' @title Github Explorer Depository
#' @param filter (`character`) either 'latest' or 'everything'. If 'latest' then
#'   return the latest query of each entity. Otherwise, return all all records
#'   (might include multiple records per entity).
#' @keywords internal
#' @export
#' @noRd
Depository <- R6::R6Class(
    classname = "Repository", cloneable = FALSE, public = list(
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
        snapshot_USER = function() { private$snapshot("USER"); invisible(self) },
        snapshot_REPO = function() { private$snapshot("REPO"); invisible(self) }
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
    # Setup
    s3 <- S3::S3$new(access_control_list = c("public-read", "private")[2])
    uri <- "s3://tidylab/github.observatory/"
    snapshot_uri <- s3$path(uri, "snapshots")

    # Retrieve Data
    value <- self[[paste0("read_", key)]]()
    if(nrow(value) == 0) return()

    # Name Snapshot
    last_processed <- value$processed_at|> as_date() |> max() |> lubridate::floor_date(unit = "1 week")
    file_name <- fs::path(paste0(key, "_", last_processed), ext = "csv.bz2")

    # Compress Data
    local_file <- fs::path_temp(file_name) |> as.character()
    readr::write_csv(value, bzfile(local_file))

    # Upload file
    s3$file_copy(local_file, snapshot_uri, overwrite = TRUE)

    # Return
    rm(local_file)
    invisible()
})
