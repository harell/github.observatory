#' @title Github Explorer Depository
#' @param filter (`character`) either 'latest' or 'everything'. If 'latest' then
#'   return the latest query of each entity. Otherwise, return all all records
#'   (might include multiple records per entity).
#' @keywords internal
#' @export
#' @noRd
Depository <- R6::R6Class(
    classname = "Repository", lock_objects = FALSE, cloneable = FALSE, public = list(
        initialize = function(
        local_path = fs::path_wd("_cache", "tables"),
        remote_path = "s3://tidylab/github.observatory/tables/"
        ){
            private$remote_path <- as.character(remote_path)
            private$local_path <- as.character(local_path)
            fs::dir_create(private$local_path)

            invisible(self)
        },
        read_USER      = function() { return(private$read_remotely("USER")) },
        read_REPO      = function() { return(private$read_remotely("REPO")) },
        read_PACKAGE   = function() { return(private$read_remotely("PACKAGE")) },
        read_FOLLOWING = function() { return(private$read_remotely("FOLLOWING")) },
        read_SPECTATOR = function() { return(private$read_remotely("SPECTATOR")) },
        overwrite_USER = function(value) { private$overwrite_remotely("USER", value); invisible(self) },
        overwrite_REPO = function(value) { private$overwrite_remotely("REPO", value); invisible(self) },
        overwrite_PACKAGE = function(value) { private$overwrite_remotely("PACKAGE", value); invisible(self) },
        overwrite_FOLLOWING = function(value) { private$overwrite_remotely("FOLLOWING", value); invisible(self) },
        overwrite_SPECTATOR = function(value) { private$overwrite_remotely("SPECTATOR", value); invisible(self) }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        local_path = ".",
        remote_path = ".",
        null_table = tibble::tibble(),
        # Private Methods ---------------------------------------------------------
        sync_csv = function(...) { stop() },
        read_csv = function(...) { stop() },
        write_csv = function(...) { stop() },
        read_remotely = function(key) { stop() },
        overwrite_remotely = function(key, value) { stop() },
        snapshot = function(key) { stop() }
    )
)


# Remote Data Storage -----------------------------------------------------
Depository$set(which = "private", name = "read_remotely", overwrite = TRUE, value = function(key) {
    # Setup
    s3 <- S3::S3$new(access_control_list = c("public-read", "private")[2])
    remote_path <- private$remote_path
    local_path <- private$local_path

    # Name File
    file_name <- fs::path(key, ext = "csv.bz2")
    remote_file <- s3$path(remote_path, file_name)
    local_file <- fs::path(local_path, file_name)

    # Check Sync
    remote_file_meta <- s3$file_size(remote_file)
    local_file_meta <- fs::file_size(local_file)
    are_files_synced <- all.equal(s3$file_size(remote_file), fs::file_size(local_file), check.attributes = FALSE)

    # Read file
    if(isFALSE(s3$file_exists(remote_file))){
        tbl <- private$null_table
    } else if (isTRUE(are_files_synced)){
        tbl <- private$read_csv(bzfile(local_file))
    } else {
        s3$file_copy(remote_file, local_path, overwrite = TRUE)
        tbl <- private$read_csv(bzfile(local_file))
    }

    # Return
    return(tbl)
})

Depository$set(which = "private", name = "overwrite_remotely", overwrite = TRUE, value = function(key, value) {
    stopifnot(is.data.frame(value))

    # Setup
    s3 <- S3::S3$new(access_control_list = c("public-read", "private")[2])
    remote_path <- private$remote_path

    # Name File
    # last_processed <- value$processed_at|> as_date() |> max() |> lubridate::floor_date(unit = "1 week")
    last_processed <- NULL
    file_name <- fs::path(glue::glue_collapse(c(key, last_processed), sep = "_"), ext = "csv.bz2")

    # Compress Data
    local_file <- fs::path_temp(file_name) |> as.character()
    private$write_csv(value, bzfile(local_file))

    # Upload file
    s3$file_copy(local_file, remote_path, overwrite = TRUE)

    # Return
    rm(local_file)
    invisible()
})


# Low-level Methods -------------------------------------------------------
Depository$set(which = "private", name = "read_csv", overwrite = TRUE, value = function(...) purrr::partial(readr::read_csv, show_col_types = FALSE, progress = FALSE, lazy = FALSE)(...))
Depository$set(which = "private", name = "write_csv", overwrite = TRUE, value = function(...) purrr::partial(readr::write_csv, na = "", append = FALSE)(...))
Depository$set(which = "private", name = "sync_csv", overwrite = TRUE, value = function(source, target) {

})

