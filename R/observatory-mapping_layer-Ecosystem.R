#' @title Github Explorer Repository
#' @param value (`data.frame`) Data to store in the Repository.
#' @family R Ecosystem classes
#' @export
Ecosystem <- R6::R6Class(
    classname = "Repository", lock_objects = FALSE, cloneable = FALSE, public = list(
        #' @description Instantiate a Repository object
        #' @param local_path (`character`) A local dir path where files will be stored.
        #' @param remote_path (`character`) A remote dir path on AWS S3 where files will be stored.
        initialize = function(
        local_path = fs::path_wd("_cache", "tables"),
        remote_path = "s3://tidylab/github.observatory/tables/"
        ){
            private$remote_path <- as.character(remote_path)
            private$local_path <- as.character(local_path)
            fs::dir_create(private$local_path)

            invisible(self)
        },
        #' @description Read the Github information of R Users.
        read_USER = function() { return(private$read("USER")) },
        #' @description Read the Github information of R Packages.
        read_REPO = function() { return(private$read("REPO")) },
        #' @description Read the CRAN information of R Packages.
        read_PACKAGE = function() { return(private$read("PACKAGE")) },
        #' @description Read the CRAN package dependencies of R Packages.
        read_DEPENDENCY = function() { return(private$read("DEPENDENCY")) },
        #' @description Read who is following who in the R zoo.
        read_FOLLOWING = function() { return(private$read("FOLLOWING")) },
        #' @description Read R packages "contributors", "stargazers", and "watchers".
        read_SPECTATOR = function() { return(private$read("SPECTATOR")) },
        #' @description Overwrite the Github information of R Users.
        overwrite_USER = function(value) { private$overwrite("USER", value); invisible(self) },
        #' @description Overwrite the Github information of R Packages.
        overwrite_REPO = function(value) { private$overwrite("REPO", value); invisible(self) },
        #' @description Overwrite the CRAN information of R Packages.
        overwrite_PACKAGE = function(value) { private$overwrite("PACKAGE", value); invisible(self) },
        #' @description Overwrite the CRAN package dependencies of R Packages.
        overwrite_DEPENDENCY = function(value) { private$overwrite("DEPENDENCY", value); invisible(self) },
        #' @description Overwrite who is following who in the R zoo.
        overwrite_FOLLOWING = function(value) { private$overwrite("FOLLOWING", value); invisible(self) },
        #' @description Overwrite R packages "contributors", "stargazers", and "watchers".
        overwrite_SPECTATOR = function(value) { private$overwrite("SPECTATOR", value); invisible(self) }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        local_path = ".",
        remote_path = ".",
        null_table = tibble::tibble(),
        # Private Methods ---------------------------------------------------------
        read_csv = function(...) { stop() },
        write_csv = function(...) { stop() },
        read = function(key) { stop() },
        overwrite = function(key, value) { stop() },
        snapshot = function(key) { stop() }
    )
)


# Remote Data Storage -----------------------------------------------------
Ecosystem$set(which = "private", name = "read", overwrite = TRUE, value = function(key) {
    # Setup
    s3 <- S3::S3$new(access_control_list = c("public-read", "private")[2])
    remote_path <- private$remote_path
    local_path <- private$local_path

    # Name File
    file_name <- fs::path(key, ext = "csv.bz2")
    remote_file <- s3$path(remote_path, file_name)
    local_file <- fs::path(local_path, file_name)

    # Sync file
    are_files_synced <- isTRUE(as.integer(s3$file_size(remote_file)) == as.integer(fs::file_size(local_file)))
    if(isFALSE(are_files_synced)) s3$file_copy(remote_file, local_path, overwrite = TRUE)

    # Read file
    tbl <- private$read_csv(bzfile(local_file))
    return(tbl)
})

Ecosystem$set(which = "private", name = "overwrite", overwrite = TRUE, value = function(key, value) {
    assert_that("data.frame" %in% class(value))

    # Setup
    s3 <- S3::S3$new(access_control_list = c("public-read", "private")[2])
    remote_path <- private$remote_path
    local_path <- private$local_path

    # Name File
    file_name <- fs::path(key, ext = "csv.bz2")
    remote_file <- s3$path(remote_path, file_name)
    local_file <- fs::path(local_path, file_name)

    # Compress and write file locally
    private$write_csv(value, bzfile(local_file))

    # Sync file
    are_files_synced <- isTRUE(as.integer(s3$file_size(remote_file)) == as.integer(fs::file_size(local_file)))
    if(isTRUE(are_files_synced)) return()

    # Upload file
    s3$file_copy(local_file, remote_path, overwrite = TRUE)
    return()
})


# Low-level Methods -------------------------------------------------------
Ecosystem$set(which = "private", name = "read_csv", overwrite = TRUE, value = function(...) purrr::partial(readr::read_csv, show_col_types = FALSE, progress = FALSE, lazy = FALSE)(...))
Ecosystem$set(which = "private", name = "write_csv", overwrite = TRUE, value = function(...) purrr::partial(readr::write_csv, na = "", append = FALSE)(...))
