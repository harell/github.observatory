#' @title Github Explorer GDrive Repository
#' @keywords internal
#' @export
#' @noRd
GDrive <- R6::R6Class(
    classname = "GDriveRepository", cloneable = FALSE, public = list(
        initialize = function(){
            private$path <- usethis::proj_path("_cache", "tables")
            fs::dir_create(private$path)
        },
        read_USER = function(){
            private$read(key = "USER")
        },
        overwrite_USER = function(value){
            private$overwrite(key = "USER", value)
            invisible(self)
        }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        path = ".",
        null_table = tibble::tibble(),
        # Private Methods ---------------------------------------------------------
        read = function(key) return(tibble::tibble()),
        overwrite = function(key, value) return(invisible())
    )
)


# Class Methods -----------------------------------------------------------
GDrive$set(which = "private", name = "read", overwrite = TRUE, value = function(key) {
    file <- fs::path(private$path, key, ext = ".csv")
    if(file.exists(file)){
        return(readr::read_csv(file, show_col_types = FALSE, progress = TRUE, lazy = FALSE))
    } else {
        return(private$null_table)
    }
})

GDrive$set(which = "private", name = "overwrite", overwrite = TRUE, value = function(key, value) {
    stopifnot(is.data.frame(value))
    readr::write_csv(value, fs::path(private$path, key, ext = ".csv"))
    invisible()
})
