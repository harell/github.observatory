#' @title Query Queue
#' @keywords internal
#' @export
#' @noRd
QueryQueue <- R6::R6Class(
    cloneable = FALSE, public = list(
        initialize = function(path = usethis::proj_path("_cache", "tables")){
            private$depo <- Depository$new(path)
            private$repo_archive <- RepoArchive$new()
            private$user_archive <- UserArchive$new()
        }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        depo = new.env(),
        repo_archive = new.env(),
        user_archive = new.env(),
        null_query = c(),
        # Private Methods ---------------------------------------------------------
        generate_REPO_queue = function() collections::priority_queue(),
        generate_USER_queue = function() collections::priority_queue()
    ), active = list(
        REPO = function() private$generate_REPO_queue(),
        USER = function() private$generate_USER_queue()
    )
)


# Class Methods -----------------------------------------------------------
QueryQueue$set(which = "private", name = "generate_REPO_queue", overwrite = TRUE, value = function() {
    pkgs_on_cran <- private$depo$read_PACKAGE()

    pkgs_in_cache <- tryCatch(
        private$repo_archive$show()
        |> dplyr::filter(type %in% "overview")
        |> dplyr::pull(artifact)
        |> private$repo_archive$load()
        |> purrr::map_chr(~purrr::pluck(.x, "package"))
        |> unique()
        , error = function(e) return(character())
    )

    pkgs_to_query <- setdiff(pkgs_on_cran$package, pkgs_in_cache)

    collections::priority_queue(
        items = pkgs_to_query,
        priorities = 2
    )
})
