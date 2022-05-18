#' @title Query Queue
#' @keywords internal
#' @export
#' @noRd
QueryQueue <- R6::R6Class(
    cloneable = FALSE, public = list(
        initialize = function(path = usethis::proj_path("_cache", "tables")){
            private$depo <- Depository$new(path)
            private$repo_db <- RepoQueryDB$new()
            private$user_db <- UserQueryDB$new()
        }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        depo = new.env(),
        repo_db = new.env(),
        user_db = new.env(),
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

    invisible(
        pkgs_in_cache <- private$repo_db$load()
        |> dplyr::filter(entity %in% "repo", type %in% "overview")
        |> dplyr::distinct(alias)
        |> dplyr::pull(alias)
    )
    new_pkgs <- setdiff(pkgs_on_cran$package, pkgs_in_cache) |> sample()

    collections::priority_queue(
        items = new_pkgs,
        priorities = 2
    )
})


QueryQueue$set(which = "private", name = "generate_USER_queue", overwrite = TRUE, value = function() {
    ecosystem_users <- tryCatch(invisible(
        private$repo_db$load()
        |> dplyr::filter(data %not_in% "[]")
        |> dplyr::filter(type %in% c("contributors", "stargazers"))
        |> dplyr::pull(data)
        |> purrr::map_dfr(jsonlite::fromJSON)
        |> observatory$discard$robots()
        |> dplyr::pull(id)
        |> as.integer()
        |> unique()
    ), error = function(e) return(character(0)))

    existing_users <- unique(private$user_db$load()$id)

    new_users <- setdiff(ecosystem_users, existing_users) |> sample()

    collections::priority_queue(
        items = new_users,
        priorities = 2
    )
})
