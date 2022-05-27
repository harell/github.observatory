#' @title Query Queue
#' @keywords internal
#' @export
#' @noRd
QueryQueue <- R6::R6Class(
    cloneable = FALSE, public = list(
        initialize = function(path = fs::path_wd("_cache", "tables")){
            private$eco <- Ecosystem$new(path)
            private$repo_db <- RepoQueryDB$new()
            private$user_db <- UserQueryDB$new()
        }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        eco = new.env(),
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
    pkgs_on_cran <- private$eco$read_PACKAGE()

    invisible(
        pkgs_in_cache <- private$repo_db$load()
        |> dplyr::filter(entity %in% "repo", type %in% "overview")
        |> dplyr::distinct(alias)
        |> dplyr::pull(alias)
    )
    new_pkgs <- setdiff(pkgs_on_cran$package, pkgs_in_cache)

    set.seed(2107)
    if(length(new_pkgs) > 1) new_pkgs <- sample(new_pkgs)
    if(github$is_on_ci()) new_pkgs <- head(new_pkgs, 1000)

    collections::priority_queue(
        items = new_pkgs,
        priorities = 2
    )
})


QueryQueue$set(which = "private", name = "generate_USER_queue", overwrite = TRUE, value = function() {
    ecosystem_users <- tryCatch(invisible(
        private$repo_db$load()
        |> dplyr::filter(type %in% c("contributors", "stargazers"))
        |> dplyr::pull(data)
        |> purrr::map_dfr(jsonlite::fromJSON)
        |> observatory$discard$robots()
        |> dplyr::pull(id)
        |> as.integer()
        |> unique()
    ), error = function(e) return(character(0)))

    existing_users <- private$user_db$load()$id |> unique() |> as.integer()

    new_users <- setdiff(ecosystem_users, existing_users)

    set.seed(2107)
    if(length(new_users) > 1) new_users <- sample(new_users)
    if(github$is_on_ci()) new_users <- head(new_users, 1000)

    collections::priority_queue(
        items = new_users,
        priorities = 2
    )
})
