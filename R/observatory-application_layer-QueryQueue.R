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
    # pkgload::load_all(usethis::proj_get(), quiet = TRUE)
    # repo_archive <- RepoArchive$new()
    # user_archive <- UserArchive$new()
    #
    # artifacts <- repo_archive$show()
    #
    # if(nrow(artifacts) > 0) {
    #     invisible(
    #         artifacts <- artifacts
    #         |> dplyr::filter(type %in% c("contributors", "forkers", "stargazers", "watchers")[c(1,3)])
    #         |> dplyr::arrange(dplyr::desc(date))
    #         |> dplyr::distinct(id, type, .keep_all = TRUE)
    #     )
    #
    #     invisible(
    #         artifacts$data <- artifacts$artifact
    #         |> purrr::map(~.x |> repo_archive$load() |> unlist())
    #         |> purrr::map(~as.integer(.x[names(.x) == "id"]))
    #     )
    #
    #     invisible(
    #         all_users <- artifacts
    #         |> dplyr::pull(data)
    #         |> purrr::flatten_int()
    #         |> unique()
    #     )
    #
    #     existing_users <- tryCatch(
    #         user_archive$show()
    #         |> dplyr::filter(entity %in% "user")
    #         |> dplyr::pull("id")
    #         |> as.integer(),
    #         error = function(e) return(0)
    #     )
    #
    #     new_users <- setdiff(all_users, existing_users)
    #
    # } else {
    #
    #     new_users <- integer(0)
    # }

    new_users <- "280924484"
    collections::priority_queue(
        items = new_users,
        priorities = 2
    )
})
