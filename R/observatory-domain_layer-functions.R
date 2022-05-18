#' @title Github Explorer Utility Functions
#' @export
observatory <- new.env()


# Conventions -------------------------------------------------------------
observatory$standardise$date <- function(x) { return(x |> as_date() |> lubridate::format_ISO8601()) }
observatory$standardise$name <- function(x) { return(x |> as.character() |> snakecase::to_title_case()) }
observatory$standardise$col_names <- function(.data) {
    .fn <- snakecase::to_snake_case
    return(if(is.data.frame(.data)) {return(purrr::partial(dplyr::rename_with, .fn = .fn)(.data))} else {.fn(.data)})
}


# Filters -----------------------------------------------------------------
observatory$discard$ghosts <- function(.data, var = login) dplyr::filter(.data, {{var}} %not_in% c("ghost"))
observatory$discard$robots <- function(.data, var = id) dplyr::filter(.data, {{var}} %not_in% c(16374903, 841039, 8518239, 34143537, 5877145, 14808551))


# Queries -----------------------------------------------------------------
## User Queries
observatory$query$user <- new.env()

observatory$query$user$by_id <- function(id) {
    return(
        glue("/user/{id}")
        |> github$query()
        |> purrr::list_merge(queried_at = lubridate::format_ISO8601(Sys.time()))
    )
}

observatory$query$user$by_login <- function(login) {
    return(
        glue("/users/{login}")
        |> github$query()
    )
}

observatory$query$user$following <- function(login) {
    return(
        glue("/users/{login}/following")
        |> github$query()
    )
}


## Package Queries
observatory$query$package <- new.env()

observatory$query$package$overview <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}")
        |> github$query()
        |> purrr::list_merge(queried_at = lubridate::format_ISO8601(Sys.time()))
    )
}

observatory$query$package$stargazers <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}/stargazers")
        |> github$query()
    )
}


observatory$query$package$contributors <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}/contributors")
        |> github$query()
    )
}

observatory$query$package$watchers <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}/subscribers")
        |> github$query()
    )
}

observatory$query$package$forkers <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}/forks")
        |> github$query()
    )
}


