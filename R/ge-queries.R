#' @title Github Explorer Query Functions
#' @export
query <- new.env()


# User Queries ------------------------------------------------------------
query$user <- new.env()

query$user$by_id <- function(id) {
    return(
        glue("/user/{id}")
        |> github$query()
        |> purrr::list_merge(queried_at = lubridate::format_ISO8601(Sys.time()))
    )
}

query$user$by_login <- function(login) {
    return(
        glue("/users/{login}")
        |> github$query()
    )
}

query$user$following <- function(login) {
    return(
        glue("/users/{login}/following")
        |> github$query()
    )
}


# Package Queries ---------------------------------------------------------
query$package <- new.env()

query$package$overview <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}")
        |> github$query()
        |> purrr::list_merge(queried_at = lubridate::format_ISO8601(Sys.time()))
    )
}

query$package$stargazers <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}/stargazers")
        |> github$query()
    )
}


query$package$contributors <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}/contributors")
        |> github$query()
    )
}

query$package$watchers <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}/subscribers")
        |> github$query()
    )
}

query$package$forkers <- function(owner, repo) {
    return(
        glue("/repos/{owner}/{repo}/forks")
        |> github$query()
    )
}
