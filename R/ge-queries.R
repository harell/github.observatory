#' @title Github Query Functions
#' @export
query <- new.env()


# User Queries ------------------------------------------------------------
query$user <- new.env()

query$user$starred <- function(user){
    output <- github$query(glue("/users/{user}/starred"))
    return(purrr::map_chr(output, ~purrr::pluck(.x, "full_name")))
}


# Package Queries ---------------------------------------------------------
query$package <- new.env()

query$package$overview <- function(owner, repo){
    github$query(glue("/repos/{owner}/{repo}"))
}

query$package$stargazers <- function(owner, repo){
    github$query(glue("/repos/{owner}/{repo}/stargazers"))
}
