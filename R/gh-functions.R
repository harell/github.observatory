#' @title Github Query Functions
#' @export
gh <- new.env()


# User Queries ------------------------------------------------------------
gh$user <- new.env()

gh$user$get_starred <- function(user){
    output <- gh$utils$gh(glue("/users/{user}/starred"))
    return(purrr::map_chr(output, ~purrr::pluck(.x, "full_name")))
}


# Package Queries ---------------------------------------------------------
gh$package <- new.env()

gh$package$get_overview <- function(owner, repo){
    gh::gh(glue("/repos/{owner}/{repo}"), .limit = 100)
}

gh$package$get_stargazers <- function(owner, repo){
    gh$utils$gh(glue("/repos/{owner}/{repo}/stargazers"))
}


# Utility Functions -------------------------------------------------------
gh$utils <- new.env()

gh$utils$gh <- function(endpoint){
    output <- list()
    output[[1]] <- gh::gh(endpoint, .limit = 100)

    while(output |> purrr::pluck(dplyr::last) |> gh$utils$has_next())
        output[[length(output) + 1]] <- output |> purrr::pluck(dplyr::last) |> gh::gh_next()

    return(
        output
        |> purrr::flatten()
        |> as.list()
    )
}

gh$utils$has_next <- eval(parse(text = "gh:::gh_has_next"))
