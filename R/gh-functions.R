#' @title Github Query Functions
#' @export
gh <- new.env()


# User Queries ------------------------------------------------------------
gh$user <- new.env()

gh$user$get_starred <- function(user){
    output <- list()
    output[[1]] <- gh::gh(glue("/users/{user}/starred?per_page=100"))

    while(output |> purrr::pluck(dplyr::last) |> gh$utils$has_next())
        output[[length(output) + 1]] <- output |> purrr::pluck(dplyr::last) |> gh::gh_next()

    return(
        output
        |> purrr::flatten()
        |> purrr::map_chr(~purrr::pluck(.x, "full_name"))
    )
}


# Utility Functions -------------------------------------------------------
gh$utils <- new.env()

gh$utils$has_next <- eval(parse(text = "gh:::gh_has_next"))
