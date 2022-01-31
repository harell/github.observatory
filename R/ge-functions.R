#' @title Github Explorer Functions
#' @export
ge <- new.env()


# Utility Functions -------------------------------------------------------
ge$utils <- new.env()

ge$github$is_valid_url <- function(url) return(
    dplyr::if_else(is.na(url), "NA", url)
    |> stringr::str_detect("github.com/.*/.*/.*")
)

ge$github$parse_slug <- function(url) return(
    url
    |> httr::parse_url()
    |> purrr::pluck("path")
    |> stringr::str_remove("^/")
    |> stringr::str_remove_all("/issues(|/)(|/new|new/)$")
)
