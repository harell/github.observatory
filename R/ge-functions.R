#' @title Github Explorer Functions
#' @export
ge <- new.env()


# Utility Functions -------------------------------------------------------
ge$utils <- new.env()

ge$github$extract_owner <- function(url) return(
    url
    |> ge$github$parse_slug()
    |> stringr::str_split("/")
    |> purrr::pluck(1, 1)
)

ge$github$extract_repo <- function(url) return(
    url
    |> ge$github$parse_slug()
    |> stringr::str_split("/")
    |> purrr::pluck(1, 2)
)

ge$github$extract_root <- function(url) return(
    stringr::str_glue("https://github.com/{slug}", slug = ge$github$parse_slug(url))
)

ge$github$parse_slug <- function(url) return(
    url
    |> httr::parse_url()
    |> purrr::pluck("path")
    |> stringr::str_remove("^/")
    |> stringr::str_remove_all("/issues(|/)(|/new|new/)$")
)

ge$github$compose_cran_slug <- function(package) as.character(stringr::str_glue("https://github.com/cran/{package}/issues", package = package))

ge$github$rate_limit <- gh::gh_rate_limit

ge$github$is_valid_url <- function(url) return(
    dplyr::if_else(is.na(url), "NA", url)
    |> stringr::str_detect("github.com/.*/.*/.*")
)
