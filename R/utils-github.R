#' @title Github Query Functions
#' @export
github <- new.env()


# service -----------------------------------------------------------------
github$query <- function(endpoint){
    eval(parse(text = "httr::set_config(httr::timeout(60))"))
    output <- list()
    output[[1]] <- gh::gh(endpoint, .limit = 100)

    while(output |> purrr::pluck(dplyr::last) |> github$has_next())
        output[[length(output) + 1]] <- output |> purrr::pluck(dplyr::last) |> gh::gh_next()

    return(
        output
        |> purrr::flatten()
        |> as.list()
    )
}

github$has_next <- eval(parse(text = "gh:::gh_has_next"))

github$alter_PAT <- function(){
    invisible(
        PAT <- Sys.getenv()
        |> tibble::enframe()
        |> dplyr::mutate(dplyr::across(.fns = as.character))
        |> dplyr::filter(!name %in% "GITHUB_PAT")
        |> dplyr::filter(stringr::str_detect(name, "^GITHUB_PAT"))
        |> dplyr::pull(value)
    )

    if(length(PAT) >= 1){
        PAT_index <- which.max(PAT %in% Sys.getenv("GITHUB_PAT"))
        Sys.setenv(GITHUB_PAT = c(PAT, PAT)[PAT_index + 1])
    }

    return(invisible())
}


# predicates --------------------------------------------------------------
github$is_valid_url <- function(url) return(
    dplyr::if_else(is.na(url), "NA", url)
    |> stringr::str_detect("github.com/.*/.*")
)


# parsers -----------------------------------------------------------------
github$extract$owner <- function(url) return(
    url
    |> github$extract$slug()
    |> stringr::str_split("/")
    |> purrr::map_chr(~.x[1])
)

github$extract$repo <- function(url) return(
    url
    |> github$extract$slug()
    |> stringr::str_split("/")
    |> purrr::map_chr(~.x[2])
)

github$extract$root <- function(url) return(
    stringr::str_glue("https://github.com/{slug}", slug = github$extract$slug(url))
)

github$extract$slug <- function(url) return(
    url
    |> httr::parse_url()
    |> purrr::pluck("path")
    |> stringr::str_remove("^/")
    |> stringr::str_remove_all("/issues(|/)(|/new|new/)$")
)
github$extract$slug <- purrr::compose(unname, Vectorize(github$extract$slug))

github$compose$slug <- function(owner, repo) as.character(stringr::str_glue("{owner}/{repo}", owner = owner, repo = repo))

github$return_remaining_quote <- purrr::compose(~purrr::pluck(.x, "remaining"), gh::gh_rate_limit)