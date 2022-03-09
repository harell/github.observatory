#' @title Github Explorer Utility Functions
#' @export
ge <- new.env()


# conventions -------------------------------------------------------------
ge$standardise$col_names <- function(.data) {
    .fn <- snakecase::to_snake_case
    return(if(is.data.frame(.data)) {return(purrr::partial(dplyr::rename_with, .fn = .fn)(.data))} else {.fn(.data)})
}

ge$standardise$date <- function(x) { return(x |> as.Date() |> lubridate::format_ISO8601()) }

ge$standardise$name <- function(x) { return(x |> as.character() |> snakecase::to_title_case()) }


# filters -----------------------------------------------------------------
ge$discard$ghosts <- function(.data, var = "login") dplyr::filter(.data, {{var}} %not_in% c("ghost"))
ge$discard$robots <- function(.data, var = "id") dplyr::filter(.data, {{var}} %not_in% c(16374903, 841039,))
