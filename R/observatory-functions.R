#' @title Github Explorer Utility Functions
#' @export
observatory <- new.env()


# conventions -------------------------------------------------------------
observatory$standardise$date <- function(x) { return(x |> as.Date() |> lubridate::format_ISO8601()) }
observatory$standardise$name <- function(x) { return(x |> as.character() |> snakecase::to_title_case()) }
observatory$standardise$col_names <- function(.data) {
    .fn <- snakecase::to_snake_case
    return(if(is.data.frame(.data)) {return(purrr::partial(dplyr::rename_with, .fn = .fn)(.data))} else {.fn(.data)})
}


# filters -----------------------------------------------------------------
observatory$discard$ghosts <- function(.data, var = "login") dplyr::filter(.data, {{var}} %not_in% c("ghost"))
observatory$discard$robots <- function(.data, var = "id") dplyr::filter(.data, {{var}} %not_in% c(16374903, 841039, 8518239, 34143537, 5877145, 14808551))
