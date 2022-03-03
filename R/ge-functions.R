#' @title Github Explorer Utility Functions
#' @export
ge <- new.env()


# conventions -------------------------------------------------------------
ge$standardise$col_names <- function(.data) {
    .fn <- snakecase::to_snake_case
    return(if(is.data.frame(.data)) {return(purrr::partial(dplyr::rename_with, .fn = .fn)(.data))} else {.fn(.data)})
}


# filters -----------------------------------------------------------------
ge$filter$ghosts <- function(.data, var = "login") dplyr::filter(.data, {{var}} %not_in% c("ghost"))
