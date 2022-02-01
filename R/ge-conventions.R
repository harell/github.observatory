#' @name conventions
#' @rdname conventions
#'
#' @title Name Style Conventions
#'
NULL


#' @rdname conventions
#' @inheritParams dplyr::rename_with
#' @export
#' @examples
#' # Original column names
#' iris |> colnames()
#' # Standardised data column names
#' iris |> standardise_col_names() |> colnames()
standardise_col_names <- function(.data) {
    .fn <- snakecase::to_snake_case
    return(if(is.data.frame(.data)) {return(purrr::partial(dplyr::rename_with, .fn = .fn)(.data))} else {.fn(.data)})
}

