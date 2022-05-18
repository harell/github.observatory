#' @name utils-base
#' @rdname utils-base
#'
#' @title Base Functions
#'
NULL


#' @rdname utils-base
#' @export
does_not_exist <- purrr::negate(base::exists)


#' @rdname utils-base
#' @export
file_not_exists <- purrr::negate(base::file.exists)


#' @rdname utils-base
#' @export
`%not_in%` <- purrr::negate(`%in%`)
