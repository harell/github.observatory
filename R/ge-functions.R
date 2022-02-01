#' @title Github Explorer Functions
#' @export
ge <- new.env()


# Utility Functions -------------------------------------------------------
ge$compose_cran_slug <- function(package) as.character(stringr::str_glue("https://github.com/cran/{package}/issues", package = package))

