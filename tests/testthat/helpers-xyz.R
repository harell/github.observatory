# utilities ---------------------------------------------------------------
line_break <- function() paste0("\n", paste0(rep("#", 80), collapse = ""))
banner <- function(title) paste0(line_break(), paste0("\n## ", title), line_break(), "\n", collapse = "")
read_lines <- function(path) paste(readLines(path), collapse = "\n")


# testthat ----------------------------------------------------------------
expect_not_failure <- purrr::partial(testthat::expect_type, type = "environment")
skip_localy <- purrr::partial(testthat::skip_on_os, os = "windows")
