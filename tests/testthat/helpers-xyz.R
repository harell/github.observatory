# testthat ----------------------------------------------------------------
expect_not_failure <- purrr::partial(testthat::expect_type, type = "environment")
skip_localy <- function() {if (testthat:::on_ci()) {return(invisible(TRUE))} else { testthat::skip("On local machine") }}


# list operations ---------------------------------------------------------
select <- function(.x, ..., .strict = TRUE) {
    pos <- eval(parse(text = "tidyselect::eval_select(quote(c(...)), .x, strict = .strict)"))
    eval(parse(text = "rlang::set_names(.x[pos], names(pos))"))
}

