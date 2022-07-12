# testthat ----------------------------------------------------------------
skip_localy <- function() {if (testthat:::on_ci()) {return(invisible(TRUE))} else { testthat::skip("On local machine") }}

expect_not_failure <- purrr::partial(testthat::expect_type, type = "environment")
expect_non_empty_data.frame <- function(object){
    expect_s3_class(object, "data.frame")
    expect_gt(nrow(object), 0)
}


# list operations ---------------------------------------------------------
select <- function(.x, ..., .strict = TRUE) {
    pos <- eval(parse(text = "tidyselect::eval_select(quote(c(...)), .x, strict = .strict)"))
    eval(parse(text = "rlang::set_names(.x[pos], names(pos))"))
}

