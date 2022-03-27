# Assertions --------------------------------------------------------------
#' @title Assertions Set
#' @export
#' @examples
#' # assert contains the following assertions:
#' ls(assert)
assert <- new.env()
assert_that <- function(..., env = parent.frame(), msg = NULL) invisible(assertthat::assert_that(..., env = env, msg = msg))
assert$has_columns <- function(object, col_names) assert_that(assertthat::has_name(object, col_names))
assert$is_unique <- function(object) assert_that(length(object) == length(unique(object)), msg = paste0("The following values are not unique: ", paste(unique(object[duplicated(object)]), collapse = ", ")))

# General -----------------------------------------------------------------
assert$is_class <- function(object, class) assert_that(class %in% base::class(object), msg = paste("object is not of type", class))
assert$is_data.frame <- function(object) assert$is_class(object, "data.frame")
assert$is_not_empty_data.frame <- function(object) {assert$is_data.frame(object); assert_that(nrow(object) > 0, msg = "data.frame is empty")}
assert$is_one_of <- function(x, y) assert_that(all(x %in% y), msg = paste("x should be one of", paste(y, collapse = ", ")))


# Objects -----------------------------------------------------------------
assert$is_tsibble <- function(object) assert_that(tsibble::is_tsibble(object))


# Value Objects -----------------------------------------------------------
