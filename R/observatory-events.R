#' @title System Events
#' @export
#' @examples
#' # events contains the following events:
#' ls(events)
events <- new.env()


# Queries -----------------------------------------------------------------
events$FailedToQueryRepo <- function(context) {
    invisible(glue("[\033[31mx\033[39m] Failed to retrieve `{context}` information"))
}

events$SucceededToQueryRepo <- function(context) {
    invisible(glue("[\033[32mv\033[39m] Retrieved `{context}` information"))
}
