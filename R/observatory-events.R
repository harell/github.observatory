#' @title System Events
#' @export
#' @examples
#' # events contains the following events:
#' ls(events)
events <- new.env()


# Queries -----------------------------------------------------------------
events$FailedToQuery <- function(context) {
    glue("[\033[31mx\033[39m] Failed to retrieve `{context}` information")
}

events$SucceededToQuery <- function(context) {
    glue("[\033[32mv\033[39m] Retrieved `{context}` information")
}
