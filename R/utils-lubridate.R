#' @title lubridate Extensions
#' @export
lubridate <- new.env()

lubridate$floor_week <- purrr::partial(lubridate::floor_date, unit = "1 week")
