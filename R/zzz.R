.onAttach <- function(lib, pkg,...){#nocov start
    if(require("future", quietly = TRUE)) future::plan(multisession, workers = 10) else future <- base::identity
    # future::plan(sequential)

}#nocov end
