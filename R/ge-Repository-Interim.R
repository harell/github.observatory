#' @title Github Explorer InterimRepository
#' @keywords internal
#' @export
#' @noRd
InterimRepository <- R6::R6Class(
    classname = "InterimRepository", cloneable = FALSE, public = list(
        initialize = function(){
            ## Configurations
            private$paths <- list(package = list())
            private$paths$package$cran_desc <- usethis::proj_path("_cache", "cran_desc", ext = "csv")
            private$paths$package$repo_desc <- usethis::proj_path("_cache", "repo_desc", ext = "rds")

            ## Set Cache
            private$cache$package <- list()
            private$cache$user <- list()
        },
        #' @description Store changes
        commit = function(){
            obj_names <- c("cran_desc", "repo_desc")

            for(obj_name in obj_names){
                x <- private$cache$package[[obj_name]]
                if(is.null(x)) next
                file <- private$paths$package[[obj_name]]
                private$write_obj(x, file)
            }

            message("Commited changes to database")
            invisible(self)
        },
        unroll = function(){
            private$cache <- new.env()
            message("Unrolled changes to database")
            invisible(self)
        },
        write_cran_desc = function(x){
            private$cache$package$cran_desc <- x
            invisible(self)
        },
        write_repo_desc = function(x){
            assert$has_columns(x, colnames(private$null_repo_desc))
            private$cache$package$repo_desc <- dplyr::select(x, colnames(private$null_repo_desc))
            invisible(self)
        },
        read_cran_desc = function(){
            if(is.null(private$cache$package$cran_desc)) private$cache$package$cran_desc <- private$read_obj(private$paths$package$cran_desc)
            return(private$cache$package$cran_desc)
        },
        read_repo_desc = function(){
            file_exists <- file.exists(private$paths$package$repo_desc)
            file_loaded <- !is.null(private$cache$package$repo_desc)
            if(!file_exists & !file_loaded) self$create_repo_desc() else if(file_exists & !file_loaded) private$cache$package$repo_desc <- private$read_obj(private$paths$package$repo_desc)
            return(private$cache$package$repo_desc)
        },
        create_repo_desc = function() suppressMessages(
            self$read_cran_desc()
            |> dplyr::transmute(owner = github$extract$owner(github_slug), repo = github$extract$repo(github_slug))
            |> dplyr::full_join(private$null_repo_desc)
            |> self$write_repo_desc()
        )
    ), private = list(
        null_repo_desc = tibble::tibble(
            owner = NA_character_,
            repo = NA_character_,
            id = NA_integer_,
            stargazers_id = list(NULL),
            stargazers_login = list(NULL)
        )[0,],
        # Private Methods ---------------------------------------------------------
        write_obj = function(x, file){
            fs::dir_create(dirname(file), FALSE, TRUE)

            switch(tolower(fs::path_ext(file)),
                   csv = readr::write_csv(x, file, na = ""),
                   rds = readr::write_rds(x, file)
            )

            message("Saved ", basename(file))
        },
        read_obj = function(file){
            if(file.exists(file)) {
                x <- switch(
                    tolower(fs::path_ext(file)),
                    csv = readr::read_csv(file, show_col_types = FALSE, lazy = FALSE),
                    rds = readr::read_rds(file)
                )
            } else {
                x <- tibble::tibble()
            }

            message("Loaded ", basename(file))
            return(x)
        },
        # Private Fields ----------------------------------------------------------
        cache = new.env(),
        paths = list()
    )
)
