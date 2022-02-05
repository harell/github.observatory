#' @title Github Explorer Repository
#' @keywords internal
#' @export
#' @noRd
Repository <- R6::R6Class(
    classname = "Repository", cloneable = FALSE, public = list(
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
            private$cache$package$repo_desc <- x
            invisible(self)
        },
        read_cran_desc = function(){
            if(is.null(private$cache$package$cran_desc)) private$cache$package$cran_desc <- private$read_obj(private$paths$package$cran_desc)
            return(private$cache$package$cran_desc)
        },
        read_repo_desc = function(){
            file_exists <- file.exists(private$paths$package$repo_desc)
            file_loaded <- !is.null(private$cache$package$repo_desc)
            if(!file_exists & !file_loaded) self$create_repo_desc() else if(file_exists) private$cache$package$repo_desc <- private$read_obj(private$paths$package$repo_desc)
            return(private$cache$package$repo_desc)
        },
        create_repo_desc = function() suppressMessages(
            self$read_cran_desc()
            |> dplyr::transmute(owner = github$extract$owner(github_slug), repo = github$extract$repo(github_slug))
            |> tibble::add_column(stargazers = list(NULL))
            |> self$write_repo_desc()
        )
    ), private = list(
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

#' @title Github Explorer Archive
#' @keywords internal
#' @export
#' @noRd
Archive <- R6::R6Class(
    classname = "Repository", cloneable = FALSE, public = list(
        # Public Methods ----------------------------------------------------------
        #' @param path (`character`) A character denoting an existing directory of the Repository for which metadata will be aved and returned.
        initialize = function(path = usethis::proj_path("_cache")){
            private$path <- path
            suppressMessages(archivist::createLocalRepo(path))
        },
        save = function(artifact, tags = character()){
            tags <- c(paste0("date:", lubridate::format_ISO8601(Sys.Date())), tags)
            archivist::saveToLocalRepo(artifact = artifact, repoDir = private$path, archiveTags = FALSE, archiveMiniature = FALSE, archiveSessionInfo = FALSE, force = TRUE, userTags = tags)
            invisible(self)
        },
        load = function(md5hash){
            purrr::map(md5hash, archivist::loadFromLocalRepo, repoDir = private$path, value = TRUE)
        },
        show = function() tryCatch((
            archivist::splitTagsLocal(repoDir = private$path)
            |> dplyr::select(-createdDate)
            |> tidyr::pivot_wider(id_cols = artifact, names_from = tagKey, values_from = tagValue, values_fn = list)
            |> tidyr::unnest(cols = -artifact)
            |> dplyr::select(-format)
            |> dplyr::distinct()
            |> dplyr::mutate(date = as.Date(date))
            |> dplyr::arrange(dplyr::across(-artifact))
        ), error = function(e) return(
            tibble::tibble(artifact = NA_character_, date = Sys.Date())
            |> tidyr::drop_na()
        )),
        clean = function(){
            N1 <- nrow(self$show())
            private$discard_duplicates()
            private$discard_outdated()
            N2 <- nrow(self$show())
            if(N1>N2) message("Discarded ", N1-N2, " items")
            invisible(self)
        },
        finalize = function(){self$clean(); invisible(self)}
    ), private = list(
        # Private Fields ----------------------------------------------------------
        path = character(),
        # Private Methods ---------------------------------------------------------
        discard_duplicates = function(){
            invisible(
                keep_artifact <- self$show()
                |> dplyr::group_by(dplyr::across(-artifact))
                |> dplyr::summarise(dplyr::across(artifact, dplyr::first), .groups = "drop")
                |> dplyr::pull(artifact)
            )
            discard_artifact <- dplyr::setdiff(dplyr::pull(self$show(), artifact), keep_artifact)
            archivist::rmFromLocalRepo(discard_artifact, repoDir = private$path, removeData = TRUE, removeMiniature = TRUE, many = TRUE)
        },
        discard_outdated = function(){
            invisible(
                keep_artifact <- self$show()
                |> dplyr::arrange(dplyr::desc(date))
                |> dplyr::group_by(dplyr::across(c(-artifact, -date)))
                |> dplyr::slice_head(n = 1)
                |> dplyr::ungroup()
                |> dplyr::pull(artifact)
            )
            discard_artifact <- dplyr::setdiff(dplyr::pull(self$show(), artifact), keep_artifact)
            archivist::rmFromLocalRepo(discard_artifact, repoDir = private$path, removeData = TRUE, removeMiniature = TRUE, many = TRUE)
        }
    )# end private
)# end Archive
