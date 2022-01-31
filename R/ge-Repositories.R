#' @title Github Explorer Repository
#' @export
#' @noRd
Repository <- R6::R6Class(
    classname = "Repository", public = list(
        #' @param immediate (`logical`) Should queries be committed immediately?
        initialize = function(immediate = FALSE){
            private$immediate <- immediate

            ## Configurations
            private$paths <- list(package = list())
            private$paths$package <- list(cran_package_description = usethis::proj_path("_cache", "cran_package_description", ext = "csv"))

            ## Set Cache
            private$cache <- new.env()
            private$cache$package <- list(cran_package_description = self$read_package_description_table())
        },
        #' @description Store changes
        commit = function(){
            x <- private$cache$package$cran_package_description
            file <- private$paths$package$cran_package_description
            private$write_sheet(x, file)

            message("Commited data to database")
            invisible(self)
        },
        write_package_description_table = function(x){private$cache$package$cran_package_description <- x; invisible(self)},
        read_package_description_table = function(){private$read_sheet(private$paths$package$cran_package_description)}
    ), private = list(
        # Private Methods ---------------------------------------------------------
        write_sheet = function(x, file){
            dir.create(dirname(file), FALSE, TRUE)
            readr::write_csv(x, file, na = "")
            message("Saved ", basename(file))
        },
        read_sheet = function(file){
            x <- if(file.exists(file)) readr::read_csv(file, show_col_types = FALSE, lazy = FALSE) else tibble::tibble()
            message("Loaded ", basename(file))
            return(x)
        },
        # Private Fields ----------------------------------------------------------
        immediate = NULL,
        cache = new.env(),
        paths = list()
    )
)

#' @title Github Explorer Archive
#' @export
#' @noRd
Archive <- R6::R6Class(
    classname = "Repository", public = list(
        # Public Methods ----------------------------------------------------------
        #' @param path (`character`) A character denoting an existing directory of the Repository for which metadata will be aved and returned.
        initialize = function(path = usethis::proj_path("_cache")){
            private$path <- path
            suppressMessages(archivist::createLocalRepo(path))
        },
        save = function(artifact, tags = character()){
            archivist::saveToLocalRepo(artifact = artifact, repoDir = private$path, archiveTags = FALSE, archiveMiniature = FALSE, archiveSessionInfo = FALSE, force = TRUE, userTags = c(paste0("date:", lubridate::format_ISO8601(Sys.Date())), tags))
            invisible(self)
        },
        show = function() return(
            archivist::splitTagsLocal(repoDir = private$path)
            |> dplyr::select(-createdDate)
            |> dplyr::distinct()
            |> tidyr::pivot_wider(id_cols = artifact, names_from = tagKey, values_from = tagValue)
            |> dplyr::select(-format)
            |> dplyr::mutate(date = as.Date(date))
        )
    ), private = list(
        # Private Fields ----------------------------------------------------------
        path = character()
    )
)
