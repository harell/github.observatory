#' @title Github Explorer Archive
#'
#' @param artifact (`?`) An arbitrary R artifact to be saved.
#' @param tags (`character`) tags in "key:value" format. E.g. "entity:repo", "type:overview".
#' @param md5hash (`character`) artifact md5 hash.
#'
#' @export
Archive <- R6::R6Class(classname = "Archive", cloneable = FALSE, public = list(
    # Public Methods ----------------------------------------------------------
    #' @param path (`character`) A character denoting an existing directory of the Archive for which metadata will be aved and returned.
    #' @param immediate (`logical`) Should queries be committed immediately?
    initialize = function(path = usethis::proj_path("_cache"), immediate = FALSE){
        dir.create(path, F, T)
        private$path <- path
        private$immediate <- immediate
        suppressMessages(archivist::createLocalRepo(path))
    },
    #' @description store artifact in archive
    save = function(artifact, tags = character()){
        tags <- c(paste0("date:", lubridate::format_ISO8601(Sys.Date())), tags)

        if(private$immediate){
            private$.save(artifact, tags = tags)
        } else {
            private$tags <- append(private$tags, list(tags))
            private$artifact <- append(private$artifact, list(artifact))
        }

        invisible(self)
    },
    #' @description fetch artifact from archive
    load = function(md5hash){
        purrr::map(md5hash, archivist::loadFromLocalRepo, repoDir = private$path, value = TRUE)
    },
    #' @description discard artifact from archive
    delete = function(md5hash){
        private$.delete(md5hash)
        invisible(self)
    },
    #' @description show artifact within the archive
    show = function() if(private$is_empty_archive()){
        return(tibble::tibble(artifact = NA_character_, date = Sys.Date()) |> tidyr::drop_na())
    } else {
        return(
            archivist::splitTagsLocal(repoDir = private$path)
            |> dplyr::select(-createdDate)
            |> tidyr::pivot_wider(id_cols = artifact, names_from = tagKey, values_from = tagValue, values_fn = list)
            |> tidyr::unnest(cols = -artifact)
            |> dplyr::select(-format)
            |> dplyr::distinct()
            |> dplyr::mutate(date = as_date(date))
            |> dplyr::arrange(dplyr::across(-artifact))
        )
    },
    #' @description commit artifacts to archive
    commit = function(){
        purrr::walk2(private$artifact, private$tags, private$.save)
        private$artifact <- private$tags <- list()
        message("Commited artifacts to archive")
        invisible(self)
    },
    #' @description rollback changes from the archive
    rollback = function(){
        private$artifact <- private$tags <- list()
        message("Unrolled artifacts to archive")
        invisible(self)
    },
    #' @description clean the archive
    clean = function(){
        private$discard_corrupted()
        private$discard_duplicates()
        private$discard_outdated()
        invisible(self)
    },
    #' @description teardown archive object
    finalize = function(){
        suppressMessages(self$rollback())
        invisible(self)
    }
), private = list(
    # Private Fields ----------------------------------------------------------
    path = character(),
    immediate = logical(),
    tags = list(),
    artifact = list(),
    # Private Methods ---------------------------------------------------------
    .save = function(artifact, tags = character()) archivist::saveToLocalRepo(artifact = artifact, repoDir = private$path, archiveTags = FALSE, archiveMiniature = FALSE, archiveSessionInfo = FALSE, force = TRUE, userTags = tags),
    .delete = function(md5hash) archivist::rmFromLocalRepo(md5hash, repoDir = private$path, removeData = TRUE, removeMiniature = TRUE, many = TRUE),
    is_empty_archive = function() nrow(archivist::showLocalRepo(repoDir = private$path, method = 'tags')) == 0,
    discard_duplicates = function(){
        if(private$is_empty_archive()) return()
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
        if(private$is_empty_archive()) return()
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
    },
    discard_corrupted = function(){
        if(private$is_empty_archive()) return()
        invisible(
            corrupted_artifacts <- archivist::splitTagsLocal(repoDir = private$path)
            |> dplyr::select(-createdDate)
            |> dplyr::group_by(artifact, tagKey)
            |> dplyr::summarise(n_instances = dplyr::n(), .groups = "drop")
            |> dplyr::distinct(artifact, n_instances)
            |> dplyr::count(artifact, name = "n_variants")
            |> dplyr::filter(n_variants > 1)
            |> dplyr::pull(artifact)
        )
        archivist::rmFromLocalRepo(corrupted_artifacts, repoDir = private$path, removeData = TRUE, removeMiniature = TRUE, many = TRUE)
    }
)# end private
)# end Archive


# Archive Derivatives -----------------------------------------------------
#' @describeIn Archive User Archive
UserArchive <- new.env()
UserArchive$new <- function(path = usethis::proj_path("_cache", "archive", "user"), immediate = FALSE) Archive$new(path, immediate)

#' @describeIn Archive Repo Archive
RepoArchive <- new.env()
RepoArchive$new <- function(path = usethis::proj_path("_cache", "archive", "repo"), immediate = FALSE) Archive$new(path, immediate)
