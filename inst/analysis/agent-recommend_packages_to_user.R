# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Config ------------------------------------------------------------------
user_id <- 7226303
n <- 10


# Helpers -----------------------------------------------------------------
.recommenders <- new.env()

.recommenders$utils <- new.env()

.recommenders$repos2users <- new.env()

.recommenders$repos2users$random <- function(ecos, user_id, n, repos_to_exclude) {
    set.seed(2144)
    tryCatch(return(
        repos <- ecos$read_REPO()
        |> dplyr::rename(repo_id = id)
        |> dplyr::filter(repo_id %not_in% repos_to_exclude)
        |> dplyr::sample_n(size = dplyr::n())
        |> dplyr::pull(repo_id)
        |> head(n)
    ), error = function(e) return(integer(0)))
}

.recommenders$utils$get_repos_to_exclude <- function(ecos, user_id) {
    tryCatch(return(
        ecos$read_SPECTATOR()
        |> dplyr::filter(user_id %in% !!user_id)
        |> dplyr::distinct(repo_id)
        |> dplyr::pull(repo_id)
    ), error = function(e) return(integer(0)))
}
\

# Control Logic -----------------------------------------------------------
repos_to_exclude <- .recommenders$utils$get_repos_to_exclude(ecos, user_id)

repos_id <- .recommenders$repos2users$random(ecos, user_id, n, repos_to_exclude)
