#' @title Recommendation Engine for R Users and R Packages
#' @description
#' Recommend R Users on Github some R packages that they may like.
#'
#' @param user_id (`integer`) Github User ID.
#' @param repo_id (`integer`) Github Repo ID.
#' @param n (`integer`) How many recommendation should the function return.
#' @param degrees (`integer`) How many degrees of separation should be included? `1` are only the closest nodes. `Inf` returns all the graph.
#'
#' @section recommend_repos_to_user:
#'
#' * `random` returns the information of random packages
#'
#' @section recommend_users_to_user:
#'
#' * `random` returns the information of random users
#'
#' @family R Ecosystem classes
#' @export
Agent <- R6::R6Class(
    cloneable = FALSE,
    public = list(
        #' @description Instantiate an Agent object
        #' @param ecos (`Repository`) An \link{Ecosystem} object.
        initialize = function(ecos = Ecosystem$new()){
            private$ecos <- ecos
        },
        #' @description Given a `user_id` suggests `n` repos the user might like.
        #' @param method (`character`) The recommendation filtering technique to employ. See **recommend_repos_to_user** section for details.
        recommend_repos_to_user = function(user_id, n, method) { return(private$.recommend_repos_to_user(user_id, n, method)) },
        #' @description Given a `user_id` suggests `n` users the user might like.
        #' @param method (`character`) The recommendation filtering technique to employ. See **recommend_users_to_user** section for details.
        recommend_users_to_user = function(user_id, n, method) { return(private$.recommend_users_to_user(user_id, n, method)) },
        #' @description Given a `repo_id` find all linked packages in `degrees` degrees of separation.
        #' @param method (`character`) The link type to employ. Either `depends` or `reverse depends`.
        #' @return (`data.frame`) A table with two columns `from` and `to`. If a repo has dependencies, then `from` = `to`. If the repo is dependent on a non-existing package repo, such as 'base', the dependency is discarded.
        query_repos_graph = function(repo_id, degrees = 1, method) { return(private$.query_repos_graph(repo_id, degrees, method)) },
        #' @description Given a `user_id` find all linked users in `degrees` degrees of separation.
        #' @param method (`character`) The link type to employ. Either
        #' (1) `followers` What users are following `user_id`?; or
        #' (2) `following` What users is `user_id` following?.
        query_users_graph = function(user_id, degrees = 1, method) { return(private$.query_users_graph(user_id, degrees, method)) }
    ), private = list(
        # Private Fields ----------------------------------------------------------
        ecos = new.env(),
        # Private Methods ---------------------------------------------------------
        .recommend_repos_to_user = function(...) { stop() },
        .query_repos_graph = function(...) { stop() }
    )
)


# Private Methods ---------------------------------------------------------
Agent$set(which = "private", name = ".recommend_repos_to_user", overwrite = TRUE, value = function(user_id, n, method) {
    assert_that(assertthat::is.count(user_id), assertthat::is.count(n))
    method <- match.arg(tolower(method), c("random"))

    repos_to_exclude <- .recommenders$utils$get_repos2exclude(private$ecos, user_id)

    repos_id <- switch (
        method,
        random = .recommenders$repos2users$random(private$ecos, user_id, n, repos_to_exclude)
    )

    return(repos_id)
})

Agent$set(which = "private", name = ".recommend_users_to_user", overwrite = TRUE, value = function(user_id, n, method) {
    method <- match.arg(tolower(method), c("random"))
    assert_that(assertthat::is.count(user_id), assertthat::is.count(n))

    users_to_exclude <- .recommenders$utils$get_users2exclude(private$ecos, user_id)
    users_to_exclude <- integer(0)

    users_id <- switch (
        method,
        random = .recommenders$users2users$random(private$ecos, user_id, n, users_to_exclude)
    )

    return(users_id)
})

Agent$set(which = "private", name = ".query_repos_graph", overwrite = TRUE, value = function(repo_id, degrees = 1, method) {
    assert_that(assertthat::is.count(repo_id), assertthat::is.count(degrees))

    method <- match.arg(tolower(method), c("depends", "reverse depends"))

    if(method == "depends"){
        return(.recommenders$repos_graph$depends(private$ecos, repo_id, degrees))
    } else if (method == "reverse depends") {
        return(.recommenders$repos_graph$reverse_depends(private$ecos, repo_id, degrees))
    }
})


Agent$set(which = "private", name = ".query_users_graph", overwrite = TRUE, value = function(user_id, degrees = 1, method) {
    assert_that(assertthat::is.count(user_id), assertthat::is.count(degrees))
    method <- match.arg(tolower(method), c("followers", "following"))

    if(method == "followers"){
        return(.recommenders$users_graph$followers(private$ecos, user_id, degrees))
    } else if (method == "following") {
        return(.recommenders$users_graph$following(private$ecos, user_id, degrees))
    }
})


# Recommenders ------------------------------------------------------------
.recommenders <- new.env()

.recommenders$utils <- new.env()


# recommend_repos_to_user -------------------------------------------------
.recommenders$repos2users <- new.env()

.recommenders$repos2users$random <- function(ecos, user_id, n, repos_to_exclude) {
    set.seed(2144)
    null_table <- tibble::tibble(rank = NA_integer_, repo_id = NA_integer_)[0,]

    tryCatch(return(
        repos <- ecos$read_REPO()
        |> dplyr::filter(id %not_in% repos_to_exclude)
        |> dplyr::slice_sample(n = n)
        |> dplyr::transmute(
            rank    = as.integer(1:dplyr::n()),
            repo_id = as.integer(id)
        )
    ), error = function(e) return(null_table))
}


# recommend_users_to_user -------------------------------------------------
.recommenders$users2users <- new.env()

.recommenders$users2users$random <- function(ecos, user_id, n, users_to_exclude)
{
    set.seed(2144)
    null_table <- tibble::tibble(rank = NA_integer_, user_id = NA_integer_)[0,]

    tryCatch(return(
        users <- ecos$read_USER()
        |> dplyr::filter(id %not_in% users_to_exclude)
        |> dplyr::slice_sample(n = n)
        |> dplyr::transmute(
            rank    = as.integer(1:dplyr::n()),
            user_id = as.integer(id)
        )
    ), error = function(e) return(null_table))
}


# query_repos_graph -------------------------------------------------------
.recommenders$repos_graph <- new.env()

.recommenders$repos_graph$depends <- function(ecos, repo_id, degrees) {
    result <- tibble::tibble(degree = 0, from = repo_id, to = repo_id)

    tryCatch({
        new_dependencies <- repo_id
        repos <- ecos$read_REPO() |> dplyr::distinct(id, package)
        invisible(
            dependencies <- ecos$read_DEPENDENCY()
            |> dplyr::mutate(to = dplyr::if_else(is.na(to), from, to))
            |> dplyr::left_join(repos, by = c(from = "package"))
            |> dplyr::transmute(from = id, to = to)
            |> dplyr::left_join(repos, by = c(to = "package"))
            |> dplyr::transmute(from = from, to = id)
            |> tidyr::drop_na()
        )

        for(degree in seq_len(degrees)){
            existing_dependencies <- unique(result$to)

            invisible(
                new_result <- dependencies
                |> dplyr::filter(from %in% new_dependencies)
                |> dplyr::transmute(degree = degree, from = from, to = to)
            )

            result <- dplyr::bind_rows(result, new_result) |> dplyr::distinct()

            all_dependencies <- unique(c(result$to, result$from))
            new_dependencies <- setdiff(result$to, existing_dependencies)

            if(all(is.na(new_dependencies))) break
        }

        return(result)

    }, error = function(e) return(result))
}

.recommenders$repos_graph$reverse_depends <- function(ecos, repo_id, degrees) {
    result <- tibble::tibble(degree = 0, from = repo_id, to = repo_id)

    tryCatch({
        new_dependencies <- repo_id
        repos <- ecos$read_REPO() |> dplyr::distinct(id, package)
        invisible(
            dependencies <- ecos$read_DEPENDENCY()
            |> dplyr::mutate(to = dplyr::if_else(is.na(to), from, to))
            |> dplyr::left_join(repos, by = c(from = "package"))
            |> dplyr::transmute(from = id, to = to)
            |> dplyr::left_join(repos, by = c(to = "package"))
            |> dplyr::transmute(from = from, to = id)
            |> tidyr::drop_na()
        )

        for(degree in seq_len(degrees)){
            existing_dependencies <- unique(result$from)

            invisible(
                new_result <- dependencies
                |> dplyr::filter(to %in% new_dependencies)
                |> dplyr::transmute(degree = degree, from = from, to = to)
            )

            result <- dplyr::bind_rows(result, new_result) |> dplyr::distinct()

            all_dependencies <- unique(c(result$from, result$to))
            new_dependencies <- setdiff(result$from, existing_dependencies)

            if(all(is.na(new_dependencies))) break
        }

        return(result)

    }, error = function(e) return(result))
}


# query_users_graph -------------------------------------------------------
.recommenders$users_graph <- new.env()

.recommenders$users_graph$followers <- function(ecos, user_id, degrees) {
    result <- tibble::tibble(degree = 0, from = user_id, to = user_id)

    tryCatch({
        new_users <- user_id
        fellowship <- ecos$read_FOLLOWING()

        for(degree in seq_len(degrees)){
            existing_users <- unique(result$to)

            invisible(
                new_result <- fellowship
                |> dplyr::filter(to %in% new_users)
                |> dplyr::transmute(degree = degree, from = from, to = to)
            )

            result <- dplyr::bind_rows(result, new_result) |> dplyr::distinct()

            all_users <- unique(c(result$to, result$from))
            new_users <- setdiff(result$from, existing_users)

            if(all(is.na(new_users))) break
        }

        return(result)

    }, error = function(e) return(result))
}

.recommenders$users_graph$following <- function(ecos, user_id, degrees) {
    result <- tibble::tibble(degree = 0, from = user_id, to = user_id)

    tryCatch({
        new_users <- user_id
        fellowship <- ecos$read_FOLLOWING()

        for(degree in seq_len(degrees)){
            existing_users <- unique(result$to)

            invisible(
                new_result <- fellowship
                |> dplyr::filter(from %in% new_users)
                |> dplyr::transmute(degree = degree, from = from, to = to)
            )

            result <- dplyr::bind_rows(result, new_result) |> dplyr::distinct()

            all_users <- unique(c(result$to, result$from))
            new_users <- setdiff(result$to, existing_users)

            if(all(is.na(new_users))) break
        }

        return(result)

    }, error = function(e) return(result))
}


# Utilities ---------------------------------------------------------------
.recommenders$utils$get_repos2exclude <- function(ecos, user_id) {
    tryCatch(return(
        ecos$read_SPECTATOR()
        |> dplyr::filter(user_id %in% !!user_id)
        |> dplyr::distinct(repo_id)
        |> dplyr::pull(repo_id)
    ), error = function(e) return(0L))
}

.recommenders$utils$get_users2exclude <- function(ecos, user_id) {
    tryCatch(return(
        ecos$read_FOLLOWING()
        |> dplyr::filter(from %in% !!user_id)
        |> dplyr::distinct(to)
        |> dplyr::pull(to)
    ), error = function(e) return(0L))
}
