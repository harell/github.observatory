# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
if(!exists("ecos")) ecos <- Ecosystem$new()
# agent <- Agent$new(ecos)



# Random Users ------------------------------------------------------------
users_to_exclude <- integer(0)
n <- 1
set.seed(2144)
null_table <- tibble::tibble(rank = NA_integer_, user_id = NA_integer_)[0,]

(
    users <- ecos$read_USER()
    |> dplyr::filter(id %not_in% users_to_exclude)
    |> dplyr::filter(r_followers >= 25)
    |> dplyr::transmute(
        id = id,
        p = logb(r_followers, base = 2),
        flag = as.logical(rmultinom(n = 1, size = 10, p))
    )
    |> dplyr::filter(flag == TRUE)
    |> dplyr::arrange(-p)
    |> dplyr::transmute(user_id = as.integer(id))
    |> tibble::rowid_to_column(var = "rank")
)


log(rank(users$r_followers, ties.method = "min"))

rmultinom(n, 1, users$p)



# Random Packages ---------------------------------------------------------
repos_to_exclude <- integer(0)

(
    repos <- ecos$read_REPO()
    |> dplyr::filter(id %not_in% repos_to_exclude)
    |> dplyr::filter(stargazers_count  >= 81)
    |> dplyr::transmute(
        id = id,
        p = logb(stargazers_count, base = 2),
        flag = as.logical(rmultinom(n = 1, size = n, p))
    )
    |> dplyr::filter(flag == TRUE)
    |> dplyr::arrange(-p)
    |> dplyr::transmute(repo_id = as.integer(id))
    |> tibble::rowid_to_column(var = "rank")
)
