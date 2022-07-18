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
    |> dplyr::transmute(
        id = id,
        login = login,
        r_followers = r_followers,
        p = logb(r_followers+1, base = 1.5),
        flag = as.logical(rmultinom(n = 1, size = 10, p))
    )
    |> dplyr::filter(flag == TRUE)
    |> dplyr::arrange(-r_followers)
    |> dplyr::transmute(user_id = as.integer(id))
    |> tibble::rowid_to_column(var = "rank")
)


log(rank(users$r_followers, ties.method = "min"))

rmultinom(n, 1, users$p)
