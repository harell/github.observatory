# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repo_archive")) repo_archive <- RepoArchive$new()
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()


# Queue users -------------------------------------------------------------
invisible(
    artifacts <- repo_archive$show()
    |> dplyr::filter(type %in% c("contributors", "forkers", "stargazers", "watchers")[c(1,3)])
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, type, .keep_all = TRUE)
)

invisible(
    artifacts$data <- artifacts$artifact
    |> purrr::map(~.x |> repo_archive$load() |> unlist())
    |> purrr::map(~as.integer(.x[names(.x) == "id"]))
)

invisible(
    spectators <- artifacts
    |> dplyr::pull(data)
    |> purrr::flatten_int()
    |> unique()
)

users <- setdiff(spectators, tryCatch(user_archive$show() |> dplyr::filter(entity %in% "user") |> dplyr::pull("id") |> as.integer(), error = function(e) return(0)))


# Query Github ------------------------------------------------------------
pb <- progress::progress_bar$new(format = "Quering Github Users [:bar] :current/:total (:percent) eta: :eta", total = length(users), clear = FALSE)
for(user in users){try(pb$tick(1), silent = TRUE); tryCatch({
    github$alter_PAT()
    if((which(users %in% user) - 1) %% 10 == 0) while(github$return_remaining_quote() < 50) Sys.sleep(60)

    user_overview <- query$user$by_id(user)
    user_archive$save(user_overview, tags = c("entity:user", "type:overview", paste0("id:", user)))

    artifact <- if(user_overview$following > 0)(
        user_overview$login
        |> query$user$following()
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> user_archive$save(tags = c("entity:user", "type:following", paste0("id:", user)))
    )

    suppressMessages(user_archive$commit())
    try(pb$message(glue("Retrieved user `{user}`")), silent = TRUE)
}, error = function(e){
    suppressMessages(user_archive$rollback())
    try(pb$message(glue("Failed to Retrieve user `{user}`")), silent = TRUE)
})}


# Teardown ----------------------------------------------------------------
user_archive$clean()
