# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repository")) repository <- Repository$new()
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()


# Query Github ------------------------------------------------------------
invisible(
    users <- repository$read_repo_desc()
    |> tidyr::unnest(cols = stargazers_id)
    |> dplyr::count(stargazers_id, sort = TRUE)
    |> dplyr::pull("stargazers_id")
    |> setdiff(tryCatch(user_archive$show() |> dplyr::filter(entity %in% "user") |> dplyr::pull("id"), error = function(e) return(0)))
)

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