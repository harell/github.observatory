# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()
archive <- Archive$new()


# Query Github ------------------------------------------------------------
invisible(
    users <- repository$read_repo_desc()
    |> tidyr::unnest(cols = stargazers)
    |> dplyr::count(stargazers, sort = TRUE)
    |> dplyr::pull("stargazers")
    |> setdiff(tryCatch(archive$show() |> dplyr::filter(entity %in% "user") |> dplyr::pull("id"), error = function(e) return(0)))
)

pb <- progress::progress_bar$new(format = "Quering Github Users [:bar] :current/:total (:percent) eta: :eta", total = length(users), clear = FALSE)
for(user in users) tryCatch({
    if((which(users %in% user) - 1) %% 10 == 0) if(github$return_remaining_quote() < 50) {pb$message(glue("Reached GitHub API call limit")); break}
    # if((which(users %in% user) - 1) %% 10 == 0) while(github$return_remaining_quote() < 50) Sys.sleep(60)
    try(pb$tick(1), silent = TRUE)

    artifact <- query$user$by_id(user)
    archive$save(artifact, tags = c("entity:user", "type:overview", paste0("id:", user_id)))

    artifact <- query$user$following(artifact$login)
    artifact <- artifact |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
    archive$save(artifact, tags = c("entity:user", "type:following", paste0("id:", user)))

    try(pb$message(glue("Retrieved user `{user}`")), silent = TRUE)
}, error = function(e) try(pb$message(glue("Failed to Retrieve user `{user_id}`")), silent = TRUE))


# Teardown ----------------------------------------------------------------
# repository$write_repo_desc(tidy_repo_desc)$commit()
