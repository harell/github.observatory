# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()
archive <- Archive$new()


# Query Github ------------------------------------------------------------
invisible(
    users_ids <- repository$read_repo_desc()
    |> tidyr::unnest(cols = stargazers)
    |> dplyr::count(stargazers, sort = TRUE)
    |> dplyr::pull("stargazers")
    # |> setdiff(archive$show()$user_id)
)

pb <- progress::progress_bar$new(format = "Quering Github Users [:bar] :current/:total (:percent) eta: :eta", total = length(packages), clear = FALSE)
for(user_id in users_ids) tryCatch({
    if((which(packages %in% package) - 1) %% 10 == 0) if(github$return_remaining_quote() < 50) {pb$message(glue("Reached GitHub API call limit")); break}
    # if((which(packages %in% package) - 1) %% 10 == 0) while(github$return_remaining_quote() < 50) Sys.sleep(60)

    try(pb$tick(1), silent = TRUE)

    artifact <- query$user$by_id(user_id)
    archive$save(artifact, tags = c("entity:user", "type:overview", paste0("id:", user_id)))

    artifact <- query$user$following(artifact$following)
    stop("only keep user_id(s)")
    archive$save(artifact, tags = c("entity:user", "type:following", paste0("id:", user_id)))

    try(pb$message(glue("Retrieved user `{user_id}`")), silent = TRUE)
}, error = function(e) try(pb$message(glue("Failed to Retrieve user `{user_id}`")), silent = TRUE))


# Teardown ----------------------------------------------------------------
repository$write_repo_desc(tidy_repo_desc)$commit()
