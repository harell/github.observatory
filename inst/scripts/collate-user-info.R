# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()
if(does_not_exist("user_queue")) user_queue <- QueryQueue$new()$USER


# Query Github ------------------------------------------------------------
msg_bar <- "Quering Github Users [:bar] :current/:total (:percent) eta: :eta"
pb <- progress::progress_bar$new(format = msg_bar, total = user_queue$size(), clear = FALSE)
while(user_queue$size() > 0){ try(pb$tick(1), silent = TRUE); tryCatch({
    github$alter_PAT()
    if((user_queue$size() - 1) %% 10 == 0) while(github$return_remaining_quote() < 10) Sys.sleep(60)

    user_id <- user_queue$pop()
    user_overview <- observatory$query$user$by_id(user_id)
    user_archive$save(user_overview, tags = c("entity:user", "type:overview", paste0("id:", user_id)))

    artifact <- if(user_overview$following > 0)(
        user_overview$login
        |> observatory$query$user$following()
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> user_archive$save(tags = c("entity:user", "type:following", paste0("id:", user_id)))
    )

    suppressMessages(user_archive$commit())
    try(pb$message(glue("Retrieved user `{user_id}`")), silent = TRUE)
}, error = function(e){
    suppressMessages(user_archive$rollback())
    try(pb$message(glue("Failed to Retrieve user `{user_id}`")), silent = TRUE)
})}


# Teardown ----------------------------------------------------------------
user_archive$clean()
