# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_db")) user_db <- UserQueryDB$new()
if(does_not_exist("user_queue")) user_queue <- QueryQueue$new()$USER


# Inquire -----------------------------------------------------------------
msg_bar <- "Quering Github Users [:bar] :current/:total (:percent) eta: :eta"
pb <- progress::progress_bar$new(format = msg_bar, total = user_queue$size(), clear = FALSE)
while(user_queue$size() > 0){ try(pb$tick(1), silent = TRUE); tryCatch({
    github$alter_PAT()
    if((user_queue$size() - 1) %% 10 == 0) while(github$return_remaining_quote() < 10) Sys.sleep(60)

    user_id <- user_queue$pop()
    user_overview <- observatory$query$user$by_id(user_id)
    user_db$save(data = user_overview, entity = "user", type = "overview", id = user_id, alias = user_overview$login)

    artifact <- if(user_overview$following > 0)(
        user_overview$login
        |> observatory$query$user$following()
        |> purrr::map(~purrr::keep(.x, names(.x) %in% c("login", "id")))
        |> user_db$save(entity = "user", type = "following", id = user_id, alias = user_overview$login)
    )

    user_db$commit()
    try(pb$message(events$SucceededToQueryUser(user_id)), silent = TRUE)

}, error = function(e){
    suppressMessages(user_db$rollback())
    try(pb$message(events$FailedToQueryUser(user_id)), silent = TRUE)
})}
