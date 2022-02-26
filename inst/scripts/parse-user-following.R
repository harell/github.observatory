# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("repository")) repository <- Repository$new()
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()


# Load cached data --------------------------------------------------------
invisible(
    tags <- user_archive$show()
    |> dplyr::filter(entity %in% "user", type %in% "following")
)

# Parse following ---------------------------------------------------------
system.time(artifacts <- user_archive$load(tags$artifact))
names(artifacts) <- tags$id

following_table <- tibble::tribble(~from, ~to)
pb <- progress::progress_bar$new(format = "Parsing User Following [:bar] :current/:total (:percent) eta: :eta", total = length(tags$id), clear = FALSE)
for(id in tags$id) tryCatch({
    if((which(id %in% tags$id) - 1) %% 100 == 0) try(pb$tick(100), silent = TRUE)

    invisible(
        following_entry <- artifacts
        |> purrr::pluck(id)
        |> purrr::map_dfr(~tibble::tibble(to = .x[["id"]]))
        |> tibble::add_column(from = as.integer(id), .before = "to")
    )

    following_table <- dplyr::bind_rows(following_table, following_entry)
}, error = function(e) pb$message(glue("Failed to parse `{id}`")))

invisible(
    following_table <- following_table
    |> dplyr::filter(to %in% unique(following_table$from))
    |> dplyr::distinct()
)


# Teardown ----------------------------------------------------------------
# repository$write_repo_desc(tidy_repo_desc)$commit()
