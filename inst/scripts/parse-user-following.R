# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("user_archive")) user_archive <- UserArchive$new()
if(does_not_exist("gdrive_repo")) gdrive_repo <- GDrive$new()


# Load cached data --------------------------------------------------------
invisible(
    artifacts <- user_archive$show()
    |> dplyr::filter(entity %in% "user", type %in% "following")
    |> dplyr::arrange(dplyr::desc(date))
    |> dplyr::distinct(id, type, .keep_all = TRUE)
)


# Parse following ---------------------------------------------------------
invisible(
    artifacts$data <- artifacts$artifact
    |> purrr::map(~.x |> user_archive$load() |> unlist())
    |> purrr::map(~as.integer(.x[names(.x) == "id"]))
)

invisible(
    following <- artifacts
    |> dplyr::transmute(from = as.integer(id), to = data)
    |> tidyr::unnest(to)
)

invisible(
    tidy_following <- following
    |> dplyr::filter(from %in% to)
)


# Teardown ----------------------------------------------------------------
gdrive_repo$overwrite_FOLLOWING(tidy_following)
