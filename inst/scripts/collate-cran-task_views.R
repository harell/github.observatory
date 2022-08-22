# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Download CRAN packages list ---------------------------------------------
## Packages Task Views
views <- ctv::available.views()
packages_view <- tibble::tibble(task_view = NA_character_, package = NA_character_)[0,]

for(view in names(views)) invisible(
    packages_view <- views[[view]][["packagelist"]]
    |> dplyr::transmute(task_view = view, package = name)
    |> dplyr::bind_rows(packages_view)
)


# Process data ------------------------------------------------------------
## Bind Multiple Task Views
invisible(
    tidy_views <- packages_view
    |> dplyr::distinct()
    |> tidyr::nest(task_view = task_view)
    |> dplyr::rowwise()
    |> dplyr::mutate(task_view = task_view |> unlist() |> jsonlite::toJSON() |> as.character())
    |> dplyr::ungroup()
    |> tidyr::replace_na(list(task_view = "[]"))
)


# Teardown ----------------------------------------------------------------
ecos$overwrite_TASK_VIEW(tidy_views)
