# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Download CRAN packages list ---------------------------------------------
## Packages Task Views
views <- ctv::available.views()
packages_view <- tibble::tibble(package = NA_character_, task_view = NA_character_)[0,]

for(view in names(views)) invisible(
    packages_view <- views[[view]][["packagelist"]]
    |> dplyr::transmute(package = name, task_view = view)
    |> dplyr::bind_rows(packages_view)
)


# Process data ------------------------------------------------------------
## Bind Multiple Task Views
invisible(
    tidy_views <- packages_view
    |> dplyr::distinct()
    |> tibble::as_tibble()
)


# Teardown ----------------------------------------------------------------
ecos$overwrite_TASK_VIEW(tidy_views)
