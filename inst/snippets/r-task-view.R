# https://ctv.r-forge.r-project.org/reference/ctv-client.html
# remotes::install_cran("ctv")

views <- ctv::available.views()
package_view <- tibble::tibble(task_view = NA_character_, package = NA_character_)[0,]

for(view in names(views)) invisible(
    package_view <- views[[view]][["packagelist"]]
    |> dplyr::transmute(task_view = view, package = name)
    |> dplyr::bind_rows(package_view)
)

(
    package_view
    |> dplyr::count(package)
    |> dplyr::arrange(-n)
    |> tibble::as_tibble()
    |> dplyr::pull(n)
    |> table()
)

(
    a <- package_view
    |> dplyr::distinct()
    |> tidyr::nest(task_view = task_view)
    |> dplyr::rowwise()
    |> dplyr::mutate(task_view = task_view |> unlist() |> jsonlite::toJSON() |> as.character())
    |> dplyr::ungroup()

)
