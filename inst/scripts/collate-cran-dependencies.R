# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Helpers -----------------------------------------------------------------
DependencyTable <- function(from = NA_character_, to = list()){
    tibble::tibble(from = from, to = to)
}


# Get CRAN Packages Names -------------------------------------------------
all_packages <- ecos$read_PACKAGE()
existing_packages <- tryCatch(
    ecos$read_DEPENDENCY(),
    error = function(e) return(tibble::tibble(from = NA_character_, to = NA_character_)[0,])
)
new_packages <- setdiff(
    all_packages |> dplyr::distinct(package) |> dplyr::pull(),
    existing_packages |> dplyr::distinct(from) |> dplyr::pull()
)


# Get CRAN Packages Dependencies ------------------------------------------
dependencies <- tools::package_dependencies(new_packages, which =  c("Depends", "Imports"), recursive = FALSE)


# Process data ------------------------------------------------------------
new_enteries <- tibble::tibble(from = NA_character_, to = list())[0,]
for(i in seq_along(dependencies)){
    new_enteries <- tibble::add_case(new_enteries, from = names(dependencies)[i], to = dependencies[i])
}
new_enteries <- new_enteries |> tidyr::unnest(to, keep_empty = TRUE, ptype = list(login = "character", id = "integer"))


# Consolidate Data --------------------------------------------------------
(
    tidy_dependencies <- dplyr::bind_rows(existing_packages, new_enteries)
    |> dplyr::distinct()
    |> dplyr::arrange(from, to)
)


# Teardown ----------------------------------------------------------------
ecos$overwrite_DEPENDENCY(tidy_dependencies)
