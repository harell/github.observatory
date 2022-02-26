# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
repository <- Repository$new()



# Statistics --------------------------------------------------------------
## How many cran vs non-cran Github addresses do we have?
(
    repository$read_cran_desc()
    |> dplyr::mutate(default_owner = stringr::str_detect(github_slug, "^cran/"))
    |> dplyr::summarise(ans = sum(!default_owner)/dplyr::n())
    |> dplyr::pull(ans)
)
