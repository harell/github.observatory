# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
result <- new.env()


# Package Description Table -----------------------------------------------
pkg_desc <- tools::CRAN_package_db()

(
    tidy_pkg_desc <- pkg_desc
    |> standardise_col_names()
)
