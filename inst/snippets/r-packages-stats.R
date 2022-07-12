# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get())
# remotes::install_cran(c("dlstats", "cranlogs"))
if(!exists("ecos")) ecos <- Ecosystem$new()
pkgs <- ecos$read_PACKAGE() |> dplyr::pull(package)


# dlstats -----------------------------------------------------------------
system.time(a <- dlstats::cran_stats(pkgs[1:1]))
# 1e0 |  69.19
# 1e1 |
# 1e2 |
# 1e3 |
# 1e4 |

(
    a1 <- a
    |> dplyr::transmute(
        date = lubridate::floor_date(start, "1 month"),
        downloads = downloads,
        package = package
    )
    |> dplyr::filter(
        date != lubridate::floor_date(Sys.Date(), "1 month")
    )
    |> tidyr::nest(cran_stats = c(date, downloads))
    |> dplyr::rowwise()
    |> dplyr::mutate(cran_stats = cran_stats |> jsonlite::toJSON() |> as.character())
)


# cranlogs ----------------------------------------------------------------
system.time(b <- cranlogs::cran_downloads(pkgs[1], from = "2017-01-01", to = "last-day"))

(
    b2 <- b
    |> dplyr::transmute(
        date = lubridate::floor_date(date, "1 month"),
        downloads = count,
        package = package
    )
    |> dplyr::count(date, package, name = "downloads")
    |> dplyr::filter(date != lubridate::floor_date(Sys.Date(), "1 month"))

)


# 1e0 |  0.81
# 1e1 |  2.54
# 1e2 | 21.62
# 1e3 | -
# 1e4 | -
