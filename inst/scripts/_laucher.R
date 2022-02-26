# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
print_banner <- purrr::partial(bannerCommenter::open_box, emph = TRUE, snug = TRUE, upper = FALSE, maxChar = 120)


# Workflow ----------------------------------------------------------------
print_banner("Collate packages information from CRAN")
source(system.file("scripts", "collate-cran-desc.R", package = "github.explorer", mustWork = TRUE))

print_banner("Collate packages information from GitHub")
source(system.file("scripts", "collate-repo-desc.R", package = "github.explorer", mustWork = TRUE))

print_banner("Parse GitHub packages information")
source(system.file("scripts", "parse-repo-desc.R", package = "github.explorer", mustWork = TRUE))

print_banner("Collate users information from GitHub")
source(system.file("scripts", "collate-user-info.R", package = "github.explorer", mustWork = TRUE))

print_banner("Parse user following")
source(system.file("scripts", "parse-user-following.R", package = "github.explorer", mustWork = TRUE))