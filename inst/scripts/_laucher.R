# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)

# Workflow ----------------------------------------------------------------
print("Collate packages information from CRAN")
source(system.file("scripts", "collate-cran-desc.R", package = "github.observatory", mustWork = TRUE))

print("Collate packages information from GitHub")
source(system.file("scripts", "collate-repo-desc.R", package = "github.observatory", mustWork = TRUE))

print("Parse repo spectators")
source(system.file("scripts", "parse-repo-spectators.R", package = "github.observatory", mustWork = TRUE))

print("Collate users information from GitHub")
source(system.file("scripts", "collate-user-info.R", package = "github.observatory", mustWork = TRUE))

print("Parse repo information")
source(system.file("scripts", "parse-repo-info.R", package = "github.observatory", mustWork = TRUE))

print("Parse user information")
source(system.file("scripts", "parse-user-info.R", package = "github.observatory", mustWork = TRUE))

print("Parse user following")
source(system.file("scripts", "parse-user-following.R", package = "github.observatory", mustWork = TRUE))

print("Update user information")
source(system.file("scripts", "update-user-info.R", package = "github.observatory", mustWork = TRUE))
