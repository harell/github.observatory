# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
source <- purrr::partial(base::source, local = FALSE, echo = TRUE)


# Workflow ----------------------------------------------------------------
cli::cli_h1("Collate packages information from CRAN")
source(system.file("scripts", "collate-cran-desc.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Collate packages dependencies from CRAN")
source(system.file("scripts", "collate-cran-dependencies.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Collate packages task views from CRAN")
source(system.file("scripts", "collate-cran-task_views.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Collate packages information from GitHub")
source(system.file("scripts", "collate-repo-desc.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Collate users information from GitHub")
source(system.file("scripts", "collate-user-info.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Parse repo spectators")
source(system.file("scripts", "parse-repo-spectators.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Parse repo information")
source(system.file("scripts", "parse-repo-info.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Parse user information")
source(system.file("scripts", "parse-user-info.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Parse user following")
source(system.file("scripts", "parse-user-following.R", package = "github.observatory", mustWork = TRUE))

cli::cli_h1("Update user information")
source(system.file("scripts", "update-user-info.R", package = "github.observatory", mustWork = TRUE))
