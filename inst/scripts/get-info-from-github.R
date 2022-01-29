# Setup -------------------------------------------------------------------
# remotes::install_cran(c("gh"))
pkgload::load_all(usethis::proj_get())
result <- new.env()


# Configuration -----------------------------------------------------------
owner <- "tidyverse"
repo <- "dplyr"
user <- "harell"


# Package Queries ---------------------------------------------------------
## Query a package name
result$package$meta <- gh::gh(glue("/repos/{owner}/{repo}"))
## Query a package stargazers
result$package$stargazers <- gh::gh(glue("/repos/{owner}/{repo}/stargazers"))


# User Queries ------------------------------------------------------------
result$user$meta <- gh::gh(glue("/users/{user}"))
result$user$stars <- gh$user$get_starred(user)


# Misc --------------------------------------------------------------------
# gh::gh_rate_limit()
