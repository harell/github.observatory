# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()
agent <- Agent$new(ecos)


# Config ------------------------------------------------------------------
repo_id <- 19521307 # R6
degrees <- 1


# Control Logic -----------------------------------------------------------
(deps <- agent$query_repos_graph(repo_id, degrees = 1, method = "dep"))
(deps <- agent$query_repos_graph(repo_id, degrees = 1, method = "rev"))
(deps <- agent$query_repos_graph(repo_id, degrees = 2, method = "rev"))
(deps <- agent$query_repos_graph(repo_id, degrees = 6, method = "rev"))


