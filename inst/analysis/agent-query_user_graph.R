# Setup -------------------------------------------------------------------
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()
agent <- Agent$new(ecos)


# Config ------------------------------------------------------------------
user_id <- 7226303 # harell
degrees <- 1


# Control Logic -----------------------------------------------------------
(deps <- agent$query_users_graph(user_id, degrees = 1, method = "followers"))
(deps <- agent$query_users_graph(user_id, degrees = 1, method = "following"))
(deps <- agent$query_users_graph(user_id, degrees = 6, method = "following"))



