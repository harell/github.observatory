# Setup -------------------------------------------------------------------
# remotes::install_cran(c("ggraph", "tidygraph"))
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()


# Task View Graph ---------------------------------------------------------
task_views <- ecos$read_TASK_VIEW()

