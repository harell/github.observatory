# Setup -------------------------------------------------------------------
# remotes::install_cran("ggraph")

library(ggraph)
pkgload::load_all(usethis::proj_get(), quiet = TRUE)
if(does_not_exist("ecos")) ecos <- Ecosystem$new()
if(does_not_exist("agent")) agent <- Agent$new()


# Configuration -----------------------------------------------------------
selected_package <- "distr6"
selected_repo_id <- ecos$read_REPO() |> dplyr::filter(package == selected_package) |> dplyr::slice(1) |> dplyr::pull(id)


# Query Linked Packages ---------------------------------------------------
graph <- agent$query_repos_graph(
    selected_repo_id,
    degrees = 1,
    method = c("depends", "reverse depends")[1]
)


# Process Graph Data ------------------------------------------------------
(
    tbl_nodes <- ecos$read_REPO()
    |> dplyr::filter(id %in% graph$to)
    |> dplyr::left_join(ecos$read_PACKAGE() |> dplyr::select(-full_name), by = "package")
    |> dplyr::rename(name = id)
)

(
    tbl_links <- graph
    |> dplyr::transmute(
        from = from,
        to = to,
        weight = 1
    )
)

graph <- igraph::graph_from_data_frame(tbl_links, vertices = tbl_nodes, directed = TRUE)


# Visualise Graph ---------------------------------------------------------
(
    graph
    |> ggraph(layout = 'kk', weights = weight)
    + geom_edge_link2()
    + geom_node_point()
    + geom_node_label(aes(label = package))
    + theme_graph(background = "white")
)

