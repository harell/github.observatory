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
result <- agent$query_repos_graph(
    selected_repo_id,
    degrees = 2,
    method = c("depends", "reverse depends")[1]
)


# Process Graph Data ------------------------------------------------------
(
    tbl_nodes <- ecos$read_REPO()
    |> dplyr::filter(id %in% result$to)
    |> dplyr::left_join(ecos$read_PACKAGE() |> dplyr::select(-full_name), by = "package")
    |> dplyr::rename(name = id)
)

(
    tbl_links <- result
    |> dplyr::transmute(
        from = from,
        to = to,
        # degree = degree,
        weight = 1
    )
)

graph <- igraph::graph_from_data_frame(tbl_links, vertices = tbl_nodes, directed = TRUE)


# Visualise Graph ---------------------------------------------------------
(
    graph
    |> ggraph(layout = 'kk', weights = weight)
    + geom_edge_link2(arrow = grid::arrow(type = "closed", length = grid::unit(0.02, "npc")), alpha = 1, colour = "#808080")
    # + geom_edge_diagonal()
    + geom_node_point(aes(size = stargazers_count), alpha = 0.7, show.legend = FALSE, colour = "black")
    + geom_node_text(aes(label = package), repel = TRUE)
    + scale_color_brewer(palette = "Set1")
    + theme_void()
)

