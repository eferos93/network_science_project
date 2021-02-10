# Title     : TODO
# Objective : TODO
# Created by: eferos93
# Created on: 10/02/21

source('clean_data.R')
library(tidyverse)
library(igraph)
library(ggraph)
library(devtools)
install_github('eferos93/tidygraph')
library(tidygraph)

set_graph_style()

graph <- tbl_graph(nodes = programming_languages_tibble,
                   edges = influences_tibble, directed = TRUE) %>%
  activate(nodes) %>%
  arrange(year)

get_centrality <- function (graph, centrality_function) {
  result_centraility <-
    graph %>%
      activate(nodes) %>%
      mutate(influence = centrality_function)
  return(result_centraility)
}

plot_centrality <- function (graph_with_centrality, filtering_degree = 0, centrality_metric) {
  graph_with_centrality %>%
    filter(influence > filtering_degree) %>%
    filter(!node_is_isolated()) %>%
    ggraph(layout = 'fr') +
    geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE, edge_colour = 'grey66') +
    geom_node_point(aes(size = influence), show.legend = FALSE) +
    geom_node_text(aes(label = name), repel = TRUE, show.legend = FALSE, colour = 'red', family = 'serif') +
    labs(
      subtitle = paste('Centrality metric:', centrality_metric)
    ) +
    theme_void()
}

plot_most_influent_languages <- function (graph_with_centraility, how_many = 10, centrality_metric) {
  as_tibble(graph_with_centraility, what = 'vertices') %>%
    top_n(how_many) %>%
    select(name, influence) %>%
    ggplot(aes(x = reorder(as.factor(name), -influence), y = influence)) +
    geom_bar(stat = 'identity', fill = 'blue4') +
    labs(
      title = 'Top ten most influential programming languages',
      subtitle = paste('Centrality metric used:', centrality_metric),
      x = '',
      y = 'Influence'
    ) +
    theme_classic()
}

subgraph_of_programming_language <- function (graph, programming_language,
                                              selected_mode = 'all') {
  subgraph <- graph %>%
    convert(to_bfs_tree,
            which(.N()$name == programming_language), mode = selected_mode) %>%
    activate(nodes) %>%
    filter(!node_is_isolated())

  subgraph %>%
    ggraph(layout = 'fr') +
    geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE, edge_colour = 'grey66') +
    geom_node_point(aes(size = influence), show.legend = FALSE) +
    geom_node_text(aes(filter = name != programming_language, label = name),
                   repel = TRUE, show.legend = FALSE, colour = 'red', family = 'serif') +
    geom_node_text(aes(filter = name == programming_language, label = name),
                   colour = 'green', family = 'serif', repel = TRUE, show.legend = FALSE) +
    theme_void()
}

get_clusters <- function (graph, centrality_function, clustering_function) {
  graph_no_isolated_nodes <- graph %>%
    activate(nodes) %>%
    mutate(influence = centrality_function) %>%
    filter(!node_is_isolated()) %>%
    filter(influence > 0)

  clustered <- graph_no_isolated_nodes %>%
    activate(nodes) %>%
    mutate(group = clustering_function %>%  as.factor())
  return(clustered)
}
plot_clusters <-  function(clustered, centrality_function_name, clustering_fuction_name, groups = NULL) {
  if(!is.null(groups)) {
    clustered <- clustered %>% filter(group %in% groups)
  }
  clustered %>%
    ggraph(layout = 'fr') +
    geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE, edge_colour = 'grey66') +
    geom_node_point(aes(colour = group), show.legend = FALSE) +
    geom_node_text(aes(label = name), repel = TRUE, show.legend = FALSE, colour = 'black', family = 'serif') +
    facet_nodes(~group) +
    labs(
      subtitle = paste0('Centrality metric used to filter nodes: ', centrality_function_name,
                        '. Clustering algorithm used: ', clustering_fuction_name)
    ) +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
}



# Network
graph %>% filter(!node_is_isolated()) %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE, edge_colour = 'grey66') +
  geom_node_point(show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = TRUE, show.legend = FALSE, colour = 'red', family = 'serif')
#------------------------------------------------------------------------------------------------

# Centralities
graph_influence_degree <- get_centrality(graph, centrality_degree(mode = 'in'))
plot_centrality(graph_influence_degree, centrality_metric = 'in-degree')
plot_most_influent_languages(graph_influence_degree, centrality_metric = 'in-degree')

graph_closeness_centrality <- get_centrality(graph, centrality_closeness(mode = 'in'))
plot_centrality(graph_closeness_centrality, centrality_metric = 'in-closeness')
plot_most_influent_languages(graph_closeness_centrality, centrality_metric = 'in-closeness')

graph_pagerank <- get_centrality(graph, centrality_pagerank())
plot_centrality(graph_pagerank, centrality_metric = 'pagerank')
plot_most_influent_languages(graph_pagerank, centrality_metric = 'pagerank')

graph_edge_betweenness <- get_centrality(graph, centrality_betweenness())
plot_centrality(graph_edge_betweenness, centrality_metric = 'betweenness')
plot_most_influent_languages(graph_edge_betweenness, centrality_metric = 'betweenness')

graph_influence_degree <- get_centrality(graph, centrality_degree(mode = 'out'))
plot_centrality(graph_influence_degree, centrality_metric = 'out-degree')

plot_most_influent_languages(graph_influence_degree, centrality_metric = 'out-degree')
#---------------------------------------------------------------------------------------------

# Clustering
get_clusters(graph, centrality_pagerank(), group_infomap()) %>%
  plot_clusters('pagerank', 'infomap', 1:8)


get_clusters(graph, centrality_betweenness(), group_infomap()) %>%
        plot_clusters('betweenness', 'infomap', 1:8)


get_clusters(graph, centrality_closeness(), group_infomap()) %>%
  plot_clusters('closeness', 'infomap', 1:4)