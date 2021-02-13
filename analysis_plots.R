# Title     : Analysis and Plot
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
  arrange(year)

get_centrality <- function (graph, centrality_function) {
  result_centraility <-
    graph %>%
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
      title = 'Centrality',
      subtitle = paste('Algorithm:', centrality_metric)
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
      title = 'Top ten most influenced programming languages',
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
    filter(!node_is_isolated())

  subgraph %>%
    ggraph(layout = 'fr', circular = TRUE) +
    geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE, edge_colour = 'grey66') +
    geom_node_point(aes(size = influence), show.legend = FALSE) +
    geom_node_text(aes(filter = name != programming_language, label = name),
                   repel = TRUE, show.legend = FALSE, colour = 'red', family = 'serif') +
    geom_node_text(aes(filter = name == programming_language, label = name),
                   colour = 'green', family = 'serif', repel = TRUE, show.legend = FALSE) +
    theme_void()
}

get_clusters <- function (graph, clustering_function) {
  graph %>% filter(!node_is_isolated()) %>%  mutate(group = clustering_function %>% as.factor())
}

plot_clusters <-  function(clustered, clustering_fuction_name, groups = NULL) {
  if(!is.null(groups)) {
    clustered <- clustered %>% filter(group %in% groups)
  }
  clustered %>%
    ggraph(layout = 'circle') +
    geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE, edge_colour = 'grey66') +
    geom_node_point(aes(colour = group), show.legend = FALSE) +
    geom_node_text(aes(label = name), repel = TRUE, show.legend = FALSE, colour = 'black', family = 'serif') +
    facet_nodes(~group) +
    labs(
      title = 'Communities',
      subtitle = paste('Clustering algorithm used:', clustering_fuction_name)
    ) +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
}

plot_save <- function(plot,
                      width = 4500,
                      height = 3000,
                      text.factor = 1,
                      filename = paste0(
                        format(
                          Sys.time(),
                          format = '%Y%m%d-%H%M%S'
                        ), '-Rplot.png'
                      )
) {

  dpi <- text.factor * 300
  width_calc <- width / dpi
  height_calc <- height / dpi

  ggsave(filename = filename,
         dpi = dpi,
         width = width_calc,
         height = height_calc,
         units = 'in',
         plot = plot)
}



#------------------------------------------------------------------------------------------------
# Network

p <- graph %>% filter(!node_is_isolated()) %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(alpha = stat(index)), show.legend = FALSE, edge_colour = 'grey66') +
  geom_node_point(show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = TRUE, show.legend = FALSE, colour = 'red', family = 'serif') +
  labs(
    title = 'The network'
  ) +
  theme_graph()

plot_save(p, filename = 'plots/network.png')


# Centralities
graph_influence_degree <- get_centrality(graph, centrality_degree(mode = 'in'))
plot_centrality(graph_influence_degree, centrality_metric = 'in-degree') %>%
  plot_save(filename = 'plots/degree_in.png')
plot_most_influent_languages(graph_influence_degree, centrality_metric = 'in-degree') %>%
  plot_save(filename = 'plots/hist_degree_in.png')

graph_closeness_centrality <- get_centrality(graph, centrality_closeness(mode = 'in'))
plot_centrality(graph_closeness_centrality, centrality_metric = 'in-closeness') %>%
  plot_save(filename = 'plots/closeness_in.png')
plot_most_influent_languages(graph_closeness_centrality, centrality_metric = 'in-closeness') %>%
  plot_save(filename = 'plots/hist_closeness_in.png')

graph_pagerank <- get_centrality(graph, centrality_pagerank())
plot_centrality(graph_pagerank, centrality_metric = 'pagerank') %>%
  plot_save(filename = 'plots/pagerank.png')
plot_most_influent_languages(graph_pagerank, centrality_metric = 'pagerank') %>%
  plot_save(filename = 'plots/hist_pagerank.png')

graph_edge_betweenness <- get_centrality(graph, centrality_betweenness())
plot_centrality(graph_edge_betweenness, centrality_metric = 'betweenness') %>%
  plot_save(filename = 'plots/betweenness.png')
plot_most_influent_languages(graph_edge_betweenness, centrality_metric = 'betweenness') %>%
  plot_save(filename = 'plots/hist_betweenness.png')

graph_influenced_degree <- get_centrality(graph, centrality_degree(mode = 'out'))
plot_centrality(graph_influenced_degree, centrality_metric = 'out-degree') %>%
  plot_save(filename = 'plots/degree_out.png')
plot_most_influent_languages(graph_influenced_degree, centrality_metric = 'out-degree') %>%
  plot_save(filename = 'plots/hist_degree_out.png')
#---------------------------------------------------------------------------------------------

# Clustering
graph %>%
  get_clusters(group_infomap()) %>%
  plot_clusters('infomap', 1:13) %>% 
  plot_save(height = 5000, filename = 'plots/communities.png')
