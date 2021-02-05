# Title     :
# Objective : TODO
# Created by: eferos93
# Created on: 05/02/21

source('clean_data.R')
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)

graph <- tbl_graph(nodes = programming_languages_tibble,
                   edges = influences_tibble, directed = TRUE)