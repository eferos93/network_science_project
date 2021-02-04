# Title     : TODO
# Objective : TODO
# Created by: eferos93
# Created on: 02/02/21

library(dplyr)
library(pdftools)
library(stringr)


unclean_data <-
  pdf_text("https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsif.2015.0249&file=rsif20150249supp1.pdf")

data_as_vector <- scan(text=unclean_data, what=character(0), sep='\n', skip = 41)
data_as_vector <- data_as_vector[!grepl("^#", data_as_vector)]

remove_spaces_and_quotes <- function(data_as_vector, nodes_or_edges) {
  data_as_vector[grepl(paste0("^", nodes_or_edges), data_as_vector)] %>%
  sapply(FUN = function(row) { str_replace_all(row, pattern = paste0(nodes_or_edges, ' '), '') }, USE.NAMES = FALSE) %>%
  sapply(FUN = function(row) { str_replace_all(row, pattern = '\"', ' ') }, USE.NAMES = FALSE) %>%
  sapply(FUN = function(row) { str_replace_all(row, pattern = '^\\s+', '') }, USE.NAMES = FALSE) %>%
  sapply(FUN = function(row) { str_replace_all(row, pattern = '\\s+$', '') }, USE.NAMES = FALSE) %>%
  sapply(FUN = function(row) { str_replace_all(row, pattern = '\\s+', ', ') }, USE.NAMES = FALSE)
}

nodes <- remove_spaces_and_quotes(data_as_vector, 'Year')
edges <- remove_spaces_and_quotes(data_as_vector, 'Cite')

nodes_tibble <- tibble(
  programming_language = sapply(nodes, FUN = function (row) { str_extract(row, pattern = '^\\w+') }, USE.NAMES = FALSE),
  year = sapply(nodes, FUN = function (row) { str_extract(row, pattern = '\\w+$') }, USE.NAMES = FALSE)
)

edges_tibble <- tibble(
  is_influenced = sapply(edges, FUN = function (row) { str_extract(row, pattern = '^\\w+') }, USE.NAMES = FALSE),
  by = sapply(edges, FUN = function (row) { str_extract(row, pattern = '\\w+$') }, USE.NAMES = FALSE)
)