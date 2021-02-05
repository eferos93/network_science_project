# Title     : Data Wrangling
# Objective : Get the data and tidy it
# Created by: eferos93
# Created on: 02/02/21

library(pdftools)
library(stringr)
library(dplyr)

unclean_data <-
  pdf_text("data/rsif20150249supp1.pdf")

data_as_vector <- scan(text=unclean_data, what=character(0), sep='\n', skip = 41)
data_as_vector <- data_as_vector[!grepl("^#", data_as_vector)]

remove_spaces_and_quotes <- function(data_as_vector, nodes_or_edges) {
  data_as_vector[grepl(paste0("^", nodes_or_edges), data_as_vector)] %>%
  sapply(FUN = function(line) { str_replace_all(line, pattern = paste0(nodes_or_edges, ' '), '') }, USE.NAMES = FALSE) %>%
  sapply(FUN = function(line) { str_replace_all(line, pattern = '\"', '') }, USE.NAMES = FALSE) %>%
  sapply(FUN = function(line) { str_replace_all(line, pattern = '^\\s+', '') }, USE.NAMES = FALSE) %>%
  sapply(FUN = function(line) { str_replace_all(line, pattern = '\\s+$', '') }, USE.NAMES = FALSE) %>%
  sapply(FUN = function(line) { str_replace_all(line, pattern = '\\s+', ' ') }, USE.NAMES = FALSE)
}

nodes <- remove_spaces_and_quotes(data_as_vector, 'Year')
edges <- remove_spaces_and_quotes(data_as_vector, 'Cite')

programming_languages_tibble <- tibble(
  name = sapply(nodes, FUN = function (line) { str_extract(line, pattern = '^\\S+') }, USE.NAMES = FALSE),
  year = sapply(nodes, FUN = function (line) { str_extract(line, pattern = '\\S+$') }, USE.NAMES = FALSE)
)

influences_tibble <- tibble(
  #is influenced
  from = sapply(edges, FUN = function (line) { str_extract(line, pattern = '^\\S+') }, USE.NAMES = FALSE),
  #by
  to = sapply(edges, FUN = function (line) { str_extract(line, pattern = '\\S+$') }, USE.NAMES = FALSE)
)

rm(edges)
rm(nodes)
rm(data_as_vector)
rm(unclean_data)