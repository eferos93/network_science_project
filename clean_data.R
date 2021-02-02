# Title     : TODO
# Objective : TODO
# Created by: eferos93
# Created on: 02/02/21

library(dplyr)
library(pdftools)
library(stringr)


unclean_data <-
  pdf_text("https://royalsocietypublishing.org/action/downloadSupplement?doi=10.1098%2Frsif.2015.0249&file=rsif20150249supp1.pdf")

data_as_vector <- scan(text=unclean_data, what='', sep='\n', skip = 41)
data_as_vector <- data_as_vector[!grepl("^#", data_as_vector)]
nodes <- data_as_vector[grepl("^Year", data_as_vector)] %>%
  sapply(FUN = function (row) {str_replace_all(row, pattern = '\"', ' ')}, USE.NAMES = FALSE) %>%
  sapply(FUN = function (row) {str_replace_all(row, pattern = 'Year ', '')}, USE.NAMES = FALSE) %>%
  sapply(FUN = function (row) {str_replace_all(row, pattern = '^\\s+', '')}, USE.NAMES = FALSE) %>%
  sapply(FUN = function (row) {str_replace_all(row, pattern = '\\s+', ', ')}, USE.NAMES = FALSE)

edges <- data_as_vector[grepl("^Cite", data_as_vector)] %>%
  sapply(FUN = function (row) {str_replace_all(row, pattern = '\"', " ")}, USE.NAMES = FALSE) %>%
  sapply(FUN = function (row) {str_replace_all(row, pattern = 'Cite ', '')}, USE.NAMES = FALSE) %>%
  sapply(FUN = function (row) {str_replace_all(row, pattern = '^\\s+', '')}, USE.NAMES = FALSE) %>%
  sapply(FUN = function (row) {str_replace_all(row, pattern = '\\s+', ', ')}, USE.NAMES = FALSE)



