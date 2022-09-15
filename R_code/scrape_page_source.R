library(httr)
library(rvest)
library(tidyverse)
library(assertthat)
library(here)
library(polite)
library(RSelenium)

source(here("R_code", "funcs.R"))

# tmp_list <- readRDS("results/wiley_Aug31_500iter.rds")
tmp_list <- readRDS("results/wiley_Sep01_600iter.rds")
wiley_check_list <- tmp_list$wiley_check_list
wileyPageSourceList <- tmp_list$wileyPageSourceList

page_sources_tb <- do.call(rbind, wileyPageSourceList)

page_counts <- sapply(wiley_check_list, function(i) i$count )
page_counts_df <- data.frame(url = names(page_counts), counts = page_counts)
page_sources_tb <- page_sources_tb %>% left_join(page_counts_df, by = "url")

page_sources_tb <- page_sources_tb %>% mutate(
  html = purrr::map(page_source, function(ps) { read_html(ps[[1]]) })
)

# sample_html <- page_sources_tb$html[[1]]
#
# sample_html %>% html_element(".citation__title") %>% html_text()
# sample_html %>% html_element(".accordion-tabbed") %>% html_text()
# sample_html %>% html_element("#section-1-en p") %>% html_text()
# sample_html %>% html_element(".cover-image__details") %>% html_elements("p") %>%
#   html_text()
# sample_html %>% html_element(".doi-access-container") %>% html_text()

page_sources_tb <- page_sources_tb %>% mutate(
  title = purrr::map_chr(html, function(html) {
    html %>% html_element(".citation__title") %>% html_text()
  }),
  authors = purrr::map_chr(html, function(html) {
    html %>% html_element(".accordion-tabbed") %>% html_text()
  } ),
  abstract = purrr::map_chr(html, function(html) {
    html %>% html_element("#section-1-en p") %>% html_text()
  }),
  volumn_info = purrr::map(html, function(html) {
    html %>% html_element(".cover-image__details") %>%
      html_elements("p") %>%
      html_text()
  }),
  paper_type = purrr::map_chr(html, function(html) {
    html %>% html_element(".doi-access-container") %>% html_text()
  })
)

unique(page_sources_tb$paper_type)
page_sources_tb$url %>% str_extract("doi/abs/.+") %>% str_remove("doi/abs/")


page_sources_tb$url[page_sources_tb$paper_type == 'ORIGINAL MANUSCRIPT']

page_sources_tb$doi[page_sources_tb$doi %>% str_detect("https://")]

sum(is.na(unlist(page_sources_tb$title)))
tt2 <- page_sources_tb[is.na(unlist(page_sources_tb$title)), ]


























