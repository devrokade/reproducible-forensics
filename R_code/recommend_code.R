library(httr)
library(rvest)
library(tidyverse)
library(assertthat)
library(here)
library(polite)
library(RSelenium)

source(here("R_code", "funcs.R"))

# start local server
remDr <- rsDriver(browser='chrome', port = 4567L, chromever = "104.0.5112.79")
browser <- remDr$client

start_url <- "https://onlinelibrary.wiley.com/doi/abs/10.1111/1556-4029.13950"
site_url <- "https://onlinelibrary.wiley.com"
doi_prefix <- "/doi/abs/"
# current_doi <- str_replace(start_url, paste0(site_url, "/doi/abs/"), "")
current_url <- start_url

wileyPageSourceList <- list()
wiley_check_list <- list()
wiley_check_list[[current_url]] <- list(count = 1,
                                        to_be_checked = TRUE)
max_iter <- 600
for (i in 1:max_iter) {
  Sys.sleep(3)
  # current_url <- paste0(site_url, doi_prefix, current_doi)

  if (is.null(wileyPageSourceList[[current_url]])) {
    browser$open()
    result <- extract_page_source(browser,
                                  current_url,
                                  wiley_check_list)
    wileyPageSourceList[[current_url]] <- result$tb
    wiley_check_list <- result$updated_check_list
    browser$close()
  }

  # update current_doi
  next_candidates <- which(sapply(wiley_check_list, function(e) {e$to_be_checked}))
  if (length(next_candidates) == 0) break
  current_url <- names(next_candidates)[1]
}

saved_results <- list(wiley_check_list = wiley_check_list,
                      wileyPageSourceList = wileyPageSourceList)
saveRDS(saved_results, "results/wiley_Sep01_600iter.rds")

# browser$navigate("https://onlinelibrary.wiley.com/doi/abs/10.1111/1556-4029.12759")
# browser$navigate("https://onlinelibrary.wiley.com/doi/abs/10.1111/1556-4029.13950")
















browser$close()
browser$open()
remDr$server$stop()
