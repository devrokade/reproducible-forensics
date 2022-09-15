library(httr)
library(polite)
library(rvest)
library(tidyverse)
library(assertthat)
library(here)

i_am("R_code/r_code.R")

source(here("R_code", "funcs.R"))

year_volumn <- c("2020/65/", "2021/66/")
issue <- 1:6
issue_time <- lapply(year_volumn, function(yv, issue) {
  paste0(yv, issue)
}, issue = 1:6) %>% unlist()

urls <- paste0("https://onlinelibrary.wiley.com/toc/15564029/", issue_time)

JFS_papers1 <- tibble(
  url = urls[-(1:3)],
  paper_path = as.list(c(4, 2, 2, 3, 3, 2, 4, 2, 3))
) %>% mutate(
  data = purrr::map2(url, paper_path, get_paper_info)
)

JFS_papers2 <- tibble(
  url = urls[1:3],
  paper_path = list(5:10, 3:9, 4:10)
) %>% mutate(
  data = purrr::map2(url, paper_path, get_paper_info2)
)

JFS_papers <- rbind(JFS_papers2, JFS_papers1)

# saveRDS(JFS_papers, here("data", "JFS_2020-2021.rds"))

JFS_papers <- readRDS(here("data", "JFS_2020-2021.rds"))
papers <- do.call(rbind, JFS_papers$data)


#main-content :nth-child(4) .page-range span:nth-child(2)























