library(httr)
library(rvest)
library(tidyverse)
library(assertthat)
library(here)
library(polite)
library(RSelenium)

source(here("R_code", "funcs.R"))

url <- "https://onlinelibrary.wiley.com/doi/abs/10.1111/1556-4029.13950"

session <- bow(url, force = TRUE)

# download JAVA SE
# https://www.oracle.com/java/technologies/downloads/#jdk18-mac
remDr <- rsDriver(browser='chrome', port = 4567L, chromever = "104.0.5112.79")
browser <- remDr$client
# browser$open()
browser$navigate(url)

browser$close()
remDr$server$stop()

pagesource <- browser$getPageSource()
html <- read_html(pagesource[[1]])

html %>%
  html_elements(".creative-work__title") %>%
  html_element("a") %>%
  html_attr("href") %>%
  str_replace("/doi/full/", "")

html %>%
  html_elements(".creative-work__title") %>%
  html_text()

html %>%
  html_elements(".creative-work__title") %>%
  html_element("a") %>%
  html_attr("href")

html %>%
  html_elements(".creative-work")

scrape(session) %>%
  html_element("#pane-pcw-related")

scrape(session) %>%
  html_element(".creative-work__title , .comma") %>% html_element(".comma__list")

scrape(session) %>%
  html_element(".article-row-right .tabs__wrapper") %>%
  html_element("div") %>%
  html_element("div") %>%
  html_element("#pane-pcw-related")


scrape(session) %>%
  html_element(".tabs__wrapper") %>% html_element("div")

scrape(session) %>%
  html_nodes( ".creative-work__title" ) %>% html_node("a") %>% html_text()

scrape(session) %>%
  html_nodes(".grid-item:nth-child(1) .creative-work__title")

scrape(session) %>%
  html_node(".grid-item:nth-child(1)")

scrape(session) %>%
  html_element('//*[contains(concat( " ", @class, " " ), concat( " ", "grid-item", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "creative-work", " " ))]')

scrape(session) %>%
  html_elements("li")






