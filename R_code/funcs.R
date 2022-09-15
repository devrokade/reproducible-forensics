get_sections <- function(x) {
  is_section <- !is.na(x)
  section_names <- x[is_section]

  assert_that(
    length(section_names) != 0,
    is.character(section_names)
  )

  section_tibble <- tibble(section = x)
  sec_count <- section_tibble %>% fill(section) %>% group_by(section) %>% summarise(
    count = n()
  ) %>% pull(count) %>% {. - 1}

  rep(section_names, times = sec_count)
}


get_paper_info <- function(url, nth_child){

  session <- bow(url, force = TRUE)
  first_selector <- paste0(".bulkDownloadWrapper:nth-child(", nth_child, ") ")

  papers <- scrape(session) %>%
    html_nodes( paste0(first_selector, ".issue-item .issue-item h2") ) %>%
    html_text()

  links <- scrape(session) %>%
    html_nodes(paste0(first_selector, ".issue-item .issue-item")) %>%
    html_node("a") %>%
    html_attr("href") %>%
    paste0("https://onlinelibrary.wiley.com", .)

  authors <- scrape(session) %>%
    html_nodes(paste0(first_selector, ".issue-item .issue-item .comma__list")) %>%
    html_text()

  sections <- scrape(session) %>%
    html_elements(paste0(first_selector, " .issue-item")) %>%
    sapply(function(tt) {
      tt %>% html_element("h4") %>% html_text()
    }) %>%
    get_sections()

  first_published <- scrape(session) %>%
    html_nodes(paste0(first_selector, ".issue-item .issue-item .ePubDate span:nth-child(2)")) %>%
    html_text()

  year <- str_extract(url, "/\\d{4}/") %>% parse_number()
  volumn <-  str_extract(url, "/\\d{2}/") %>% parse_number()
  issue <-  str_extract(url, "/\\d{1}$") %>% parse_number()

  result <- tibble(
    year = year,
    volumn = volumn,
    issue = issue,
    paper = papers,
    author = authors,
    first_pub = first_published,
    section = sections,
    link = links
  )
  return(result)
}


get_paper_info2 <- function(url, nth_child) {
  session <- bow(url, force = TRUE)

  paper_path <- paste(paste0(".bulkDownloadWrapper:nth-child(", nth_child, ")"), collapse = ", ")

  paper_elements <- scrape(session) %>% html_elements(paper_path)

  year <- str_extract(url, "/\\d{4}/") %>% parse_number()
  volumn <-  str_extract(url, "/\\d{2}/") %>% parse_number()
  issue <-  str_extract(url, "/\\d{1}$") %>% parse_number()

  paper_list <- lapply(paper_elements, function(tt) {

    tibble(
      year = year,
      volumn = volumn,
      issue = issue,
      paper = tt %>% html_elements("h2") %>% html_text(),
      author = tt %>% html_elements(".comma__list") %>% html_text(),
      first_pub = tt %>% html_elements(".ePubDate span:nth-child(2)") %>% html_text(),
      section = tt %>% html_elements("h3") %>% html_text(),
      link = tt %>% html_elements(".issue-item .issue-item") %>% html_element("a") %>%
        html_attr("href") %>%
        paste0("https://onlinelibrary.wiley.com", .)
    )

  })

  do.call(rbind, paper_list)

}

extract_page_source <- function(browser,
                                url,
                                wiley_check_list,
                                selector = ".creative-work__title",
                                site_url = "https://onlinelibrary.wiley.com",
                                doi_prefix = "/doi/abs/") {

  browser$navigate(url)
  pagesource <- browser$getPageSource()
  html <- read_html(pagesource[[1]])

  links <- html %>%
    html_elements(".creative-work__title") %>%
    html_element("a") %>%
    html_attr("href")

  child_urls <- sapply(links, function(link) {
    if (!str_detect(link, "https")) {
      link <- paste0(site_url, link)
    }
    str_replace(link, '/doi/full/', '/doi/abs/')
  })

  # dois <- links %>% str_replace("/doi/full/", "")
  # current_doi <- str_replace(url, paste0(site_url, doi_prefix), "")

  for (child_url in child_urls) {
    if (is.null(wiley_check_list[[child_url]])) {
      wiley_check_list[[child_url]] <- list(count = 1,
                                            to_be_checked = TRUE)
    } else {
      wiley_check_list[[child_url]]$count <- wiley_check_list[[url]]$count + 1
    }
  }

  wiley_check_list[[url]]$to_be_checked <- FALSE

  result <- list(tb = tibble(url = url,
                             page_source = pagesource,
                             child_url = list(child_urls)),
                 updated_check_list = wiley_check_list)
  return( result )
}
