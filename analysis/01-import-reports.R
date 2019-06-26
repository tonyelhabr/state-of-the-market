

paths <-
  years %>%
  purrr::map_chr(download_sotmreport)
paths

paths <-
  list.files(
    path = "data-raw",
    pattern = "pdf$",
    full.names = TRUE
  )
paths

paths_info <-
  paths %>%
  tibble(path = .) %>%
  mutate(
    year = path %>% str_extract("201[0-8]") %>% as.integer()
  ) %>%
  select(year, path)
paths_info

pages_nest <-
  paths_info %>%
  mutate(
    page = purrr::map(path, ~pdftools::pdf_text(.))
  ) %>%
  select(-path)
pages_nest

pages <-
  pages_nest %>%
  unnest(page) %>%
  group_by(year) %>%
  mutate(idx_page = row_number()) %>%
  ungroup()
pages

pages_n <-
  pages_nest %>%
  mutate(n_pages = purrr::map_int(page, ~length(.x))) %>%
  select(-page)
pages_n

# NOTE: These are the same across all reports.
toc_page_start <- 3L
toc_page_end <- 7L
toc_pages <-
  pages %>%
  filter(idx_page >= 3L, idx_page <= (toc_page_end - 1))
toc_pages

body_pages <-
  pages %>%
  filter(idx_page >= toc_page_end)
body_pages
