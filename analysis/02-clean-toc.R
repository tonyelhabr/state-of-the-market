

toc <-
  toc_pages %>%
  unnest_pages() %>%
  mutate_at(
    vars(line),
    ~str_to_lower(.) %>%
      str_replace_all("[.]+", "-") %>%
      str_replace_all("201[2-9]", "[year]")
  )
toc

toc_ex <-
  toc %>%
  filter(year == 2018) %>%
  filter(line %>% str_detect("review of real-time market outcomes| 1[:]"))
toc_ex

toc_aug <-
  toc %>%
  filter(line %>% str_detect("executive|contents|state\\sof\\sthe|list\\sof", negate = TRUE)) %>%
  mutate_at(
    vars(line),
    list(line_type = ~case_when(
      str_detect(., "[-]\\s[ixv]+$") ~ "summary_subsection",
      str_detect(., "^[ixv]+[-]") ~ "section",
      str_detect(., "^figure") ~ "figure",
      str_detect(., "^table") ~ "table",
      str_detect(., "^[a-z]") ~ "subsection",
    )
    )
  ) %>%
  filter(!is.na(line_type)) %>%
  mutate_at(
    vars(line),
    list(label = ~case_when(
      line_type == "summary_subsection" ~ str_replace(., "(^.*)(\\s?+[-]\\s?+)([ixv]+$)", "\\1"),
      line_type == "section" ~ str_replace(., "(^[ixv]+)([-]\\s?+)(.*)(\\s?+[-]\\s?+[0-9]+$)", "\\3"),
      line_type == "subsection" ~ str_replace(., "(^[a-z])([-]\\s?+)(.*)(\\s?+[-]\\s?+[0-9]+$)", "\\3"),
      line_type %in% c("figure", "table") ~ str_replace(., "(^[a-z]+\\s?+)([0-9]+)([:]\\s?+)(.*)(\\s?+[-]\\s?+[0-9]+$)", "\\4"),
      TRUE ~ NA_character_
    )
    )
  ) %>%
  mutate_at(vars(label), str_trim) %>%
  mutate_at(
    vars(line),
    # NOTE: `NA`s will be introduced here.
    list(index = ~case_when(
      line_type %in% c("figure", "table") ~ str_replace(., "(^[a-z]+\\s?+)([0-9]+)([:].*$)", "\\2"),
      TRUE ~ NA_character_
    )
    )
  ) %>%
  mutate_at(vars(index), as.integer) %>%
  mutate(page_num = str_replace(line, "(^.*[-]\\s?+)([ivx0-9]+$)", "\\2")) %>%
  mutate_at(
    vars(page_num),
    list(~case_when(
      line_type != "summary_subsection" ~ as.integer(.),
      TRUE ~ roman2numeric(.)
    )
    )
  ) %>%
  select(-idx_page, -idx_line, -line)
toc_aug

toc_aug_ex <-
  toc_aug %>%
  filter(year == 2018) %>%
  # filter(label %in% c("review of real-time market outcomes", "real-time market prices"))
  group_by(line_type) %>%
  filter(row_number() == first(row_number())) %>%
  ungroup() %>%
  select(
    Year = year,
    `Line Type` = line_type,
    Index = index,
    `Page Number` = page_num,
    Label = label
  ) %>%
  create_kable()
toc_aug_ex

section_rngs <-
  toc_aug %>%
  filter(line_type == "section") %>%
  group_by(year) %>%
  left_join(pages_n) %>%
  mutate(page_end = coalesce(dplyr::lead(page_num) - 1L, n_pages)) %>%
  mutate(idx_section = row_number()) %>%
  mutate_at(
    vars(label),
    ~case_when(
      idx_section == 1 ~ "RTM",
      idx_section == 2 ~ "DAM",
      idx_section == 3 ~ "Congestion",
      idx_section == 4 ~ "Supply/Demand",
      idx_section == 5 ~ "Reliability",
      idx_section == 6 ~ str_to_title(.),
      idx_section == 7 ~ "Analysis"
    )
  ) %>%
  ungroup() %>%
  # NOTE: Reverse the order for the plots.
  mutate_at(
    vars(label),
    ~forcats::fct_reorder(., idx_section)
  ) %>%
  select(year, page_start = page_num, page_end, idx_section, section_label = label)
section_rngs

section_labels <-
  section_rngs %>%
  distinct(idx_section, section_label)
section_labels

toc_sections <-
  toc_aug %>%
  filter(line_type %in% c("subsection", "figure", "table")) %>%
  fuzzyjoin::fuzzy_left_join(
    section_rngs,
    by = c(
      "year" = "year",
      "page_num" = "page_start",
      "page_num" = "page_end"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
  select(-matches("num_|[.]y")) %>%
  rename(year = year.x)
toc_sections

section_label_lvls <- section_rngs %>% pull(section_label) %>% levels()
section_label_lvls

section_rngs_n <-
  toc_sections %>%
  subset_content() %>%
  count(year, section_label) %>%
  left_join(section_rngs) %>%
  mutate(n_pages = page_end - page_start)
section_rngs_n

toc_n <-
  toc_sections %>%
  group_by(line_type, label) %>%
  add_count() %>%
  ungroup()
toc_n

toc_n %>% pull(section_label) %>% levels()

toc_content_n <- toc_n %>% subset_content()
toc_content_n

toc_n_1yr <- toc_content_n %>% filter(year == 2018)
toc_n_1yr

summ_toc_n_1yr <-
  toc_n_1yr %>%
  group_by(year, section_label) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(label = sprintf('%s - %d', section_label, n))
summ_toc_n_1yr

toc_content_n1 <-
  toc_content_n %>%
  filter(n == 1) %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  group_by(section_label) %>%
  mutate(idx_max = max(idx)) %>%
  ungroup()
toc_content_n1

toc_content_n1_show <-
  toc_content_n1 %>%
  select(
    Year = year,
    Section = section_label,
    Type = line_type,
    Label = label
  ) %>%
  create_kable()
toc_content_n1_show

summ_toc_content_n1 <-
  toc_content_n1 %>%
  group_by(section_label) %>%
  summarise(idx_max = max(idx)) %>%
  ungroup()
summ_toc_content_n1
