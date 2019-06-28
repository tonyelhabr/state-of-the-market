

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
  create_kable_md()
toc_content_n1_show

summ_toc_content_n1 <-
  toc_content_n1 %>%
  group_by(section_label) %>%
  summarise(idx_max = max(idx)) %>%
  ungroup()
summ_toc_content_n1
