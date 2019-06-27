

lines_raw <-
  body_pages %>%
  unnest_pages()
lines_raw

body <-
  lines_raw %>%
  group_by(year) %>%
  summarise(
    lines = paste(line, collapse = ' ', sep = '')
  )
body

sents_raw_idx <-
  body %>%
  tidytext::unnest_tokens(
    output = sentence,
    input = lines,
    token = 'sentences'
  ) %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  select(year, idx, everything())
sents_raw_idx


sents_raw_sim_nest <-
  years_grid %>%
  mutate(data = list(sents_raw_idx)) %>%
  group_by(idx_grp, year1, year2) %>%
  mutate(data = purrr::map(data, ~filter(.x,  year %in% c(year1, year2)))) %>%
  mutate(
    x1 = purrr::map(data, ~filter(.x,  year == year1) %>% pull()),
    x2 = purrr::map(data, ~filter(.x,  year == year2) %>% pull())
  ) %>%
  ungroup() %>%
  mutate(
    sim = purrr::map2(x1, x2, compute_token_sim)
  )
sents_raw_sim_nest


sents_raw_sim <-
  inner_join(
    sents_raw_sim_nest %>%
      unnest(data) %>%
      filter(year == year1) %>%
      select(-year) %>%
      rename(idx1 = idx, sentence1 = sentence),
    sents_raw_sim_nest %>%
      unnest(sim)
  ) %>%
  inner_join(
    sents_raw_sim_nest %>%
      unnest(data) %>%
      filter(year == year2) %>%
      select(-year) %>%
      rename(idx2 = idx, sentence2 = sentence)
  ) %>%
  select(idx_grp, year1, year2, idx1, idx2, sim_max, sentence1, sentence2)



# Drop the executive summary part for comparison purposes.
sents_raw_sim <-
  sents_raw_sim %>%
  mutate(
    is_body =
      case_when(
        year1 == 2016 & idx1 < 354 ~ FALSE,
        year1 == 2017 & idx1 < 405 ~ FALSE,
        TRUE ~ TRUE
      )
  ) %>%
  filter(is_body) %>%
  select(-is_body)
sents_raw_sim %>% ggplot() + aes(x = sim_max) + geom_histogram() + facet_wrap(~idx_grp)
sents_raw_sim %>% group_by(idx_grp) %>% select(sim_max) %>% skimr::skim()
