
sents_idx <-
  sents_redux_wo_delim %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  select(year, idx, everything())
sents_idx

years_grid <-
  crossing(
    year1 = years,
    year2 = years
  ) %>%
  filter(year2 > year1) %>%
  arrange(year1, year2) %>%
  mutate(idx_grp = row_number())
years_grid

sents_sim_nest <-
  years_grid %>%
  mutate(data = list(sents_idx)) %>%
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
sents_sim_nest

sents_sim <-
  inner_join(
    sents_sim_nest %>%
      unnest(data) %>%
      filter(year == year1) %>%
      select(-year) %>%
      rename(idx1 = idx, sentence1 = sentence),
    sents_sim_nest %>%
      unnest(sim)
  ) %>%
  inner_join(
    sents_sim_nest %>%
      unnest(data) %>%
      filter(year == year2) %>%
      select(-year) %>%
      rename(idx2 = idx, sentence2 = sentence)
  ) %>%
  mutate(grp_label = sprintf('%d vs. %d', year1, year2)) %>%
  mutate_at(vars(grp_label), factor) %>%
  select(
    # idx_grp,
    grp_label,
    year1,
    year2,
    section_label,
    idx1,
    idx2,
    sim_max,
    sentence1,
    sentence2
  )
sents_sim

summ_sents_section_sim <-
  sents_sim %>%
  group_by(grp_label, section_label) %>%
  summarise(sim_max_mean = sim_max %>% mean()) %>%
  ungroup() %>%
  group_by(section_label) %>%
  mutate(sim_max_mean_all = sim_max_mean %>% mean()) %>%
  ungroup() %>%
  group_by(grp_label) %>%
  mutate(idx = row_number(sim_max_mean_all)) %>%
  ungroup() %>%
  arrange(grp_label, idx)
summ_sents_section_sim
# x1 <- sents_sim %>% slice(c(1:2)) %>% pull(sentence1)
# x2 <- sents_sim %>% slice(c(1:2)) %>% pull(sentence2)
# it1 <- x1 %>% text2vec::itoken()
# # it1
# it2 <- x2 %>% text2vec::itoken()
# # it2
# x <- c(x1, x2) %>% unique()
# it <- x %>% text2vec::itoken()
# # it
# v <- it %>% text2vec::create_vocabulary()
# # v
# vectorizer <- v %>% text2vec::vocab_vectorizer()
# # vectorizer
# dtm1 <- it1 %>% text2vec::create_dtm(vectorizer = vectorizer)
# # dtm1[1:10, 1:10]
# dtm2 <- it2 %>% text2vec::create_dtm(vectorizer = vectorizer)
# # dtm2[1:10, 10:12]
# sim <- text2vec::sim2(dtm1, dtm2)
#
# tmat_sim <- as(sim, "TsparseMatrix")
# tmat_sim
# sents_sim %>% group_by(idx_grp) %>% select(sim_max) %>% skimr::skim()
# sents_sim %>% ggplot() + aes(x = sim_max) + geom_histogram() + facet_wrap(~idx_grp)

sim_data_ex <-
  tibble(
    x1 = 'abcde',
    x2 = c('abcde', 'zbcde', 'zycde', 'fghij', 'baecd', 'aabcde', 'aaabcde', 'abcdef', 'abcd'),
    description =
      c('Identical strings.',
        'One different character ("z" in `String 2` instead of "a").',
        'Two different characters  ("z" and "y" in `String 2` instead of "a" and "b").',
        'All different characters.',
        'Different ordering of characters, but identical characters.',
        'Repeated characters ("a" in `String 2`), one-character difference in string lengths.',
        'Repeated characters ("a" twice in `String 2`), two-character difference in string lengths.',
        'Same characters, one additional character ("z" in `String 2`) in one string.',
        'Same characters, one additional character ("e" in `String 1`) in one string.'
      )
  ) %>%
  mutate(idx = row_number())
sim_data_ex
sim_ex <-
  sim_data_ex %>%
  tidystringdist::tidy_stringdist(x1, x2) %>%
  mutate(cosine_sim = 1 - cosine)
sim_ex

sim_ex_show <-
  sim_ex %>%
  mutate_at(vars(cosine_sim), ~round(., 2) %>% sprintf('%.2f', .)) %>%
  select(
    `Case` = idx,
    `Description` = description,
    `String 1` = x1,
    `String 2` = x2,
    `Cosine Similarity` = cosine_sim
  ) %>%
  create_kable_md()
sim_ex_show
