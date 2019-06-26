
# sents_redux_wo_delim ----
# NOTE: This is for 2 vizzes.
sents_section_n <-
  sents_redux_wo_delim_filt %>%
  count(year, section_label) %>%
  # NOTE: This `n` is for count of content.
  inner_join(section_rngs_n %>% select(-matches("^n$")))
sents_section_n

# NOTE: Not currently used.
section_n <-
  left_join(
    sents_section_n %>%
      select(year, section_label, n_pages, n_sentences = n),
    section_rngs_n %>%
      select(year, section_label, n_pages, n_content = n)
  )
section_n_cors <-
  section_n %>%
  select_if(is.numeric) %>%
  select(-year) %>%
  corrr::correlate(quiet = TRUE)
section_n_cors

# section_n %>%
#   select(-n_pages) %>%
#   rename(n = n_sentences, n_pages = n_content) %>%
#   do_visualize_x_vs_y(label_arrw = 'dummy')

sents_n <-
  sents_redux_wo_delim_filt %>%
  # NOTE: Could just do a distinct for this first `count()`.
  count(year, section_label, sentence) %>%
  count(section_label, sentence, sort = TRUE)
sents_n

sents_n_max <-
  sents_n %>%
  group_by(section_label) %>%
  filter(n == 3) %>%
  ungroup() %>%
  select(section_label, sentence)
sents_n_max

sents_n_max %>% count(section_label)
sents_n_max %>% filter(section_label == 'Analysis')
sents_n_max %>% filter(sentence %>% str_detect('QQ'))
