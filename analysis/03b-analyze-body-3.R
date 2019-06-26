
words_n <-
  # words_aug %>%
  words_filt %>%
  count(year, word, sort = TRUE)
words_n

# NOTE: 'Manual' tfidf
# FIXME?: `words_aug` or `words_filt`?
words_frac <-
  words_n %>%
  left_join(words_filt %>% group_by(year) %>% summarise(n_doc = n())) %>%
  left_join(words_filt %>% group_by(word) %>% summarise(n_word = n())) %>%
  mutate(
    word_frac_doc = n / n_doc,
    word_frac_total = n / n_word
  ) %>%
  arrange(desc(word_frac_doc))
words_frac

words_frac_filt <-
  words_frac %>%
  # filter(n_word > 5) %>%
  filter(n_word > 5, word_frac_total > 0.75) %>%
  arrange(desc(word_frac_total))
words_frac_filt

words_tfidf <-
  words_n %>%
  tidytext::bind_tf_idf(word, year, n) %>%
  arrange(desc(tf_idf))
words_tfidf

words_section_n <-
  words %>%
  count(year, word, section_label, sort = TRUE)
words_section_n

words_section_tfidf <-
  words_section_n %>%
  # filter(year == 2018) %>%
  tidytext::bind_tf_idf(word, section_label, n) %>%
  arrange(desc(tf_idf))
words_section_tfidf

words_section_tfidf_filt <-
  words_section_tfidf %>%
  filter(n > 2) %>%
  filter(tf_idf > 0)
words_section_tfidf_filt

summ_words_section_tfidf_filt  <-
  words_section_tfidf_filt %>%
  group_by(section_label) %>%
  summarise(tf_idf_mean = mean(tf_idf)) %>%
  ungroup() %>%
  arrange(tf_idf_mean) %>%
  # mutate_at(vars(section_label), forcats::fct_inorder) %>%
  # NOTE: `idx` is for the plot.
  mutate(idx = row_number() %>% as.character())
summ_words_section_tfidf_filt

# summ_words_section_tfidf_filt_2 <-
#   words_section_tfidf_filt %>%
#   group_by(section_label) %>%
#   averagize_at('tf_idf') %>%
#   ungroup() %>%
#   arrange(desc(tf_idf_mean))
# summ_words_section_tfidf_filt_2

words_tfidf_filt <-
  words_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(
    year = year %>% factor(),
    word = word %>% factor(levels = rev(unique(word)))
  ) %>%
  group_by(year) %>%
  top_n(10, wt = tf_idf) %>%
  ungroup()
words_tfidf_filt
