
# sentences ----
# NOTE: This is for 2 vizzes.
sentences_section_n <-
  sentences_filt %>%
  count(year, section_label) %>%
  # NOTE: This `n` is for count of content.
  inner_join(section_rngs_n %>% select(-matches("^n$")))
sentences_section_n

# NOTE: Not currently used.
section_n <-
  left_join(
    sentences_section_n %>%
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

sentences_n <-
  sentences_filt %>%
  # NOTE: Could just do a distinct for this first `count()`.
  count(year, section_label, sentence) %>%
  count(section_label, sentence, sort = TRUE)
sentences_n

sentences_n_max <-
  sentences_n %>%
  group_by(section_label) %>%
  filter(n == 3) %>%
  ungroup() %>%
  select(section_label, sentence)
sentences_n_max

sentences_n_max %>% count(section_label)
sentences_n_max %>% filter(section_label == 'Analysis')
sentences_n_max %>% filter(sentence %>% str_detect('QQ'))

# ngrams ----
ngrams <-
  bind_rows(
    unnest_tokens_ngrams(lines_redux, 6),
    unnest_tokens_ngrams(lines_redux, 8),
    unnest_tokens_ngrams(lines_redux, 10)
  )
ngrams

# ngrams %>% filter(line_type == 'content') %>% filter(ngram %>% str_detect(zones_rgx, negate = FALSE))

# NOTE: Although `drop_words_generic` (and `anti_join(stop_words)` were already called
# in order to create `words_filt`, `drop_ngrams_generic()` should also be called here.
ngrams_filt <-
  ngrams %>%
  drop_ngrams_generic()
ngrams_filt

ngrams_n_maxk_allyrs <-
  ngrams_filt %>%
  filter(k == max(k)) %>%
  count(year, section_label, ngram, name = 'n_year') %>%
  count(ngram, section_label, sort = TRUE)
ngrams_n_maxk_allyrs

ngrams_n_maxk_allyrs_filt <-
  ngrams_n_maxk_allyrs %>%
  filter(n == 3) %>%
  select(-n)
ngrams_n_maxk_allyrs_filt

n_k_max <-
  # ngrams_filt %>%
  ngrams %>%
  count(k) %>%
  summarise(temp = max(n)) %>%
  pull(temp)
n_k_max

ngrams_filt_n <-
  ngrams_filt %>%
  select(year, ngram, k) %>%
  add_count(k, name = 'n_k') %>%
  mutate(n_k_factor =  max(n_k) / n_k)
ngrams_filt_n

ngrams_filt_n %>% distinct(k, n_k_factor)

ngrams_tfidf <-
  ngrams_filt %>%
  count(year, ngram) %>%
  tidytext::bind_tf_idf(term = ngram, document = year, n = n)
ngrams_tfidf

# NTOE: This attempt to answer the question 'What were the most 'unique' ngrams?'
# NOTE: Larger n-grams are more likely to have higher TFIDF, so need to account
# for this by dividing by `n_k_factor`.
ngrams_tfidf_aug <-
  ngrams_tfidf %>%
  left_join(
    ngrams_filt_n %>% distinct()
  ) %>%
  # distinct() %>%
  mutate(tf_idf_adj = tf_idf / n_k_factor) %>%
  arrange(desc(tf_idf))
ngrams_tfidf_aug

set.seed(42)
ngrams_tfidf_top_sample <-
  ngrams_tfidf_aug %>%
  group_by(year, k) %>%
  top_n(3, wt = tf_idf_adj) %>%
  sample_n(3, wt = tf_idf_adj) %>%
  ungroup() %>%
  select(year, n, k, ngram) %>%
  arrange(year, n, k, ngram)
ngrams_tfidf_top_sample

# What were the most used exteneded ngrams.?
# (In this case, 'extended' = ngram of X tokens.)
ngrams_tf_maxk <-
  ngrams_tfidf_aug %>%
  filter(k == max(k)) %>%
  group_by(k, ngram) %>%
  summarise(n = sum(n), tf = mean(n * tf)) %>%
  ungroup() %>%
  arrange(desc(tf))
ngrams_tf_maxk

#+ ngrams_tf_maxk_top-1, include=T, eval=T, echo=T
ngrams_tf_maxk_top <-
  ngrams_tf_maxk %>%
  select(n, k, ngram) %>%
  filter(n == max(n))
ngrams_tf_maxk_top

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
