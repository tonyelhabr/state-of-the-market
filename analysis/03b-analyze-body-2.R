

# ngrams ----
ngrams <-
  bind_rows(
    unnest_tokens_ngrams(lines_redux_w_delim, 6),
    unnest_tokens_ngrams(lines_redux_w_delim, 8),
    unnest_tokens_ngrams(lines_redux_w_delim, 10)
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
