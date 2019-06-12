
# library("tokenizers")
# library("pdftools")
filter <- dplyr::filter

.paths <-
  fs::dir_ls(
    path = "data-raw",
    regexp = "pdf$"
  )
paths <-
  .paths %>%
  as.character() %>%
  tibble(path = .) %>%
  mutate(
    year = path %>% str_extract("201[0-8]") %>% as.integer()
  ) %>%
  select(year, path)

idx_page_start <- 7L
text_pages_nest <-
  paths %>%
  # slice(1) %>%
  mutate(
    page = purrr::map(path, ~pdftools::pdf_text(.) %>% .[-c(1L:(idx_page_start - 1L))])
  ) %>%
  select(-path)
text_pages_nest

text_pages <-
  text_pages_nest %>%
  unnest(page) %>%
  group_by(year) %>%
  mutate(idx_page = (row_number() + idx_page_start) %>% as.integer()) %>%
  ungroup()
text_pages

text_sentences <-
  text_pages %>%
  mutate(
    sentences = purrr::map(page, ~{
      .x %>%
        str_squish() %>%
        str_split(pattern = "[.]\\s+") %>%
        unlist() %>%
        enframe(name = "idx_sentence", value = "sentence")
    })
  ) %>%
  select(-page) %>%
  unnest(sentences)
text_sentences

# TODO:
# + Add "sections", possibly corresponding to the first sentence afer each page line.
# + Remove page lines.
# + Label figure lines?

# + In 2016, actual page 1 is `idx_page = 29`. For 2017 it is 31. For 2018 it is 30.
# + Page footers are noted by "2016 State of the Market Report | xxi /" or
# "xx | 2016 State of the Market Report /"
# on alternating pages.
# + The first line for each new section is parsed imperfectly. For example,
# on page 59 in 2016. "Day-Ahead Market Performance II" and
# "DAY-AHEAD MARKET PERFORMANCE ERCOT's day-ahead ..." are parsed as two lines.
# It should be "Day-Ahead Market Performance" for the page header and
# "II DAY-AHEAD MARKET PERFORMANCE" for the section header,
# then "ERCOT's day-ahead ..." for the first sentence of the body.
text_sentences %>%
  mutate(
    is_page_num = sentence %>%  str_detect("^[0-9]{1,4}$")
  ) %>%
  filter(is_page_num)

text_sentences %>%
  filter(sentence %>% str_detect("Figure [0-9]+[:]"))

text_words <-
  text_sentences %>%
  tidytext::unnest_tokens(word, sentence, token = "words") %>%
  mutate_at(vars(word), ~stringi::stri_unescape_unicode(.) %>% str_remove_all(",")) %>%
  mutate(word_num = word %>% as.numeric())
text_words

# stop_words <- stopwords::data_stopwords_snowball["en"] %>% unlist() %>% tibble(word = .)
stop_words <- tidytext::stop_words

text_words_aug <-
  text_words %>%
  anti_join(stop_words) %>%
  # mutate_at(vars(word), ~dplyr::if_else(. == year, "[year]", .))
  mutate_at(
    vars(word_num),
    list(word_clean = ~case_when(
      is.na(.) ~ NA_character_,
      . == year ~ "[year]",
      . == (year + 1) ~ "[year+1]",
      . == (year - 1) ~ "[year-1]",
      . == (year + 2) ~ "[year+2]",
      . == (year - 2) ~ "[year-2]",
      . >= (year + 3) && . <= (year + 6) ~ "[year>+3]",
      . <= (year - 3) && . >= (year - 6) ~ "[year<-3]",
      (round(. / 1e1) * 1e1) == 0 ~ "[x<10]",
      (round(. / 1e2) * 1e2) == 0 ~ "[10<x<100]",
      (round(. / 1e3) * 1e3) == 0 ~ "[100<x<1k]",
      (round(. / 1e6) * 1e6) == 0 ~ "[1k<x<1M]",
      (round(. / 1e9) * 1e9) == 0 ~ "[1M<x<1B]",
      (round(. / 1e9) * 1e9) >= 0 ~ "[x>1B]",
      TRUE ~ "[x]"
    )
    )
  ) %>%
  mutate_at(vars(word_clean), ~coalesce(., word))
text_words_aug
# text_words_aug %>% filter(word == "2,000.00")
text_words_clean <-
  text_words_aug %>%
   select(year, idx_page, idx_sentence, word = word_clean)
words_n <-
  text_words_clean %>%
  count(year, word, sort = TRUE, name = "n")
words_n

words_frac <-
  words_n %>%
  left_join(text_words_clean %>% group_by(year) %>% summarise(n_doc = n())) %>%
  left_join(text_words_clean %>% group_by(word) %>% summarise(n_word = n())) %>%
  mutate(
    word_frac_doc = n / n_doc,
    word_frac_total = n / n_word
  ) %>%
  arrange(desc(word_frac_doc))
words_frac

filter <- dplyr::filter

words_frac %>%
  # dplyr::filter(n_word > 5) %>%
  filter(n_word > 5, word_frac_total > 0.75) %>%
  arrange(desc(word_frac_total)) -> z

words_tfidf <-
  words_n %>%
  tidytext::bind_tf_idf(word, year, n) %>%
  arrange(desc(tf_idf))
words_tfidf

# Reference: fig 3.4 at https://www.tidytextmining.com/tfidf.html#tfidf
viz_tfidf <-
  words_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(
    year = year %>% factor(),
    word = word %>% factor(levels = rev(unique(word)))
  ) %>%
  group_by(year) %>%
  top_n(6) %>%
  ungroup() %>%
  ggplot() +
  aes(x = word, y = tf_idf, fill = year) +
  # geom_col(show.legend = FALSE) +
  geom_col() +
  guides(fill = FALSE) +
  facet_wrap(~year, scales = "free") +
  labs(x = NULL, y = NULL) +
  teplot::theme_te() +
  theme(
    axis.text.x = element_blank()
  ) +
  coord_flip()
viz_tfidf
