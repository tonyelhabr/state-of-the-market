
library("tidyverse")
# library("tokenizers")
# library("pdftools")

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
    year = path %>% str_extract("201[6-8]") %>% as.integer()
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

text_words <-
  text_sentences %>%
  tidytext::unnest_tokens(word, sentence, token = "words") %>%
  mutate_at(vars(word), stringi::stri_unescape_unicode)
text_words

# stop_words <- stopwords::data_stopwords_snowball["en"] %>% unlist() %>% tibble(word = .)
stop_words <- tidytext::stop_words

text_words_filt <- text_words %>% anti_join(stop_words)
words_n <-
  text_words_filt %>%
  count(year, word, sort = TRUE, name = "n")
words_n

words_frac_bydoc <-
  words_n %>%
  left_join(text_words_filt %>% group_by(year) %>% summarise(n_doc = n())) %>%
  mutate(word_frac_doc = n / n_doc)
words_frac_bydoc

words_frac_byword <-
  words_n %>%
  left_join(text_words_filt %>% group_by(word) %>% summarise(n_word = n())) %>%
  mutate(word_frac_total = n / n_word)
words_frac_byword
