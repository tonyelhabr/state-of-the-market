

lines <-
  body_pages %>%
  unnest_pages() %>%
  mutate_at(vars(line), tolower)
lines

# TODO:
# + Add "sections", possibly corresponding to the first lineence afer each page line.
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
# then "ERCOT's day-ahead ..." for the first lineence of the body.

lines_aug <-
  lines %>%
  mutate_at(vars(line), ~str_remove_all(., "\\/\\s+")) %>%
  mutate_at(
    vars(line),
    list(line_type = ~case_when(
      str_detect(., "^[A-Za-z]$") ~ "section_alphanumeric_label",
      str_detect(., "^[0-9]{1,2}$") ~ "list_item_ordered",
      str_detect(., "\\\uf0b7") ~ "list_item_unordered",
      str_detect(., "Figure [0-9]+[:]") ~ "figure_label",
      str_detect(., "Figure [0-9]+[^:]") ~ "figure_explanation",
      str_detect(., "201[2-9] State of the Market Report") ~ "page_footer",
      # str_detect(., "^[-A-Z\\s]{10,}") ~ "section_header",
      # str_length(.) < 20 ~ "filler",
      TRUE ~ "content"
    )
    )
  )
lines_aug

lines_aug %>% count(year, line_type)
lines_aug %>% count(line_type)
lines_aug %>% head_reports(idx_page)

body_rngs <-
  body_pages %>%
  filter(page %>% str_detect("Executive Summary")) %>%
  group_by(year) %>%
  filter(idx_page == last(idx_page)) %>%
  ungroup() %>%
  select(-page) %>%
  rename(page_start = idx_page) %>%
  mutate_at(vars(page_start), list(~ifelse(year == 2016, . + 2L, . + 1L))) %>%
  left_join(pages_n) %>%
  mutate(page_end = n_pages - page_start + 1L) %>%
  select(year, page_start, page_end, n_pages)
body_rngs

lines_sections <-
  lines_aug %>%
  inner_join(body_rngs) %>%
  # NOTE: Get rid of the executive summary section.
  filter(idx_page >= page_start) %>%
  group_by(year) %>%
  mutate(page_num = idx_page - min(idx_page) + 1) %>%
  ungroup() %>%
  select(year, page_num, idx_line, line_type, line) %>%
  fuzzyjoin::fuzzy_left_join(
    section_rngs,
    by = c(
      "year" = "year",
      "page_num" = "page_start",
      "page_num" = "page_end"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
  select(-matches("[.]y|page_[se]")) %>%
  rename(year = year.x)
lines_sections

rgx_month_abbs <-
  month.abb %>%
  tolower() %>%
  paste_collapse_strictly()
rgx_month_abbs

rgx_month_names <-
  month.name %>%
  tolower() %>%
  paste_collapse_strictly()
rgx_month_names

rgx_month_abb1s <-
  month.abb %>%
  str_sub(end = 1) %>%
  tolower() %>%
  setdiff("a") %>%
  paste_collapse_strictly()
rgx_month_abb1s

rgx_months <-
  c(rgx_month_names, rgx_month_abbs, rgx_month_abb1s) %>%
  paste_collapse(start = "", end = "", collapse = "|")
rgx_months

# TODO(?)
# month_abbs2 <- c("m", "j", "j", "a", "s", "o", "n", "d")

zones <- c("houston", "north", "south", "west")
rgx_zones <-
  zones %>%
  purrr::map_chr(~paste_collapse(., start = "", end = "", collapse = "\\s")) %>%
  paste_collapse_loosely()
rgx_zones

words <-
  lines_sections %>%
  tidytext::unnest_tokens(
    output = word,
    input = line
  ) %>%
  mutate_at(vars(word), ~stringi::stri_unescape_unicode(.) %>% str_remove_all(",")) %>%
  mutate_at(
    vars(word),
    list(
      word_int = ~as.integer(.),
      word_num = ~as.numeric(.),
      word_month = ~dplyr::case_when(
        str_detect(., rgx_months) ~ word,
        TRUE ~ NA_character_
      )
    )
  )
words

# words %>%
#   filter(!is.na(word_date))
# words %>%
#   filter(dplyr::lag(word, 1) == "second")

# stop_words <- stopwords::data_stopwords_snowball["en"] %>% unlist() %>% tibble(word = .)
stop_words <- tidytext::stop_words

words_aug <-
  words %>%
  # anti_join(stop_words) %>%
  # mutate_at(vars(word), ~dplyr::if_else(. == year, "[year]", .))
  mutate_at(
    vars(word),
    list(~case_when(
      !is.na(word_month) ~ delimitize("month"),
      word_int == year ~ delimitize("year"),
      word_int == (year > 1) ~ delimitize("yearlead1"),
      word_int == (year < 1) ~ delimitize("yearlag1"),
      word_int == (year > 2) ~ delimitize("yearlead2"),
      word_int == (year < 2) ~ delimitize("yearlag2"),
      # NOTE: Put some bounds on these to avoid capturing ALL integers.
      word_int >= (year > 3) && . <= (year < 6) ~ delimitize("yeargtlead3"),
      word_int <= (year < 3) && . >= (year > 6) ~ delimitize("yearltlag3"),
      # NOTE: This could either be a month or a day.
      (word_int >= 1) & (word_int <= 31) ~ delimitize("monthday"),
      (round(word_num / 1e1) * 1e1) == 0 ~ delimitize("xlt1"),
      (round(word_num / 1e2) * 1e2) == 0 ~ delimitize("ltxlt100"),
      (round(word_num / 1e3) * 1e3) == 0 ~ delimitize("100ltxlt1k"),
      (round(word_num / 1e6) * 1e6) == 0 ~ delimitize("1kltxlt1M"),
      (round(word_num / 1e9) * 1e9) == 0 ~ delimitize("1Mltxlt1B"),
      (round(word_num / 1e9) * 1e9) >= 0 ~ delimitize("xgt1B"),
      !is.na(word_num) ~ delimitize("x"),
      # NOTE: "a" can also be a month label (for April/August), but it's also prevalently
      # used as an adjective, so its problemmatic.
      # Maybe just leave this filtering for later?
      # word %in% c("j", "f", "m", "s",  "o", "n", "d") ~ "QQmonthlabelQQ",
      # str_detect(., rgx_months) ~ "QQmonthQQ",
      # str_detect(., rgx_zones) ~ "QQzoneQQ",
      TRUE ~ word
    )
    )
  )
words_aug

words_filt <-
  words_aug %>%
  drop_words_generic() %>%
  anti_join(stop_words)
words_filt

lines_redux <-
  words_aug %>%
  recreate_lines_from_words()
lines_redux

lines_redux %>% count(line_type)

lines_redux_filt <-
  words_filt %>%
  recreate_lines_from_words()
lines_redux_filt

# Debugging.
# NOTE: These are single letter chart labels for months.
# lines_redux %>% filter(line %>% str_detect("j j a s o n d j")) -> z1
# z1 %>% clipr::write_clip()
# lines_redux %>% filter(line %>% str_detect("f mam j j a s o n")) -> z2
# z2 %>% clipr::write_clip()
# lines_redux %>% filter(line %>% str_detect("houston north south west houston north south west")) -> z3
# z3 %>% clipr::write_clip()

#+ ngrams-1, include=T, eval=T, echo=T
ngrams <-
  bind_rows(
    unnest_tokens_ngrams(lines_redux, 6),
    unnest_tokens_ngrams(lines_redux, 8),
    unnest_tokens_ngrams(lines_redux, 10)
  )
ngrams

# ngrams %>% filter(line_type == "content") %>% filter(ngram %>% str_detect(zones_rgx, negate = FALSE))

# NOTE: Although `drop_words_generic` (and `anti_join(stop_words)` were already called
# in order to create `words_filt`, `drop_ngrams_generic()` should also be called here.
ngrams_filt <-
  ngrams %>%
  drop_ngrams_generic()
ngrams_filt

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
  add_count(k, name = "n_k") %>%
  mutate(n_k_factor =  max(n_k) / n_k)
ngrams_filt_n

ngrams_filt_n %>% distinct(k, n_k_factor)

ngrams_tfidf <-
  ngrams_filt %>%
  count(year, ngram) %>%
  tidytext::bind_tf_idf(term = ngram, document = year, n = n)
ngrams_tfidf

# NTOE: This attempt to answer the question "What were the most "unique" ngrams?"
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
# (In this case, "extended" = ngram of X tokens.)
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

# NOTE: "Manual" tfidf
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

words_section_tfidf_summ <-
  words_section_tfidf_filt %>%
  group_by(section_label) %>%
  averagize_at("tf_idf") %>%
  ungroup() %>%
  arrange(desc(tf_idf_avg))
words_section_tfidf_summ

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
