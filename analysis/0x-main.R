
# library("tokenizers")
# library("pdftools")
# filter <- dplyr::filter
library("tidyverse")
library("teplot")

paths <-
  fs::dir_ls(
    path = "data-raw",
    regexp = "pdf$"
  )
paths_info <-
  paths %>%
  as.character() %>%
  tibble(path = .) %>%
  mutate(
    year = path %>% str_extract("201[0-8]") %>% as.integer()
  ) %>%
  select(year, path)

idx_page_start <- 7L
toc_pages_nest <-
  paths_info %>%
  mutate(
    page = purrr::map(path, ~pdftools::pdf_text(.) %>% .[c(3L:(idx_page_start - 1))])
  ) %>%
  select(-path)
toc_pages_nest

toc_pages <-
  toc_pages_nest %>%
  unnest(page) %>%
  group_by(year) %>%
  mutate(idx_page = (row_number() + idx_page_start) %>% as.integer()) %>%
  ungroup()
toc_pages

toc <-
  toc_pages %>%
  mutate(
    lines = purrr::map(page, ~{
      .x %>%
        str_split("\\n") %>%
        purrr::map(str_squish) %>%
        unlist() %>%
        enframe(name = "idx_line", value = "line")
    })
  ) %>%
  select(-page) %>%
  unnest(lines) %>%
  mutate_at(
    vars(line),
    ~str_to_lower(.) %>%
      str_replace_all("[.]+", "-") %>%
      str_replace_all("201[2-9]", "[year]")
  )

toc_aug <-
  toc %>%
  dplyr::filter(line %>% str_detect("executive|contents|state\\sof\\sthe|list\\sof", negate = TRUE)) %>%
  mutate_at(
    vars(line),
    list(line_type = ~case_when(
      str_detect(., "[-]\\s[ixv]+$") ~ "summary_subsection",
      str_detect(., "^[ixv]+[-]") ~ "section",
      str_detect(., "^figure") ~ "figure",
      str_detect(., "^table") ~ "table",
      str_detect(., "^[a-z]") ~ "subsection",
      TRUE ~ NA_character_
    )
    )
  ) %>%
  dplyr::filter(!is.na(line_type)) %>%
  mutate_at(
    vars(line),
    list(label = ~case_when(
      line_type == "summary_subsection" ~ str_replace(., "(^.*)(\\s?+[-]\\s?+)([ixv]+$)", "\\1"),
      line_type == "section" ~ str_replace(., "(^[ixv]+)([-]\\s?+)(.*)(\\s?+[-]\\s?+[0-9]+$)", "\\3"),
      line_type == "subsection" ~ str_replace(., "(^[a-z])([-]\\s?+)(.*)(\\s?+[-]\\s?+[0-9]+$)", "\\3"),
      line_type %in% c("figure", "table") ~ str_replace(., "(^[a-z]+\\s?+)([0-9]+)([:]\\s?+)(.*)(\\s?+[-]\\s?+[0-9]+$)", "\\4"),
      TRUE ~ NA_character_
    )
    )
  ) %>%
  mutate_at(vars(label), str_trim) %>%
  mutate_at(
    vars(line),
    list(index = ~case_when(
      line_type %in% c("figure", "table") ~ str_replace(., "(^[a-z]+\\s?+)([0-9]+)([:].*$)", "\\2"),
      TRUE ~ NA_character_
    )
    )
  ) %>%
  mutate(page_num = str_replace(line, "(^.*[-]\\s?+)([ivx0-9]+$)", "\\2")) %>%
  mutate_at(
    vars(page_num),
    list(~case_when(
      line_type != "summary_subsection" ~ as.integer(.),
      TRUE ~ utils:::.roman2numeric(.)
    )
    )
  ) %>%
  select(-idx_page, -idx_line, -line)
toc_aug

section_rngs <-
  toc_aug %>%
  dplyr::filter(line_type == "section") %>%
  group_by(year) %>%
  mutate(page_num_end = coalesce(dplyr::lead(page_num) - 1, Inf)) %>%
  mutate(idx_section = row_number()) %>%
  ungroup() %>%
  mutate_at(vars(label), ~sprintf("%d. %s", idx_section, label)) %>%
  mutate_at(vars(label), ~forcats::fct_inorder(.) %>% forcats::fct_rev()) %>%
  select(year, page_num_start = page_num, page_num_end, section_label = label)
section_rngs
section_label_lvls <- section_rngs %>% pull(section_label) %>% levels()
section_label_lvls

toc_aug_sections <-
  toc_aug %>%
  dplyr::filter(line_type %in% c("subsection", "figure", "table")) %>%
  fuzzyjoin::fuzzy_left_join(
    section_rngs,
    by = c(
      "year" = "year",
      "page_num" = "page_num_start",
      "page_num" = "page_num_end"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
  select(-matches("num_|[.]y")) %>%
  rename(year = year.x)
toc_aug_sections

toc_n <-
  toc_aug_sections %>%
  group_by(line_type, label) %>%
  add_count() %>%
  ungroup()
toc_n

library("teplot")
# library("awtools")
theme_custom<- function(...) {
  teplot::theme_te(
    base_family = "",
    base_size = 14,
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  ) +
    # hrbrthemes::theme_ipsum() +
    theme(
      # legend.key.height = unit(1.5, "cm"),
      # legend.spacing.y = unit(0.5, "cm"),
      plot.title = element_text(size = 22),
      plot.subtitle = element_text(size = 18),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16)
    )
}
ggplot2::theme_set(theme_custom())
labs_xy_null <- function(...) {
  labs(
    ...,
    x = NULL,
    y = NULL
  )
}

scale_fill_custom <- function() {
  ggthemes::scale_fill_tableau(labels = function(x) str_wrap(x, width = 30))
}

toc_n %>% pull(section_label) %>% levels()
toc_n_lists <- toc_n %>% dplyr::filter(line_type %in% c("table", "figure"))
toc_n_1yr <- toc_n_lists %>%  dplyr::filter(year == 2018)
toc_n_1yr
toc_n_1yr_wfl <-
  toc_n_1yr %>%
  mutate_at(vars(section_label), as.character) %>%
  ggwaffle::waffle_iron(ggwaffle::aes_d(group = section_label)) %>%
  as_tibble() %>%
  rename(section_label = group) %>%
  mutate_at(vars(section_label), ~forcats::as_factor(section_label_lvls))
toc_n_1yr_wfl

viz_toc_n_1yr <-
  toc_n_1yr_wfl %>%
  ggplot() +
  aes(x = x, y = y, fill = section_label) +
  ggwaffle::geom_waffle() +
  coord_equal() +
  scale_fill_custom() +
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    panel.grid = element_blank()
  ) +
  labs_xy_null() +
  labs(
    fill = "Section",
    title = "Composition of Potomac Economics' \"State of the Market\" Reports on ERCOT",
    subtitle = str_wrap(
      "Count of figures and tables Appearing in Potomac Economics' \"State of the Market\" Reports on ERCOT bin 2018.", 90
    )
  )
viz_toc_n_1yr

toc_n1 <-
  toc_n %>%
  dplyr::filter(line_type %in% c("table", "figure"), n == 1)
toc_n1

viz_toc_n1 <-
  toc_n1 %>%
  ggplot() +
  aes(x = year, y = n, fill = section_label) +
  geom_col(color = "white") +
  # guides(fill = guide_legend()) +
  # hrbrthemes::scale_fill_ipsum() +
  ggthemes::scale_fill_tableau(labels = function(x) str_wrap(x, width = 30)) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs(
    fill = "Section",
    title = "How Much Content was Really Unique in Each Report?",
    subtitle = str_wrap("Count of  figures and tables Appearing in only 1 of the 3 Potomac Economics' \"State of the Market\" Reports on ERCOT between 2016 and 2018.", 90),
    caption = paste0(
      str_wrap("Day-Ahead Market (DAM) Performance, Reliability Unit Commitments (RUCs), and Resource Adequacy received more attention in 2018 than in past years.", 110),
      "\n\nVisualization by Tony ElHabr."),
    x = NULL,
    y = NULL
  ) +
  coord_flip()
viz_toc_n1

# body ---
body_pages_nest <-
  paths_info %>%
  mutate(
    page = purrr::map(path, ~pdftools::pdf_text(.) %>% .[-c(1L:(idx_page_start - 1L))])
  ) %>%
  select(-path)
body_pages_nest

body_pages <-
  body_pages_nest %>%
  unnest(page) %>%
  group_by(year) %>%
  mutate(idx_page = (row_number() + idx_page_start) %>% as.integer()) %>%
  ungroup()
body_pages

sents <-
  body_pages %>%
  mutate(
    sents = purrr::map(page, ~{
      .x %>%
        # str_squish() %>%
        str_split(pattern = "[.]\\s?+") %>%
        purrr::map(str_squish) %>%
        unlist() %>%
        enframe(name = "idx_sent", value = "sent")
    })
  ) %>%
  select(-page) %>%
  unnest(sents)
sents

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
sents
head_reports <- function(data, ..., .n_rows = 30) {
  opt_old <- getOption("tibble.print_max")
  # options(tibble.print_min = .n_rows)
  options(tibble.print_max = .n_rows)
  on.exit(options(tibble.print_min = opt_old))
  data %>%
    group_by(year, ...) %>%
    slice(c(1:10), .preserve = FALSE) %>%
    ungroup()

}

sents_aug <-
  sents %>%
  mutate_at(vars(sent), ~str_remove_all(., "\\/\\s+")) %>%
  mutate_at(
    vars(sent),
    list(sent_type = ~case_when(
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
sents_aug %>% count(year, sent_type)
sents_aug %>% count(sent_type)
sents_aug %>% head_reports(idx_page)

paste_collapse <- function(x) {
  paste0("(^", paste(x, collapse = "$)|(^", sep = ""), "$)")
}

rgx_month_abbs <- month.abb %>% tolower() %>% paste_collapse()
rgx_month_names <- month.name %>% tolower() %>% paste_collapse()
# words_aug %>% dplyr::filter(word %>% str_detect(rgx_month_names))
# words

words <-
  sents_aug %>%
  tidytext::unnest_tokens(
    output = word,
    input = sent
  ) %>%
  mutate_at(vars(word), ~stringi::stri_unescape_unicode(.) %>% str_remove_all(",")) %>%
  mutate_at(
    vars(word),
    list(
      word_int = ~as.integer(.),
      word_num = ~as.numeric(.),
      word_date = ~dplyr::case_when(
        str_detect(., rgx_month_abbs) | str_detect(., rgx_month_names) ~ word,
        TRUE ~ NA_character_
      )
    )
  )
words
# words %>%
#   dplyr::filter(!is.na(word_date))
# words %>%
#   dplyr::filter(dplyr::lag(word, 1) == "second")

# stop_words <- stopwords::data_stopwords_snowball["en"] %>% unlist() %>% tibble(word = .)
stop_words <- tidytext::stop_words

words_aug <-
  words %>%
  # anti_join(stop_words) %>%
  # mutate_at(vars(word), ~dplyr::if_else(. == year, "[year]", .))
  mutate_at(
    vars(word),
    list(~case_when(
      is.na(word_num) & is.na(word_date) ~ word,
      !is.na(word_date) ~ "[month]",
      word_int == year ~ "[year]",
      word_int == (year + 1) ~ "[year+1]",
      word_int == (year - 1) ~ "[year-1]",
      word_int == (year + 2) ~ "[year+2]",
      word_int == (year - 2) ~ "[year-2]",
      word_int >= (year + 3) && . <= (year + 3) ~ "[year>+3]",
      word_int <= (year - 3) && . >= (year - 3) ~ "[year<-3]",
      (word_int >= 1) & (word_int <= 31) ~ "[month/day]",
      # isword_numinteger(word_num) && (word_num >= 1) && (word_num <= 31) ~ "[day]",
      (round(word_num / 1e1) * 1e1) == 0 ~ "[x<10]",
      (round(word_num / 1e2) * 1e2) == 0 ~ "[10<x<100]",
      (round(word_num / 1e3) * 1e3) == 0 ~ "[100<x<1k]",
      (round(word_num / 1e6) * 1e6) == 0 ~ "[1k<x<1M]",
      (round(word_num / 1e9) * 1e9) == 0 ~ "[1M<x<1B]",
      (round(word_num / 1e9) * 1e9) >= 0 ~ "[x>1B]",
      !is.na(word_num) ~ "[x]",
      TRUE ~ word
    )
    )
  )
words_aug

sents_redux <-
  words_aug %>%
  group_by(year, idx_page, idx_sent, sent_type) %>%
  summarise(sent = paste(word, collapse = " ")) %>%
  ungroup()
sents_redux

ngrams <-
  sents_redux %>%
  tidytext::unnest_tokens(
    output = ngram,
    input = sent,
    to_lower = FALSE,
    # strip_punct = FALSE,
    # token = "skip_ngrams",
    # n = 4,
    # k = 2
    token = "ngrams",
    n = 6
  )
ngrams
ngrams %>% dplyr::filter(ngram %>% str_detect("\\["))

ngrams_tfidf <-
  ngrams %>%
  count(year, ngram) %>%
  dplyr::filter(ngram %>% str_detect("year|month|day|10|1k|1M|1B", negate = TRUE)) %>%
  tidytext::bind_tf_idf(ngram, year, n) %>%
  arrange(desc(tf_idf))
ngrams_tfidf

sents_tfidf <-
  sents_redux %>%
  count(year, sent) %>%
  # mutate(n = 1) %>%
  tidytext::bind_tf_idf(sent, year, n) %>%
  arrange(desc(tf_idf))
sents_tfidf

n_top <- 10
sents_tfidf_top <-
  sents_tfidf %>%
  group_by(year) %>%
  arrange(desc(tf_idf), .by_group = TRUE) %>%
  dplyr::filter(sent %>% str_detect("\\[|admin", negate = TRUE)) %>%
  slice(c(1:n_top)) %>%
  ungroup()
sents_tfidf_top

# Using this?
sents_redux_n <-
  sents_redux %>%
  count(sent_type, sent, sort = TRUE) %>%
  # dplyr::filter(!(sent_type %in% c("page_footer", "section_alphanumeric_label"))) %>%
  dplyr::filter(sent_type == "content")
sents_redux_n

# sents_redux_n1 <-
#   sents_redux_n %>%
#   dplyr::filter(n > 1)
# sents_redux_n1

words_n <-
  words_aug %>%
  count(year, word, sort = TRUE)
words_n

words_frac <-
  words_n %>%
  left_join(words_aug %>% group_by(year) %>% summarise(n_doc = n())) %>%
  left_join(words_aug %>% group_by(word) %>% summarise(n_word = n())) %>%
  mutate(
    word_frac_doc = n / n_doc,
    word_frac_total = n / n_word
  ) %>%
  arrange(desc(word_frac_doc))
words_frac

words_frac_filt <-
  words_frac %>%
  # dplyr::filter(n_word > 5) %>%
  dplyr::filter(n_word > 5, word_frac_total > 0.75) %>%
  arrange(desc(word_frac_total))
words_frac_filt

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
