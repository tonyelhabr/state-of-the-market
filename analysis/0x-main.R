
# library("tokenizers")
# library("pdftools")
# filter <- dplyr::filter
library("tidyverse")
library("teplot")
# library("awtools")

filter <- dplyr::filter

# paths ----
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
paths_info

# pages ----
pages_nest <-
  paths_info %>%
  mutate(
    page = purrr::map(path, ~pdftools::pdf_text(.))
  ) %>%
  select(-path)

pages <-
  pages_nest %>%
  unnest(page) %>%
  group_by(year) %>%
  mutate(idx_page = row_number()) %>%
  ungroup()
pages

pages_n <-
  pages_nest %>%
  mutate(n_page = purrr::map_int(page, ~length(.x))) %>%
  select(-page)
pages_n

idx_page_start <- 7L
toc_pages <-
  pages %>%
  filter(idx_page >= 3L, idx_page <= (idx_page_start - 1))
toc_pages

body_pages <-
  pages %>%
  filter(idx_page >= idx_page_start)
body_pages

# toc ----
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
toc

# `utils:::.roman2numeric()`, but without the warning clause.
roman2numeric <- function (x) {
  out <- integer(length(x))
  out[ina <- is.na(x) | !nzchar(x)] <- NA
  if (any(ind <- !ina)) {
    y <- toupper(x[ind])
    y <- gsub("CM", "DCCCC", y)
    y <- gsub("CD", "CCCC", y)
    y <- gsub("XC", "LXXXX", y)
    y <- gsub("XL", "XXXX", y)
    y <- gsub("IX", "VIIII", y)
    y <- gsub("IV", "IIII", y)
    ok <- grepl("^M{,3}D?C{,4}L?X{,4}V?I{,4}$", y)
    out[ind][ok] <-
      vapply(strsplit(y[ok], ""), function(z)
        as.integer(sum(.romans[match(z,
                                     names(.romans))])), integer(1L))
    out
  }
}

toc_aug <-
  toc %>%
  filter(line %>% str_detect("executive|contents|state\\sof\\sthe|list\\sof", negate = TRUE)) %>%
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
  filter(!is.na(line_type)) %>%
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
      TRUE ~ roman2numeric(.)
    )
    )
  ) %>%
  select(-idx_page, -idx_line, -line)
toc_aug

section_rngs <-
  toc_aug %>%
  filter(line_type == "section") %>%
  group_by(year) %>%
  left_join(pages_n) %>%
  mutate(page_num_end = coalesce(dplyr::lead(page_num) - 1L, n_page)) %>%
  mutate(idx_section = row_number()) %>%
  ungroup() %>%
  mutate_at(vars(label), ~sprintf("%d. %s", idx_section, label)) %>%
  mutate_at(vars(label), ~forcats::fct_inorder(.) %>% forcats::fct_rev()) %>%
  select(year, page_num_start = page_num, page_num_end, section_label = label)
section_rngs
section_label_lvls <- section_rngs %>% pull(section_label) %>% levels()
section_label_lvls

toc_sections <-
  toc_aug %>%
  filter(line_type %in% c("subsection", "figure", "table")) %>%
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
toc_sections

toc_n <-
  toc_sections %>%
  group_by(line_type, label) %>%
  add_count() %>%
  ungroup()
toc_n

toc_n %>% pull(section_label) %>% levels()

filter_lists <- function(data) {
  data %>%
    filter(line_type %in% c("table", "figure"))
}

toc_n_lists <-
  toc_n %>%
  filter_lists()
toc_n_lists

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

toc_n_1yr <- toc_n_lists %>%  filter(year == 2018)
toc_n_1yr
toc_n_1yr_wfl <-
  toc_n_1yr %>%
  mutate_at(vars(section_label), as.character) %>%
  ggwaffle::waffle_iron(ggwaffle::aes_d(group = section_label)) %>%
  as_tibble() %>%
  rename(section_label = group) %>%
  mutate_at(vars(section_label), ~factor(., section_label_lvls))
toc_n_1yr_wfl


viz_footer <- "By: Tony ElHabr.\nData source: https://www.potomaceconomics.com/markets-monitored/ercot/.\n"
viz_label_static_1 <- "Potomac Economics' \"State of the Market\" Reports on ERCOT"
viz_label_static_2 <- "figures and tables"

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
    title = str_wrap(glue::glue("Composition of {viz_label_static_1}"), 120),
    subtitle = str_wrap(
      glue::glue(
        "Counts of {viz_label_static_2} appearing in {viz_label_static_1} in 2018."
        ), 90),
    caption = viz_footer
  )
viz_toc_n_1yr

toc_n1 <-
  toc_n %>%
  filter(line_type %in% c("table", "figure"), n == 1)
toc_n1

viz_toc_n1 <-
  toc_n1 %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  ggplot() +
  aes(x = year, y = n, fill = section_label) +
  geom_col(color = "white") +
  # geom_text(aes(label = label, y = idx), color = "white") +
  guides(fill = FALSE) +
  # hrbrthemes::scale_fill_ipsum() +
  scale_fill_custom() +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs_xy_null() +
  labs(
    fill = "Section",
    title = "How Much Content was Really Unique in Each Report?",
    subtitle = paste0(
      str_wrap(
        glue::glue(
          "Counts of {viz_label_static_2} appearing in only 1 of the 3 {viz_label_static_1} between 2016 and 2018."
          ), 90)
      ),
    caption = paste0(
      str_wrap(
        glue::glue(
          "Day-Ahead Market (DAM) Performance, Reliability Unit Commitments (RUCs),
          and Resource Adequacy received more attention in 2018 than in past years."
        ), 120),
      "\n", viz_footer
    )
  ) +
  coord_flip()
viz_toc_n1

section_rngs_n <-
  toc_sections %>%
  filter_lists() %>%
  count(year, section_label) %>%
  left_join(
    section_rngs
  ) %>%
  mutate(n_pages = page_num_end - page_num_start) %>%
  mutate(list_pages_ratio = n / n_pages)
section_rngs_n

section_rngs_n_1yr <- section_rngs_n %>% filter(year == 2018)
section_rngs_n_1yr

viz_section_rngs_n_1yr <-
  section_rngs_n_1yr %>%
  ggplot() +
  aes(x = section_label, y = list_pages_ratio, fill = section_label) +
  geom_col() +
  # scale_y_continuous(labels = scales::percent) +
  scale_fill_custom() +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Ratio",
    fill = "Section",
    title = "Which Sections Have a Disproportionate Number of Figures And Tables?",
    subtitle = paste0(
      str_wrap(
        glue::glue(
          "Ratio of counts of {viz_label_static_2} vs. number of pages per section in {viz_label_static_1} in 2018."
        ), 90)
    ),
    caption = paste0(
      str_wrap(
        glue::glue(
          "By coincidence (or not), the sections appearing earlier in the report have more plots and tables."
        ), 120),
      "\n", viz_footer
    )
  )
viz_section_rngs_n_1yr

# lines ----
lines <-
  body_pages %>%
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

.head_reports <- function(data, ..., .n_rows = 30) {
  opt_old <- getOption("tibble.print_max")
  # options(tibble.print_min = .n_rows)
  options(tibble.print_max = .n_rows)
  on.exit(options(tibble.print_min = opt_old))
  data %>%
    group_by(year, ...) %>%
    slice(c(1:10), .preserve = FALSE) %>%
    ungroup()

}

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
# lines_aug %>% count(year, line_type)
# lines_aug %>% count(line_type)
# lines_aug %>% .head_reports(idx_page)

# words ----
paste_collapse <-
  function(...,
           start = "(",
           end = ")",
           collapse = paste0(end, "|", start),
           sep = "") {
    paste0(start, paste(..., collapse = collapse, sep = sep), end)
  }

paste_collapse_loosely <- paste_collapse

paste_collapse_strictly <- function(..., start = "(^", end = "$)") {
  paste_collapse(..., start = start, end = end)
}

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

zones <- c("houston", "north", "south", "west")
rgx_zones <-
  zones %>%
  # combinat::permn() %>%
  purrr::map_chr(~paste_collapse(., start = "", end = "", collapse = "\\s")) %>%
  paste_collapse_loosely()
rgx_zones

# TODO(?)
# month_abbs2 <- c("m", "j", "j", "a", "s", "o", "n", "d")

# stop_words <- tidytext::stop_words

words <-
  lines_aug %>%
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
        # str_detect(., rgx_month_abbs) | str_detect(., rgx_month_names) ~ word,
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
      !is.na(word_month) ~ "ZmonthZ",
      word_int == year ~ "ZyearZ",
      word_int == (year > 1) ~ "Zyearlead1Z",
      word_int == (year < 1) ~ "Zyearlag1Z",
      word_int == (year > 2) ~ "Zyearlead2Z",
      word_int == (year < 2) ~ "Zyearlag2Z",
      # NOTE: Put some bounds on these to avoid capturing ALL integers.
      word_int >= (year > 3) && . <= (year < 6) ~ "Zyeargtlead3Z",
      word_int <= (year < 3) && . >= (year > 6) ~ "Zyearltlag3Z",
      (word_int >= 1) & (word_int <= 31) ~ "ZmonthdayZ",
      # isword_numinteger(word_num) && (word_num >= 1) && (word_num <= 31) ~ "ZdayZ",
      (round(word_num / 1e1) * 1e1) == 0 ~ "Zlt10Z",
      (round(word_num / 1e2) * 1e2) == 0 ~ "Z10ltxlt100Z",
      (round(word_num / 1e3) * 1e3) == 0 ~ "Z100ltxlt1kZ",
      (round(word_num / 1e6) * 1e6) == 0 ~ "Z1kltxlt1MZ",
      (round(word_num / 1e9) * 1e9) == 0 ~ "Z1Mltxlt1BZ",
      (round(word_num / 1e9) * 1e9) >= 0 ~ "Zxgt1BZ",
      !is.na(word_num) ~ "ZxZ",
      # NOTE: "a" can also be a month label (for April/August), but it's also prevalently
      # used as an adjective, so its problemmatic.
      # Maybe just leave this filtering for later?
      # word %in% c("j", "f", "m", "s",  "o", "n", "d") ~ "ZmonthlabelZ",
      # str_detect(., rgx_months) ~ "ZmonthZ",
      # str_detect(., rgx_zones) ~ "ZzoneZ",
      TRUE ~ word
    )
    )
  )
words_aug

# TODO: Ternary plot with no stopwords?
filter_tokens_generic_at <- function(data, col = "") {
  col_quo <- sym(col)
  data %>%
    filter(!!col_quo %>% str_detect("^Z|Z$|\\sZ|Z\\s", negate = TRUE))
}
filter_words_generic <- function(..., col = "word") {
  filter_tokens_generic_at(..., col = col)
}


# words_aug %>% filter(word == "ZmonthlabelZ")
# lintes_redux ----
lines_redux <-
  words_aug %>%
  group_by(year, idx_page, idx_line, line_type) %>%
  summarise(line = paste(word, collapse = " ")) %>%
  ungroup()
lines_redux
lines_redux %>% count(line_type)

# Debugging.
# NOTE: These are single letter chart labels for months.
# lines_redux %>% filter(line %>% str_detect("j j a s o n d j")) -> z1
# z1 %>% clipr::write_clip()
# lines_redux %>% filter(line %>% str_detect("f mam j j a s o n")) -> z2
# z2 %>% clipr::write_clip()
# lines_redux %>% filter(line %>% str_detect("houston north south west houston north south west")) -> z3
# z3 %>% clipr::write_clip()

# words_pmi ----
# # Reference: https://juliasilge.com/blog/word-vectors-take-two/
# slide_windows <- function(tbl, doc_var, window_size) {
#   # each word gets a skipgram (window_size words) starting on the first
#   # e.g. skipgram 1 starts on word 1, skipgram 2 starts on word 2
#
#   each_total <- tbl %>%
#     group_by(!!doc_var) %>%
#     mutate(doc_total = n(),
#            each_total = pmin(doc_total, window_size, na.rm = TRUE)) %>%
#     pull(each_total)
#
#   rle_each <- rle(each_total)
#   counts <- rle_each[["lengths"]]
#   counts[rle_each$values != window_size] <- 1
#
#   # each word get a skipgram window, starting on the first
#   # account for documents shorter than window
#   id_counts <- rep(rle_each$values, counts)
#   window_id <- rep(seq_along(id_counts), id_counts)
#
#
#   # within each skipgram, there are window_size many offsets
#   indexer <- (seq_along(rle_each[["values"]]) - 1) %>%
#     purrr::map2(rle_each[["values"]] - 1,
#                 ~ seq.int(.x, .x + .y)) %>%
#     purrr::map2(counts, ~ rep(.x, .y)) %>%
#     flatten_int() +
#     window_id
#
#   tbl[indexer, ] %>%
#     bind_cols(data_frame(window_id)) %>%
#     group_by(window_id) %>%
#     filter(n_distinct(!!doc_var) == 1) %>%
#     ungroup
# }
#
# nearest_synonyms <- function(df, token) {
#   df %>%
#     widyr::widely(~ . %*% (.[token, ]), sort = TRUE)(item1, dimension, value) %>%
#     select(-item2)
# }
#
# words_pmi <-
#   lines_redux %>%
#   tidytext::unnest_tokens(output = word, input = line) %>%
#   add_count(word) %>%
#   filter(n >= 20) %>%
#   select(-n) %>%
#   slide_windows(rlang::quo(idx_line), 4) %>%
#   widyr::pairwise_pmi(word, window_id)
# words_pmi
# words_pmi %>% arrange(desc(pmi))
#
# word_vectors <-
#   words_pmi %>%
#   widyr::widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)
# ngram_vectors
# # word_vectors %>% nearest_synonyms("west")

# ngrams ----
unnest_tokens_ngrams <- function(data, .n, ...) {
  tidytext::unnest_tokens(
    data,
    output = ngram,
    input = line,
    to_lower = FALSE,
    token = "ngrams",
    n = .n,
    ...
  ) %>%
    mutate(k = .n)
}

ngrams <-
  bind_rows(
    # unnest_tokens_ngrams(lines_redux, 4),
    unnest_tokens_ngrams(lines_redux, 6),
    unnest_tokens_ngrams(lines_redux, 8),
    unnest_tokens_ngrams(lines_redux, 10),
    unnest_tokens_ngrams(lines_redux, 12)
  )

ngrams

# ngrams %>% filter(line_type == "content") %>% filter(ngram %>% str_detect(zones_rgx, negate = FALSE))

ngrams_filt <-
  ngrams %>%
  # filter(line_type == "content") %>%
  # NOTE: Filter out the marked ngrams.
  filter(ngram %>% str_detect("^Z|Z$|\\sZ|Z\\s", negate = TRUE)) %>%
  # filter(ngram %>% str_detect("(houston\\snorth\\ssouth\\swest)", negate = TRUE))
  filter(ngram %>% str_detect(rgx_zones, negate = TRUE))
ngrams_filt

n_k_max <-
  ngrams_filt %>%
  count(k) %>%
  summarise(temp = max(n)) %>%
  pull(temp)
n_k_max

n_grams_filt_n <-
  ngrams_filt %>%
  select(year, ngram, k) %>%
  add_count(k, name = "n_k") %>%
  # mutate(n_k_factor = 1 + (max(n_k) - n_k) / (max(n_k) - 0)) %>%
  mutate(n_k_factor =  max(n_k) / n_k)
n_grams_filt_n
n_grams_filt_n %>% distinct(k, n_k_factor)

ngrams_tfidf <-
  ngrams_filt %>%
  count(year, ngram) %>%
  tidytext::bind_tf_idf(term = ngram, document = year, n = n)
ngrams_tfidf

# What were the most "unique" ngrams?
# NOTE: Larger n-grams are more likely to have higher TFIDF, so need to account
# for this by dividing by `n_k_factor`.
ngrams_tfidf_aug <-
  ngrams_tfidf %>%
  left_join(
    n_grams_filt_n %>% distinct()
  ) %>%
  # distinct() %>%
  mutate(tf_idf_adj = tf_idf / n_k_factor) %>%
  arrange(desc(tf_idf))
ngrams_tfidf_aug
# ngrams_tfidf_aug %>% arrange(desc(tf_idf_adj)) %>% mutate(rnk = row_number()) %>% filter(k == 6)

# What were the most used exteneded ngrams.?(In this case, extended = ngram of 8 tokens.)
ngrams_tf_maxk <-
  ngrams_tfidf_aug %>%
  filter(k >= max(k)) %>%
  group_by(k, ngram) %>%
  summarise(n = sum(n), tf = mean(n * tf)) %>%
  ungroup() %>%
  arrange(desc(tf))
ngrams_tf_maxk

# lines_tfidf <-
#   lines_redux %>%
#   count(year, line) %>%
#   # mutate(n = 1) %>%
#   tidytext::bind_tf_idf(line, year, n) %>%
#   arrange(desc(tf_idf))
# lines_tfidf
#
# n_top <- 10
# lines_tfidf_top <-
#   lines_tfidf %>%
#   group_by(year) %>%
#   arrange(desc(tf_idf), .by_group = TRUE) %>%
#   filter(line %>% str_detect("\\[|admin", negate = TRUE)) %>%
#   slice(c(1:n_top)) %>%
#   ungroup()
# lines_tfidf_top

# FIXME: Using this?
# lines_redux_n <-
#   lines_redux %>%
#   count(line_type, line, sort = TRUE) %>%
#   # filter(!(line_type %in% c("page_footer", "section_alphanumeric_label"))) %>%
#   filter(line_type == "content")
# lines_redux_n

# lines_redux_n1 <-
#   lines_redux_n %>%
#   filter(n > 1)
# lines_redux_n1

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
  # filter(n_word > 5) %>%
  filter(n_word > 5, word_frac_total > 0.75) %>%
  arrange(desc(word_frac_total))
words_frac_filt

words_tfidf <-
  words_n %>%
  tidytext::bind_tf_idf(word, year, n) %>%
  arrange(desc(tf_idf))
words_tfidf

words_filt <-
  words_tfidf %>%
  filter_words_generic() %>%
  anti_join(stop_words)
words_filt
# library("ggtern")
arrws <- tibble(
  x = c(1, 0, 0),
  y = c(0, 1, 0),
  z = c(0, 0, 1),
  xend = c(0, 1, 1),
  yend = c(1, 0, 1),
  zend = c(1, 1, 0)
)
words_tern <-
  words_filt %>%
  select(year, word, tf) %>%
  spread(year, tf)
words_tern

words_tern_mark <-
  words_filt %>%
  mutate(.year = year, idx = row_number()) %>%
  select(.year, idx, year, word, tf) %>%
  spread(year, tf) %>%
  arrange(idx)
words_tern_mark

viz_words_tern <-
  words_tern %>%
  ggtern::ggtern() +
  aes(x = `2016`, y = `2017`, z = `2018`) +
  ggtern::geom_mask() +
  geom_segment(
    data = arrws,
    aes(x, y, z, xend = xend, yend = yend, zend = zend),
    color = "grey",
    size = 1
  ) +
  geom_point() +
  geom_label(
    data = words_tern %>% slice(c(1:10)),
    aes(label = word)
  ) +
  theme(legend.position = "none") +
  # ggforce::geom_mark_circle(
  #   data = words_tern_mark %>% filter(idx <= 10),
  #   aes(label = word)
  # ) +
  ggtern::theme_classic()
viz_words_tern

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
