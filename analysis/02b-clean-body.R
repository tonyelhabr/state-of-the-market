

lines <-
  body_pages %>%
  unnest_pages() %>%
  mutate_at(vars(line), tolower)
lines

# TODO:
# + Add 'sections', possibly corresponding to the first lineence afer each page line.
# + Remove page lines.
# + Label figure lines?

# + In 2016, actual page 1 is `idx_page = 29`. For 2017 it is 31. For 2018 it is 30.
# + Page footers are noted by '2016 State of the Market Report | xxi /' or
# 'xx | 2016 State of the Market Report /'
# on alternating pages.
# + The first line for each new section is parsed imperfectly. For example,
# on page 59 in 2016. 'Day-Ahead Market Performance II' and
# 'DAY-AHEAD MARKET PERFORMANCE ERCOT's day-ahead ...' are parsed as two lines.
# It should be 'Day-Ahead Market Performance' for the page header and
# 'II DAY-AHEAD MARKET PERFORMANCE' for the section header,
# then 'ERCOT's day-ahead ...' for the first lineence of the body.
section_labels_orig <-
  toc_aug %>%
  filter(line_type == 'section') %>%
  distinct(label) %>%
  pull(label)

rgx_section_labels_orig <-
  c('executive summary', section_labels_orig) %>%
  paste_collapse_loosely()
rgx_section_labels_orig

lines_aug <-
  lines %>%
  # NOTE: These are page footers/headers.
  mutate_at(vars(line), ~str_remove_all(., '\\/\\s+')) %>%
  mutate_at(
    vars(line),
    list(line_type = ~case_when(
      str_detect(., rgx_section_labels_orig) ~ 'section_label',
      str_detect(., '^[a-z]$') ~ 'section_alphanumeric_label',
      str_detect(., '^[0-9]{1,2}$') ~ 'list_item_ordered',
      str_detect(., '\\\uf0b7') ~ 'list_item_unordered',
      str_detect(., '(figure|table)\\s[0-9]+[:]') ~ 'content_label',
      str_detect(., '(figure|table)\\s[0-9]+[^:]') ~ 'content_explanation',
      str_detect(., '(admin[.] code)|(see.*puct)') ~ 'misc',
      str_detect(., '201[2-9] [s]tate [o]f [t]he [m]arket [r]eport') ~ 'page_footer',
      # str_detect(., '^[-A-Z\\s]{10,}') ~ 'section_header',
      # str_length(.) < 20 ~ 'filler',
      TRUE ~ 'text'
    )
    )
  )
lines_aug

lines_aug %>% count(year, line_type)
lines_aug %>% count(line_type)
lines_aug %>% filter(line_type == 'miscell')
lines_aug %>% filter(line_type == 'section_label')

body_rngs <-
  body_pages %>%
  filter(page %>% str_detect('Executive Summary')) %>%
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
  left_join(body_rngs) %>%
  # NOTE: Get rid of the executive summary section.
  filter(idx_page >= page_start) %>%
  group_by(year) %>%
  mutate(page_num = idx_page - min(idx_page) + 1) %>%
  ungroup() %>%
  select(year, page_num, idx_line, line_type, line) %>%
  fuzzyjoin::fuzzy_left_join(
    section_rngs,
    by = c(
      'year' = 'year',
      'page_num' = 'page_start',
      'page_num' = 'page_end'
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
  select(-matches('[.]y|page_[se]')) %>%
  rename(year = year.x)

# # NOTE: Do this to include Executive Summary stuff,
# # but then need to change other things later.
# lines_sections <-
#   bind_rows(
#     lines_aug %>%
#       left_join(body_rngs) %>%
#       filter(idx_page < page_start) %>%
#       mutate(page_num = NA_real_, idx_section = 0, section_label = 'Summary'),
#     lines_sections
#   ) %>%
#   mutate_at(
#     vars(section_label),
#     ~forcats::fct_reorder(., idx_section)
#   ) %>%
#   arrange(year, idx_page)
# lines_sections
#
# lines_sections %>% filter(is.na(section_label))
# lines_sections %>% pull_distinctly(section_label) %>% levels()

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
  # Exclude this because its also a common adjective.
  setdiff('a') %>%
  paste_collapse_strictly()
rgx_month_abb1s

rgx_months <-
  c(rgx_month_names, rgx_month_abbs, rgx_month_abb1s) %>%
  paste_collapse(start = '', end = '', collapse = '|')
rgx_months

# TODO(?)
# month_abbs2 <- c('m', 'j', 'j', 'a', 's', 'o', 'n', 'd')

# zones <- c('houston', 'north', 'south', 'west')
# rgx_zones <-
#   zones %>%
#   purrr::map_chr(~paste_collapse(., start = '', end = '', collapse = '\\s')) %>%
#   paste_collapse_loosely()
# rgx_zones

# words ----
words <-
  lines_sections %>%
  tidytext::unnest_tokens(
    output = word,
    input = line,
    strip_punct = FALSE
  ) %>%
  mutate_at(
    vars(word),
    # ~stringi::stri_unescape_unicode(.) %>%
    #   str_remove_all(',')
    ~stringi::stri_unescape_unicode(.)
  ) %>%
  mutate_at(
    vars(word),
    list(
      is_punct = ~str_detect(., '[^a-z]+'),
      word_int = ~as.integer(.),
      word_num = ~as.numeric(.),
      word_month = ~dplyr::case_when(
        str_detect(., rgx_months) ~ word,
        TRUE ~ NA_character_
      )
    )
  )
words

# stop_words <- stopwords::data_stopwords_snowball['en'] %>% unlist() %>% tibble(word = .)
stop_words <- tidytext::stop_words

words_aug <-
  words %>%
  # anti_join(stop_words) %>%
  # mutate_at(vars(word), ~dplyr::if_else(. == year, '[year]', .))
  mutate_at(
    vars(word),
    ~case_when(
      !is.na(word_month) ~ delimitize('month'),
      word_int == year ~ delimitize('year'),
      word_int == (year + 1) ~ delimitize('yearlead1'),
      word_int == (year - 1) ~ delimitize('yearlag1'),
      word_int == (year + 2) ~ delimitize('yearlead2'),
      word_int == (year - 2) ~ delimitize('yearlag2'),
      # NOTE: Put some bounds on these to avoid capturing ALL integers.
      word_int >= (year + 3) && . <= (year + 6) ~ delimitize('yeargtlead3'),
      word_int <= (year - 3) && . >= (year - 6) ~ delimitize('yearltlag3'),
      # NOTE: This could either be a month or a day.
      (word_int >= 1) & (word_int <= 31) ~ delimitize('monthday'),
      (round(word_num / 1e1) * 1e1) == 0 ~ delimitize('xlt1'),
      (round(word_num / 1e2) * 1e2) == 0 ~ delimitize('ltxlt100'),
      (round(word_num / 1e3) * 1e3) == 0 ~ delimitize('100ltxlt1k'),
      (round(word_num / 1e6) * 1e6) == 0 ~ delimitize('1kltxlt1M'),
      (round(word_num / 1e9) * 1e9) == 0 ~ delimitize('1Mltxlt1B'),
      (round(word_num / 1e9) * 1e9) >= 0 ~ delimitize('xgt1B'),
      !is.na(word_num) ~ delimitize('x'),
      # NOTE: 'a' can also be a month label (for April/August), but it's also prevalently
      # used as an adjective, so its problemmatic.
      # Maybe just leave this filtering for later?
      # word %in% c('j', 'f', 'm', 's',  'o', 'n', 'd') ~ 'QQmonthlabelQQ',
      # str_detect(., rgx_months) ~ 'QQmonthQQ',
      # str_detect(., rgx_zones) ~ 'QQzoneQQ',
      TRUE ~ word
    )
  )
words_aug

words_filt <-
  words_aug %>%
  drop_words_generic() %>%
  filter(!is_punct) %>%
  anti_join(stop_words)
words_filt

lines_redux_w_delim <-
  words_aug %>%
  recreate_lines_from_words()
lines_redux_w_delim

lines_redux_w_delim %>% count(line_type)

lines_redux_w_delim_filt <-
  words_filt %>%
  recreate_lines_from_words()
lines_redux_w_delim_filt

# sents_redux_wo_delim ----
lines_sections %>% filter(line_type == 'text')
lines_redux_w_delim %>% filter(line %>% str_detect('QQ'))
lines_redux_w_delim_filt

# WIP, start ----
lines_redux_wo_delim <-
  words %>%
  recreate_lines_from_words()
lines_redux_wo_delim

body_redux_wo_delim <-
  # lines_sections %>%
  lines_redux_wo_delim %>%
  filter(line_type == 'text') %>%
  group_by(year, section_label) %>%
  summarise(text = paste(line, collapse = ' ', sep = '')) %>%
  ungroup() %>%
  # mutate_at(vars(text), ~str_replace_all(., '(\\s)([\\”\\)\\(])(\\s)', ' \1')) %>%
  # mutate_at(vars(text), ~str_replace_all(., '(\\s)([\\”\\)\\(])([.])', ' \2.'))
  mutate_at(vars(text), ~str_replace_all(., '\\s([.])', '.'))
body_redux_wo_delim

# # Testing regex.
# body_redux_wo_delim %>% slice(c(1)) %>% pull(text) %>% clipr::write_clip()

sents_redux_wo_delim <-
  body_redux_wo_delim %>%
  tidytext::unnest_tokens(
    output = sentence,
    input = text,
    # token = 'sents_redux_wo_delim'
    token = 'regex',
    # pattern = '(?=)([a-z]{2,}|[)])([.]\\s\\”|[.])\\s'
    pattern = '([a-z]{2,}|[)])([.]\\s\\”|[.])\\s'
  ) %>%
  mutate_at(vars(sentence), str_trim)
sents_redux_wo_delim

lines_redux_w_delim %>% filter(line %>% str_detect('the operating reserve adder was implemented'))
sents_redux_wo_delim %>% filter(sentence %>% str_detect('the operating reserve adder was implemented'))
sents_redux_wo_delim %>% filter(sentence %>% str_detect('QQ'))

# NOTE: `n_spaces` chosen subjectively.
# NOTE: It's evident that there is still some "clutter"
sents_redux_wo_delim_filt <-
  sents_redux_wo_delim %>%
  mutate_at(vars(sentence), list(n_spaces = ~str_split(., '[a-z]+') %>% purrr::map_int(length))) %>%
  filter(n_spaces > 4)
sents_redux_wo_delim_filt
sents_redux_wo_delim_filt %>% arrange(n_spaces)
