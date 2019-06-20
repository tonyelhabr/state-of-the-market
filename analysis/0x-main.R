#' ---
#' title: ""
#' author: ""
#' output:
#'   html_document:
#'     theme: cosmo
#'     highlight: haddock
#'     toc: true
#'     toc_depth: 3
#' ---

#+ postprocess, include=F, echo=F, cache=F
.path_r <- path.expand(rstudioapi::getSourceEditorContext()$path)
.path_sans_ext <- tools::file_path_sans_ext(.path_r)
.path_rmd <- paste0(.path_sans_ext, ".Rmd")
# rmarkdown::render(knitr::spin(.path_r, knit = FALSE))
# spelling::spell_check_files(.path_rmd)

#+ setup, include=F, cache=F
knitr::opts_knit$set(root.dir = here::here())
knitr::opts_chunk$set(
  # rows.print = 25,
  # rows.print = 25,
  echo = FALSE,
  # cache = FALSE,
  cache = TRUE,
  include = TRUE,
  fig.show = "asis",
  fig.align = "center",
  fig.width = 6,
  # size = "small",
  # fig.height = 4.5,
  # fig.width = 5,
  # out.width = 5,
  fig.asp = 0.75,
  warning = FALSE,
  message = FALSE
)
# .width <- getOption("width")
# options(width = 999)
# on.exit(options(width = .width))
# options(tibble.width = Inf)
# options(tibble.print_max = Inf)

#+ pkg-import-1, include=F, echo=F
# library("tokenizers")
# library("pdftools")
# filter <- dplyr::filter
# library("ggtern")
library("tidyverse")
library("teplot")
# library("awtools")
filter <- dplyr::filter

#+ functions-1, include=F
# "https://www.potomaceconomics.com/wp-content/uploads/2019/06/2018-State-of-the-Market-Report.pdf"
# "https://www.potomaceconomics.com/wp-content/uploads/2018/05/2017-State-of-the-Market-Report.pdf"
# "https://www.potomaceconomics.com/wp-content/uploads/2017/06/2016-ERCOT-State-of-the-Market-Report.pdf"
is_likeinteger <-
  function(x, nm = deparse(substitute(x))){
    lgl <- round(x, 0) == x
    if(!lgl) {
      msg <- sprintf("`%s` is not integer-like.", nm)
      stop(msg, call. = FALSE)
    }
    TRUE
  }

download_sotmreport <- function(year, destfile = NULL, dir = "data-raw", quiet = TRUE, ...) {
  stopifnot(length(year) == 1, year >= 2016, year <= 2018)
  stopifnot(is_likeinteger(year))
  year_lead1 <- year + 1
  month <- ifelse(year == 2017, 5, 6)
  frmt <- "https://www.potomaceconomics.com/wp-content/uploads/%04d/%02d/%04d%s-State-of-the-Market-Report.pdf"
  prefix <- ifelse(year == 2016, "-ERCOT", "")
  url <- sprintf(frmt, year_lead1, month, year, prefix)
  if(is.null(destfile)) {
    if(!is.null(dir)) {
      if(!dir.exists(dir)) {
        message(sprintf("Creating `dir` at %s.", dir))
        invisible(dir.create(dir, recursive = TRUE))
      }
    }
    basename <- basename(url)
    destfile = file.path(dir, basename)
  }
  if(file.exists(destfile)) {
    message(sprintf("Not downloading file for `year` = %04d because it has already been downloaded.", year))
    return(destfile)
  }
  status <- download.file(url = url, destfile = destfile, quiet = TRUE, ...)
  if(status != 0) {
    stop(sprintf("Could not donwload file for `year` = %04d (from %s).", year, url), call. = FALSE)
  }
  invisible(destfile)
}

convert_page_to_lines <- function(page) {
  page %>%
    str_split("\\n") %>%
    purrr::map(str_squish) %>%
    unlist() %>%
    enframe(name = "idx_line", value = "line")
}

unnest_pages <- function(pages) {
  pages %>%
    mutate(lines = purrr::map(page, convert_page_to_lines)) %>%
    select(-page) %>%
    unnest(lines)
}

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

subset_content <- function(data) {
  data %>%
    filter(line_type %in% c("table", "figure"))
}

.dir_viz <- "analysis/figs"
.export_viz <- TRUE
theme_sotmreport <- function(...) {
  teplot::theme_te(
    # base_family = "",
    base_family = "Arial Narrow",
    base_size = 12,
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  ) +
    # hrbrthemes::theme_ipsum() +
    theme(
      # legend.key.height = unit(1.5, "cm"),
      # legend.spacing.y = unit(0.5, "cm"),
      plot.caption = element_text(hjust = 1),
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 16),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
}

# theme_sotmreport <- function(...) {
#   hrbrthemes::theme_ipsum(
#     base_size = 12,
#     plot_title_size = 20,
#     subtitle_size = 16,
#     plot_margin = ggplot2::margin(20, 20, 20, 20),
#     strip_text_size = 14,
#     caption_size = 12,
#     caption_face = "plain"
#   )
# }

# Reference: https://github.com/Ryo-N7/soccer_ggplots/blob/master/Copa%20America%202019/copa_america2019.rmd
theme_sotmreport_dark <-
  function(base_family = "Arial Narrow",
           title.size = 24,
           subtitle.size = 14,
           caption.size = 14,
           axis.text.size = 14,
           axis.text.x.size = 12,
           axis.text.y.size = 12,
           axis.title.size = 16,
           strip.text.size = 18,
           color_bkgrd = "grey90",
           color_grid = ifelse(color_bkgrd == "black", "white", "black"),
           color_text = color_grid,
           color_title = color_text,
           panel.grid.major.x = element_line(size = 0.5, color = color_grid),
           panel.grid.major.y = element_line(size = 0.5, color = color_grid),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank(),
           axis.ticks = element_line(color = color_grid),
           ...) {
    theme(text = element_text(family = base_family, color = color_text),
          plot.title = element_text(
            family = base_family,
            face = "bold",
            size = title.size,
            color = color_text
          ),
          plot.subtitle = element_text(size = subtitle.size),
          plot.caption = element_text(size = caption.size),
          panel.background = element_rect(fill = color_bkgrd),
          plot.background = element_rect(fill = color_bkgrd),
          axis.text = element_text(size = axis.text.size, color = color_text),
          axis.text.x = element_text(size = axis.text.x.size, color = color_text),
          axis.text.y = element_text(size = axis.text.y.size, color = color_text),
          axis.title = element_text(size = axis.title.size),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major.x = panel.grid.major.x,
          panel.grid.major.y = panel.grid.major.y,
          panel.grid.minor.x = panel.grid.minor.x,
          panel.grid.minor.y = panel.grid.minor.y,
          strip.text = element_text(
            face = "bold",
            size = strip.text.size,
            margin = margin(4.4, 4.4, 4.4, 4.4)
          ),
          strip.background = element_blank(),
          axis.ticks = axis.ticks,
          ...
    )
  }

# ggplot2::theme_set(theme_sotmreport())

labs_xy_null <- function(...) {
  labs(
    ...,
    x = NULL,
    y = NULL
  )
}

.width_section_label <- 30
scale_fill_section <- function(..., width = .width_section_label) {
  f <- ggthemes::scale_fill_tableau
  # f <- ggthemes::scale_fill_solarized
  f(labels = function(x) str_wrap(x, width = width), ...)
}

scale_color_section <- function(..., width = .width_section_label) {
  f <- ggthemes::scale_color_tableau
  # f <- ggthemes::scale_color_solarized
  f(labels = function(x) str_wrap(x, width = width), ...)
}

scale_color_year <- function(...) {
  ggthemes::scale_color_gdocs(...)
}

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

.delim <- "QQ"

drop_tokens_generic_at <- function(data, col = "", delim = .delim) {
  col_quo <- sym(col)
  rgx <- sprintf("^%s|%s$|\\s%s|%s\\s", delim, delim, delim, delim)
  data %>%
    filter(!!col_quo %>% str_detect(rgx, negate = TRUE))
}

drop_words_generic <- function(..., col = "word") {
  drop_tokens_generic_at(..., col = col)
}

drop_ngrams_generic <- function(..., col = "ngram") {
  drop_tokens_generic_at(..., col = col)
}

recreate_lines_from_words <- function(data) {
  data %>%
    group_by(year, page_num, idx_line, line_type, section_label) %>%
    summarise(line = paste(word, collapse = " ")) %>%
    ungroup() %>%
    # NOTE: `idx_line = 1` is almost always the page header (which is redundant with the `section_label`.)
    filter(idx_line > 1)
}

delimitize <- function(x, delim = .delim) {
  sprintf("%s%s%s", delim, x, delim)
}

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

# add_idx_section_col <- function(data) {
#   data %>%
#     mutate(idx_section = section_label %>% str_replace("^([1-7])\\.(.*$)", "\\1") %>% as.integer())
# }


# Reference: https://github.com/dgrtwo/data-screencasts/blob/master/bird-collisions.Rmd
geom_mean <- function(x) {
  exp(mean(log(x + 1)) - 1)
}

averagize_at <-
  function(data,
           col,
           ...,
           sep = "_",
           suffix = "avg",
           col_new = paste0(col, sep, suffix)) {
    col_sym <- sym(col)
    col_new_sym <- sym(col_new)
    data %>%
      # summarise(!!col_new_sym := geom_mean(!!col_sym))
      # summarise(!!col_new_sym := exp(mean(log(!!col_sym + 1)) - 1))
      summarise(!!col_new_sym := mean(!!col_sym))
  }

# functions-end ----
#+ viz-vars-1, include=F, eval=F, echo=F
.viz_footer <- "\n\nBy: Tony ElHabr.\nSource: https://www.potomaceconomics.com/markets-monitored/ercot/.\n"
.viz_label_potamac <- "Potomac Economics' \"State of the Market\" Reports on ERCOT"
.viz_label_content <- "figures and tables"
.n_chr_title_wrap <- 90
.n_chr_subtitle_wrap <- 120
.n_chr_footer_wrap <- 120
.units <- "in"
.height_wide <- 6
.width_wide <- 10
.height_tall <- 10
.width_tall <- 6
.height <- 7
.width <- 7

#+ download_sotmreport-1, include=T, eval=F, echo=T
years <- 2016L:2018L
paths <-
  years %>%
  purrr::map_chr(download_sotmreport)
paths

#+ paths-1, include=F, eval=T, echo=F
paths <-
  list.files(
    path = "data-raw",
    pattern = "pdf$",
    full.names = TRUE
  )
paths

#+ paths_INFO-1, include=T, eval=T, echo=T
paths_info <-
  paths %>%
  tibble(path = .) %>%
  mutate(
    year = path %>% str_extract("201[0-8]") %>% as.integer()
  ) %>%
  select(year, path)
paths_info

#+ pages_nest-1, include=T, eval=T, echo=T
pages_nest <-
  paths_info %>%
  mutate(
    page = purrr::map(path, ~pdftools::pdf_text(.))
  ) %>%
  select(-path)
pages_nest

#+ pages-1, include=T, eval=T, echo=T
pages <-
  pages_nest %>%
  unnest(page) %>%
  group_by(year) %>%
  mutate(idx_page = row_number()) %>%
  ungroup()
pages

#+ pages_n-1, include=T, eval=T, echo=T
pages_n <-
  pages_nest %>%
  mutate(n_pages = purrr::map_int(page, ~length(.x))) %>%
  select(-page)
pages_n

#+ toc_pages-1, include=T, eval=T, echo=T
# NOTE: These are the same across all reports.
toc_page_start <- 3L
toc_page_end <- 7L
toc_pages <-
  pages %>%
  filter(idx_page >= 3L, idx_page <= (toc_page_end - 1))
toc_pages

#+ body_pages-1, include=T, eval=T, echo=T
body_pages <-
  pages %>%
  filter(idx_page >= toc_page_end)
body_pages

#+ body_rngs-1, include=F, eval=F, echo=F
# NOTE: Not currently using this! (Maybe can for augmenting `lines_aug`?)
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

#+ toc-1, include=T, eval=T, echo=T
toc <-
  toc_pages %>%
  unnest_pages() %>%
  mutate_at(
    vars(line),
    ~str_to_lower(.) %>%
      str_replace_all("[.]+", "-") %>%
      str_replace_all("201[2-9]", "[year]")
  )
toc

#+ toc_ex-1, include=T, eval=T, echo=T
toc_ex <-
  toc %>%
  filter(year == 2018) %>%
  filter(line %>% str_detect("review of real-time market outcomes| 1[:]"))
toc_ex

#+ toc_aug-1, include=T, eval=T, echo=T
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
    # NOTE: `NA`s will be introduced here.
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

#+ toc_aug_ex-1, include=T, eval=T, echo=T
toc_aug_ex <-
  toc_aug %>%
  filter(year == 2018) %>%
  # filter(label %in% c("review of real-time market outcomes", "real-time market prices"))
  group_by(line_type) %>%
  filter(row_number() == first(row_number())) %>%
  ungroup()
toc_aug_ex

#+ section_rngs-1, include=T, eval=T, echo=T
section_rngs <-
  toc_aug %>%
  filter(line_type == "section") %>%
  group_by(year) %>%
  left_join(pages_n) %>%
  mutate(page_end = coalesce(dplyr::lead(page_num) - 1L, n_pages)) %>%
  mutate(idx_section = row_number()) %>%
  ungroup() %>%
  # NOTE: Reverse the order for the plots.
  mutate_at(
    vars(label),
    ~sprintf("%d. %s", idx_section, label) %>%
      forcats::fct_inorder(.) %>%
      forcats::fct_rev()
  ) %>%
  select(year, page_start = page_num, page_end, section_label = label)
section_rngs

#+ section_rngs-2, include=F, eval=F, echo=F
# ~~FIXME: `page_start` is not really correct! It needs to account for the TOC and Exective Summary.~~

#+ section_label_lvls-1, include=F, eval=T, echo=F
section_label_lvls <- section_rngs %>% pull(section_label) %>% levels()
section_label_lvls

#+ toc_sections-1, include=T, eval=T, echo=T
toc_sections <-
  toc_aug %>%
  filter(line_type %in% c("subsection", "figure", "table")) %>%
  fuzzyjoin::fuzzy_left_join(
    section_rngs,
    by = c(
      "year" = "year",
      "page_num" = "page_start",
      "page_num" = "page_end"
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
  select(-matches("num_|[.]y")) %>%
  rename(year = year.x)
toc_sections

#+ toc_n-1, include=T, eval=T, echo=T
toc_n <-
  toc_sections %>%
  group_by(line_type, label) %>%
  add_count() %>%
  ungroup()
toc_n

#+ toc_n-2, include=F, eval=F, echo=F
toc_n %>% pull(section_label) %>% levels()

#+ toc_content_n-1, include=F, eval=F, echo=F
toc_content_n <- toc_n %>% subset_content()
toc_content_n

#+ toc_n_1yr-1, include=F, eval=F, echo=F
toc_n_1yr <- toc_content_n %>%  filter(year == 2018)
toc_n_1yr

#+ toc_n_1yr_wfl-1, include=F, eval=F, echo=F
toc_n_1yr_wfl <-
  toc_n_1yr %>%
  mutate_at(vars(section_label), as.character) %>%
  ggwaffle::waffle_iron(ggwaffle::aes_d(group = section_label)) %>%
  as_tibble() %>%
  rename(section_label = group) %>%
  mutate_at(vars(section_label), ~factor(., section_label_lvls))
toc_n_1yr_wfl

#+ viz_toc_n_1yr-1, include=F, eval=F, echo=F
viz_toc_n_1yr <-
  toc_n_1yr_wfl %>%
  ggplot() +
  aes(x = x, y = y, fill = section_label) +
  ggwaffle::geom_waffle(color = "black", size = 0.5) +
  coord_equal() +
  scale_fill_section() +
  theme_sotmreport_dark() +
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # strip.background.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs_xy_null() +
  labs(
    fill = "Section",
    title = str_wrap(glue::glue("Composition of {.viz_label_potamac}"), .n_chr_footer_wrap),
    subtitle = str_wrap(
      glue::glue(
        "Counts of {.viz_label_content} appearing in {.viz_label_potamac} in 2018."
        ), .n_chr_title_wrap),
    caption = .viz_footer
  )
viz_toc_n_1yr

#+ viz_toc_n_1yr-2, include=F, eval=F, echo=F
# viz_toc_n_1yr ----
teproj::export_ext_png(
  viz_toc_n_1yr,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)

#+ toc_content_n1-1, include=F, eval=F, echo=F
toc_content_n1 <- toc_content_n %>% filter(n == 1)
toc_content_n1

#+ viz_toc_content_n1-1, include=F, eval=F, echo=F
viz_toc_content_n1 <-
  toc_content_n1 %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  ggplot() +
  aes(x = year, y = n, fill = section_label) +
  geom_col(color = "white") +
  # geom_text(aes(label = section_label, y = max(idx) + 1), color = "black") +
  guides(fill = FALSE) +
  scale_fill_section() +
  theme_sotmreport_dark() +
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
          "Counts of {.viz_label_content} appearing in only 1 of the 3 {.viz_label_potamac} between 2016 and 2018."
          ), .n_chr_title_wrap)
      ),
    caption = paste0(
      str_wrap(
        glue::glue(
          "Day-Ahead Market (DAM) Performance, Reliability Unit Commitments (RUCs),
          and Resource Adequacy received more attention in 2018 than in past years."
        ), .n_chr_footer_wrap),
      .viz_footer
    )
  ) +
  coord_flip()
viz_toc_content_n1

#+ viz_toc_content_n1-2, include=F, eval=F, echo=F
# viz_toc_content_n1 ----
teproj::export_ext_png(
  viz_toc_content_n1,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)

#+ section_rngs_n-1, include=F, eval=F, echo=F
section_rngs_n <-
  toc_sections %>%
  subset_content() %>%
  count(year, section_label) %>%
  left_join(section_rngs) %>%
  mutate(n_pages = page_end - page_start) %>%
  mutate(list_pages_ratio = n / n_pages)
section_rngs_n

#+ section_rngs_n_1yr-1, include=F, eval=F, echo=F
section_rngs_n_1yr <- section_rngs_n %>% filter(year == 2018)
section_rngs_n_1yr

#+ viz_section_rngs_n_1yr-1, include=F, eval=F, echo=F
viz_section_rngs_n_1yr <-
  section_rngs_n_1yr %>%
  ggplot() +
  aes(x = section_label, y = list_pages_ratio, fill = section_label) +
  geom_col() +
  scale_fill_section() +
  theme_sotmreport_dark() +
  theme(
    # legend.position = "left",
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  # theme_sotmreport_dark() +
  labs(
    x = NULL,
    y = glue::glue("Ratio of {.viz_label_content} per page (per section)"),
    fill = "Section",
    title = str_wrap(
      "By coincidence (or not), the sections appearing earlier in the reports have more plots and tables.",
      .n_chr_title_wrap
    ),
    # title = "Which Sections Have a Disproportionate Number of Figures And Tables?",
    subtitle = paste0(
      str_wrap(
        paste0(
          "A higher count of {.viz_label_content} per page (per section) in {.viz_label_potamac} in 2018",
          "is an indicator that these sections have a d"
        ), .n_chr_title_wrap)
    ),
    caption = paste0(
      str_wrap(
        glue::glue(

        ), .n_chr_footer_wrap),
      .viz_footer
    )
  )
viz_section_rngs_n_1yr

#+ viz_section_rngs_n_1yr-2, include=F, eval=F, echo=F
teproj::export_ext_png(
  viz_section_rngs_n_1yr,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)

#+ lines-1, include=T, eval=T, echo=T
lines <-
  body_pages %>%
  unnest_pages() %>%
  mutate_at(vars(line), tolower)
lines

#+ lines-2, include=F, eval=F, echo=F
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

#+ lines_aug-1, include=T, eval=T, echo=T
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

#+ lines_aug-2, include=F, eval=F, echo=F
lines_aug %>% count(year, line_type)
lines_aug %>% count(line_type)
lines_aug %>% head_reports(idx_page)

#+ lines_sections-1, include=T, eval=T, echo=T
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

#+ rgx_months-1, include=F, eval=F, echo=F
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

#+ rgx_zones-1, include=F, eval=F, echo=F
zones <- c("houston", "north", "south", "west")
rgx_zones <-
  zones %>%
  purrr::map_chr(~paste_collapse(., start = "", end = "", collapse = "\\s")) %>%
  paste_collapse_loosely()
rgx_zones

#+ words-1, include=T, eval=T, echo=T
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

#+ words-2, include=F, eval=F, echo=F
# words %>%
#   filter(!is.na(word_date))
# words %>%
#   filter(dplyr::lag(word, 1) == "second")

# stop_words <- stopwords::data_stopwords_snowball["en"] %>% unlist() %>% tibble(word = .)
stop_words <- tidytext::stop_words

#+ words_aug-1, include=T, eval=T, echo=T

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

#+ words_filt-1, include=T, eval=T, echo=T
words_filt <-
  words_aug %>%
  drop_words_generic() %>%
  anti_join(stop_words)
words_filt

#+ lines_redux-1, include=T, eval=T, echo=T
lines_redux <-
  words_aug %>%
  recreate_lines_from_word()
lines_redux

#+ lines_redux-2, include=F, eval=F, echo=F
lines_redux %>% count(line_type)

#+ lines_redux_filt-1, include=T, eval=T, echo=T
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

#+ ngram-2, include=F, eval=F, echo=F
# ngrams %>% filter(line_type == "content") %>% filter(ngram %>% str_detect(zones_rgx, negate = FALSE))

#+ ngrams_filt-1, include=T, eval=T, echo=T
# NOTE: Although `drop_words_generic` (and `anti_join(stop_words)` were already called
# in order to create `words_filt`, `drop_ngrams_generic()` should also be called here.
ngrams_filt <-
  ngrams %>%
  drop_ngrams_generic()
ngrams_filt

#+ n_k_max-1, include=T, eval=T, echo=T
n_k_max <-
  # ngrams_filt %>%
  ngrams %>%
  count(k) %>%
  summarise(temp = max(n)) %>%
  pull(temp)
n_k_max

#+ ngrams_filt_n-1, include=T, eval=T, echo=T
ngrams_filt_n <-
  ngrams_filt %>%
  select(year, ngram, k) %>%
  add_count(k, name = "n_k") %>%
  mutate(n_k_factor =  max(n_k) / n_k)
ngrams_filt_n

#+ ngrams_filt_n-2, include=F, eval=F, echo=F
ngrams_filt_n %>% distinct(k, n_k_factor)

#+ ngrams_tfidf-1, include=T, eval=T, echo=T
ngrams_tfidf <-
  ngrams_filt %>%
  count(year, ngram) %>%
  tidytext::bind_tf_idf(term = ngram, document = year, n = n)
ngrams_tfidf

#+ ngrams_tfidf_aug-1, include=T, eval=T, echo=T
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

#+ ngrams_tfidf_aug-2, include=F, eval=F, echo=F
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

#+ ngrams_tf_maxk-1, include=T, eval=T, echo=T
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


#+ words_n-1, include=T, eval=T, echo=F
words_n <-
  # words_aug %>%
  words_filt %>%
  count(year, word, sort = TRUE)
words_n

#+ words_frac-1, include=T, eval=T, echo=F
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

#+ words_frac_filt-1, include=T, eval=T, echo=T
words_frac_filt <-
  words_frac %>%
  # filter(n_word > 5) %>%
  filter(n_word > 5, word_frac_total > 0.75) %>%
  arrange(desc(word_frac_total))
words_frac_filt

#+ words_tfidf-1, include=T, eval=T, echo=T
words_tfidf <-
  words_n %>%
  tidytext::bind_tf_idf(word, year, n) %>%
  arrange(desc(tf_idf))
words_tfidf

#+ words_section_tfidf-1, include=T, eval=T, echo=T
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

# bookmark ----
viz_words_section_tfidf <-
  words_section_tfidf_filt %>%
  mutate(x = dplyr::if_else(tf_idf > 0.001, 0.001, tf_idf)) %>%
  # mutate(x_inv = 1 / x) %>%
  # count(x < tf_idf) %>%
  ggplot() +
  aes(
    y = section_label,
    x = x,
    # size = x,
    alpha = x,
    group = section_label,
    color = section_label,
  ) +
  scale_alpha_continuous(range = c(0.2, 1)) +
  scale_size_continuous(range = c(0.5, 6)) +
  # geom_jitter() +
  ggbeeswarm::geom_quasirandom(
   # alpha = 0.2,
    groupOnX = FALSE
  ) +
  guides(
    size = FALSE,
    alpha = FALSE,
    color = guide_legend(override.aes = list(size = 5))
  ) +
  # guides(color = guide_legend(nrow = 4)) +
  scale_color_section() +
  theme_sotmreport_dark() +
  scale_x_continuous(limits = c(0, 0.00101), labels = scales::scientific_format(digits = 2)) +
  # lims(x = c(0, 0.005)) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    plot.caption = element_text(hjust = 0)
  ) +
  labs(
    color = "Section",
    title = str_wrap("Which section is the most \"unique\" relative to the others?", .n_chr_title_wrap),
    subtitle = str_wrap(
      glue::glue(
        "TFIDF of words in {.viz_label_potamac}."
      ), .n_chr_title_wrap),
    caption = .viz_footer,
    x = "TFIDF",
    y = NULL
  )
viz_words_section_tfidf


#+ words_section_tfidf-2, include=F, eval=T, echo=T
# words_section_tfidf_viz %>%
#   filter(idx_section > 10, idx_section < 900) %>%
#   lm(formula(log10(tf) ~ log10(idx_section)), data = .)
words_section_tfidf_viz %>%
  ggplot() +
  aes(x = section_label, y = y, group = section_label, color = section_label) +
  scale_x_log10() +
  scale_y_log10() +
  # geom_jitter(alpha = 0.1) +
  geom_line(size = 1) +
  # geom_smooth(size = 1.5, method = "lm", se = FALSE) +
  # geom_smooth(size = 1.5, se = FALSE) +
  scale_color_section() +
  # facet_wrap(~section_label, scales = "free") +
  theme_sotmreport()

#+ viz_tfidf-1, include=F, eval=T, echo=F
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

#+ viz_tfidf_chatter-1, include=F, eval=F, echo=F
# Reference: https://towardsdatascience.com/rip-wordclouds-long-live-chatterplots-e76a76896098
viz_words_tfidf <-
  words_tfidf_filt %>%
  ggplot() +
  aes(x = year, y = n, color = year) +
  ggrepel::geom_text_repel(
    aes(label = word, size = n),
    fontface = "bold",
    box.padding = 0.75,
    min.segment.length = Inf
  ) +
  scale_color_year() +
  scale_size_continuous(range = c(4, 8)) +
  # guides(size = FALSE, color = guide_legend(override.aes = list(size = 5))) +
  guides(size = FALSE, color = FALSE) +
  # labs_xy_null() +
  labs(
    x = NULL,
    y = "Count",
    # color = "Year",
    title = "Which words were the most unique in each report?",
    subtitle = paste0(
      str_wrap(
        glue::glue(
          "Counts of top 10 most unique words (quantified by TFIDF) appearing in ",
          "the {.viz_label_potamac} between 2016 and 2018."
        ), .n_chr_title_wrap)
    ),
    caption = paste0(
      str_wrap(
        glue::glue(
          "The most uniuqe words were those corresponding to regions (e.g. Denton in 2016) ",
          " and causes (e.g. Hurrican Harvey in 2017) of electric transmissoin congestion"
        ), .n_chr_footer_wrap),
      .viz_footer
    )
  ) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank()
  )
viz_words_tfidf

#+ viz_words_tfidf-2, include=F, eval=T, echo=F
teproj::export_ext_png(
  viz_words_tfidf,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)

#+ viz_words_tfidf-manual
words_tfidf %>%
  select(year, word, tf) %>%
  mutate(year = dense_rank(year))

#+ words_tern-1, include=F, eval=T, echo=F
words_tern <-
  words_tfidf %>%
  select(year, word, tf) %>%
  # group_by(year, word) %>%
  # mutate(tf_max = max(tf, na.rm = TRUE)) %>%
  # ungroup() %>%
  spread(year, tf, fill = 0)
words_tern

#+ viz_words_tern-1, include=F, eval=T, echo=F
# Reference: https://d4tagirl.com/2018/01/does-the-twitter-ratio-apply-to-the-rstats-community
# library("ggtern")
arrws <- tibble(
  x = c(1, 0, 0),
  y = c(0, 1, 0),
  z = c(0, 0, 1),
  xend = c(0, 1, 1),
  yend = c(1, 0, 1),
  zend = c(1, 1, 0)
)

# NOTE: Need to import `{ggtern}` for this to work. Also, it seems like the year
# labels have to have tick marks (and not quotes). Otherwise, the following error
# is received: "Error in rowSums(input[, ix.trl]) : 'x' must be numeric".
viz_words_tern <-
  words_tern %>%
  rename_at(vars(-word), ~paste0("Year ", ., "")) %>%
  # rename_at(vars(-word), ~paste0("y", .)) %>%
  # mutate_at(vars(word), as.factor) %>%
  mutate(idx = row_number()) %>%
  mutate(idx_rev = n() - idx + 1) %>%
  mutate(lab = case_when(
    idx <= 10 ~ word,
    TRUE ~ NA_character_
  )
  ) %>%
  arrange(idx_rev) %>%
  # filter(idx <= 4) %>%
  ggtern::ggtern(aes(x = `Year 2016`, y = `Year 2017`, z = `Year 2018`)) +
  geom_point() +
  ggtern::geom_mask() +
  # theme_sotmreport_dark() +
  # ggtern::theme(
  #   # tern.axis.text.T = element_text(color = "white"),
  #   strip.text.x = element_text(color = "blue")
  # ) +
  ggtern::theme_classic() +
  ggtern::theme_hidelabels() +
  # theme(
  #   text = element_text(color = "white"),
  #   # panel.background = element_rect(fill = "white"),
  #   plot.background = element_rect(fill = "black"),
  #   axis.text = element_text(size = 14, color = "white"),
  #   axis.text.x = element_text(size = 14, color = "white"),
  #   axis.text.y = element_text(size = 14, color = "white"),
  #   axis.title = element_text(size = 14),
  #   # strip.text.x = element_text(size = 14, color = "white"),
  #   # strip.text.y = element_text(size = 14, color = "white"),
  #   legend.position = "none"
  # ) +
  labs(
    title = "Ternary Plot",
    subtitle = .viz_label_content,
    caption = .viz_footer
  )

viz_words_tern$labels$x <- "2016"
viz_words_tern$labels$y <- "2017"
viz_words_tern$labels$z <- "2018"
viz_words_tern

#+ viz_words_tern-2, include=F, eval=T, echo=F
teproj::export_ext_png(
  viz_words_tern,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height,
  width = .width
)
# pacman::p_unload("ggtern")
# theme_set(theme_sotmreport_dark())

#+ viz_words_tern-3, include=F, eval=F, echo=F
# TODO: How to add labels?
# UPDATE: It's giving me a lot of trouble (perhaps due to the package not
# being updated in a while), so forget about it.
viz_build <- ggplot_build(viz_words_tern)
viz_build$data[[1]]
viz_build$layout$render

# viz_build$data[[1]] %>% ggplot() + aes(x = x, y = y, z = z) + geom_contour() # + geom_density_2d()
words_tfidf %>%
  mutate(y = n) %>%
  mutate(y = log(n + 1)) %>%
  # filter(y > min(y)) %>%
  arrange(desc(y)) %>%
  ggplot() +
  aes(x = year, y = y) +
  ggbeeswarm::geom_quasirandom(method = "pseudorandom")


