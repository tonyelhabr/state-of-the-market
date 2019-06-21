
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
# theme_sotmreport <- function(...) {
#   teplot::theme_te(
#     # base_family = "",
#     base_family = "Arial Narrow",
#     base_size = 12,
#     legend.title = element_text(face = "bold"),
#     legend.position = "right"
#   ) +
#     # hrbrthemes::theme_ipsum() +
#     theme(
#       # legend.key.height = unit(1.5, "cm"),
#       # legend.spacing.y = unit(0.5, "cm"),
#       plot.caption = element_text(hjust = 0),
#       plot.title = element_text(size = 24),
#       plot.subtitle = element_text(size = 16),
#       legend.title = element_text(size = 12),
#       legend.text = element_text(size = 12)
#     )
# }
#
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
  function(base_family = "Arial",
           family_title = "Arial Narrow",
           title.size = 22,
           subtitle.size = 14,
           caption.size = 14,
           axis.text.size = 14,
           axis.text.x.size = 14,
           axis.text.y.size = 14,
           axis.title.size = 16,
           strip.text.size = 18,
           legend_title_size = 14,
           legend_text_size = 12,
           color_bkgrd = "grey95",
           color_grid = ifelse(color_bkgrd == "black", "white", "black"),
           color_text = color_grid,
           color_title = color_text,
           panel.grid.major.x = element_line(size = 0.5, color = color_grid),
           panel.grid.major.y = element_line(size = 0.5, color = color_grid),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank(),
           axis.ticks = element_line(color = color_grid),
           ...) {
    theme(
      text = element_text(family = base_family, color = color_text),
      plot.title = element_text(
        family = family_title,
        face = "bold",
        size = title.size,
        color = color_text
      ),
      plot.subtitle = element_text(size = subtitle.size),
      plot.caption = element_text(size = caption.size, hjust = 0),
      panel.background = element_rect(fill = color_bkgrd),
      plot.background = element_rect(fill = color_bkgrd),
      legend.background = element_rect(fill = color_bkgrd),
      axis.text = element_text(size = axis.text.size, color = color_text),
      axis.text.x = element_text(size = axis.text.x.size, color = color_text),
      axis.text.y = element_text(size = axis.text.y.size, color = color_text),
      axis.title = element_text(size = axis.title.size),
      axis.title.x = element_text(hjust = 1),
      axis.title.y = element_text(hjust = 1),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      legend.title = element_text(
        face = "bold",
        size = legend_title_size
      ),
      legend.text = element_text(
        size = legend_text_size
      ),
      panel.grid.major.x = panel.grid.major.x,
      panel.grid.major.y = panel.grid.major.y,
      panel.grid.minor.x = panel.grid.minor.x,
      panel.grid.minor.y = panel.grid.minor.y,
      strip.text = element_text(
        # margin = margin(4.4, 4.4, 4.4, 4.4),
        # margin = margin(2, 2, 2, 2),
        face = "bold",
        size = strip.text.size
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
.viz_footer <- "\nBy Tony ElHabr"
.viz_label_potamac <- "Potomac Economics' \"State of the Market\" reports on ERCOT"
.viz_label_content <- "figures and tables"
# .n_chr_title_wrap <- 90
# .n_chr_subtitle_wrap <- 120
# .n_chr_footer_wrap <- 120
.units <- "in"
.height_wide <- 6
.width_wide <- 10
.height_tall <- 10
.width_tall <- 6
.height <- 7
.width <- 7
