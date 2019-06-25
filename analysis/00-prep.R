
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

.subset_line_type <- function(data, .line_type) {
  data %>%
    filter(line_type %in% .line_type)
}

subset_content <- purrr::partial(.subset_line_type, .line_type = c("table", "figure"))

subset_text <- purrr::partial(.subset_line_type, .line_type = c("text"))

.dir_viz <- "output"
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
theme_sotmreport <-
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
           size_grid = 0.25,
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
      panel.grid.major.x = element_line(size = size_grid, color = color_grid),
      panel.grid.major.y = element_line(size = size_grid, color = color_grid),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_line(color = color_grid),
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
      strip.text = element_text(
        # margin = margin(4.4, 4.4, 4.4, 4.4),
        # margin = margin(2, 2, 2, 2),
        face = "bold",
        size = strip.text.size
      ),
      strip.background = element_blank(),
      ...
    )
  }

# ggplot2::theme_set(theme_sotmreport())

hrbrthemes::update_geom_font_defaults(family = 'Arial', size = 4.5)

labs_xy_null <- function(...) {
  labs(
    ...,
    x = NULL,
    y = NULL
  )
}

.width_section_label <- 30
scale_fill_section <- function(..., width = .width_section_label) {
  ggthemes::scale_fill_tableau(labels = function(x) str_wrap(x, width = width), ...)
}

scale_color_section <- function(..., width = .width_section_label) {
  ggthemes::scale_color_tableau(labels = function(x) str_wrap(x, width = width), ...)
}

scale_color_year <- ggthemes::scale_color_gdocs

scale_fill_year <- ggthemes::scale_fill_gdocs

create_gg_arrw <- function(length = unit(0.2, 'cm'), ...) {
  arrow(length = length, type = 'closed', ...)
}
.size_arrw <- 1

.n_years <- 3
.n_row_year_head <- 10
.n_rows_head <- .n_years * .n_row_year_head
head_reports <- function(data, ..., .n_rows = .n_rows_head, .n_row_year = .n_row_year_head) {
  opt_old <- getOption("tibble.print_max")
  # options(tibble.print_min = .n_rows)
  options(tibble.print_max = .n_rows)
  on.exit(options(tibble.print_min = opt_old))
  data %>%
    group_by(year, ...) %>%
    slice(c(1:.n_row_year), .preserve = FALSE) %>%
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
           suffix = "mean",
           col_new = paste0(col, sep, suffix)) {
    col_sym <- sym(col)
    col_new_sym <- sym(col_new)
    data %>%
      summarise(!!col_new_sym := geom_mean(!!col_sym))
      # summarise(!!col_new_sym := exp(mean(log(!!col_sym + 1)) - 1))
      # summarise(!!col_new_sym := mean(!!col_sym))
  }

# functions-end ----
#+ viz-vars-1, include=F, eval=F, echo=F
viz_footer <- "\nBy Tony ElHabr"
.viz_label_potamac <- "Potomac Economics' \"State of the Market\" reports on ERCOT"
viz_label_content <- "figures and tables"
# .n_chr_title_wrap <- 90
# .n_chr_subtitle_wrap <- 120
# .n_chr_footer_wrap <- 120
.units <- "in"
.height_wide <- 6
.width_wide <- 10
# .height_tall <- 10
# .width_tall <- 6
# .height <- 7
# .width <- 7

create_kable <-
  function (data,
            n_show = Inf,
            show_footnote = ifelse(nrow(data) >
                                     n_show, TRUE, FALSE),
            n_footnote = nrow(data),
            format = "html",
            ...,
            full_width = FALSE,
            position = "center") {
    stopifnot(is.data.frame(data))
    res <- data
    if (show_footnote & (n_show < nrow(data))) {
      res <- res[1:n_show,]
    }
    res <- knitr::kable(res, format = format, escape = FALSE)
    if (format == "html") {
      res <- kableExtra::kable_styling(res, full_width = full_width,
                                       position = position)
      if (show_footnote) {
        res <-
          kableExtra::add_footnote(res, c(sprintf(
            "# of total rows: %s",
            .format_total(n_footnote)
          )), notation = "number")
      }
    }
    res
  }

.buffer_arrw <- 1
.len_arrw <- 2
create_gg_arrw_data <-
  function(x1,
           m,
           dir = 'up',
           len = switch(dir, up = .len_arrw, down = -.len_arrw, .len_arrw),
           buffer = .buffer_arrw,
           b = 0) {
    m_recipr <- -1 / m
    # x1 <- x1 - buffer
    y1 <- m * x1 + b
    x2 <- x1 - len
    # formula: (y - y2) = m_recipr * (x - x2)
    y2 <- m_recipr * (x2 - x1) + y1
    tibble(
      x = x1,
      y = y1,
      xend = x2,
      yend = y2
    )
  }
create_gg_arrw_data_up <- function(...) {
  create_gg_arrw_data(dir = 'up', ...)
}

create_gg_arrw_data_down <- function(...) {
  create_gg_arrw_data(dir = 'down', ...)
}


do_visualize_x_vs_y <-
  function(data,
           diagonal = TRUE,
           arrws = FALSE,
           labels = TRUE,
           cor = TRUE,
           col_section = 'section_label',
           col_x = 'n_pages',
           col_y = 'n',
           label_section = 'section',
           label_arrw = '',
           x_arrw = NULL,
           x_arrw_up_buffer = -1,
           y_arrw_up_buffer = 1,
           x_arrw_down_buffer = 1,
           y_arrw_down_buffer = -1,
           x_cor = NULL,
           y_cor = NULL,
           n_chr_wrap = 20,
           ...) {
    stopifnot(is.data.frame(data))
    nms <- data %>% names()
    stopifnot(all(c(col_section, col_x, col_y) %in% nms))

    col_x_sym <- sym(col_x)
    col_y_sym <- sym(col_y)
    col_section_sym <- sym(col_section)

    summ_ratio <-
      data %>%
      summarise_at(
        vars(!!col_x_sym, !!col_y_sym),
        list(mean = mean, min = min, max = max)
      ) %>%
      mutate(n_ratio = n_mean / n_pages_mean)

    m <- summ_ratio %>% pull(n_ratio)
    col_x_max_sym <- sym(sprintf('%s_max', col_x))
    col_x_min_sym <- sym(sprintf('%s_min', col_x))
    x_max <- summ_ratio %>% pull(!!col_x_max_sym)
    x_min <- summ_ratio %>% pull(!!col_x_min_sym)

    # NOTE: Need these for `labels` even if `arrws = FALSE`.
    if(arrws | labels) {
      if (is.null(x_arrw)) {
        # x_arrw <- 0.1 * x_max
        x_arrw <- 0.9 * x_min
      }

      arrw1 <- create_gg_arrw_data_up(x1 = x_arrw, m = m)
      arrw2 <- create_gg_arrw_data_down(x1 = x_arrw, m = m)
    }

    if(cor) {
      cor_ratio <-
        data %>%
        select(!!col_x_sym, !!col_y_sym) %>%
        corrr::correlate(quiet = TRUE) %>%
        select(temp = !!col_x_sym) %>%
        slice(2) %>%
        pull(temp)

      if (is.null(x_cor)) {
        x_cor <- 0.5 * x_max
      }
      if (is.null(y_cor)) {
        y_cor <- (x_cor - 1) * m
      }
    }

    viz <-
      data %>%
      ggplot() +
      aes(x = !!col_x_sym, y = !!col_y_sym, color = !!col_section_sym) +
      geom_point(size = 4)

    if(diagonal) {
      viz <-
        viz +
        geom_abline(
          data = summ_ratio,
          aes(slope = n_ratio, intercept = 0),
          linetype = 'dashed',
          color = 'black',
          size = 2
        )
    }

    if(arrws) {
      viz <-
        viz +
        geom_segment(
          data = arrw1,
          inherit.aes = FALSE,
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend
          ),
          size = 2,
          arrow = create_gg_arrw()
        ) +
        geom_segment(
          data = arrw2,
          inherit.aes = FALSE,
          aes(
            x = x,
            y = y,
            xend = xend,
            yend = yend
          ),
          size = 2,
          arrow = create_gg_arrw()
        )
    }

    if(labels) {
      viz <-
        viz +
        geom_text(
          data = arrw1,
          inherit.aes = FALSE,
          aes(x = xend + x_arrw_up_buffer, y = yend + y_arrw_up_buffer),
          size = 4,
          hjust = 1,
          fontface = 'bold.italic',
          label = str_wrap(glue::glue('More {label_arrw} per {label_section}'), n_chr_wrap)
        ) +
        geom_text(
          data = arrw2,
          inherit.aes = FALSE,
          aes(x = xend + x_arrw_down_buffer, y = yend + y_arrw_down_buffer),
          size = 4,
          hjust = 0,
          fontface = 'bold.italic',
          label = str_wrap(glue::glue('Less {label_arrw} per {label_section}'), n_chr_wrap)
        )

    }

    if(cor) {

      # NOTE: For whatever reason, text is a lot less blurry when `x` and `y`
      # are provided via`data`.
      viz <-
        viz +
        geom_text(
          data = tibble(x = x_cor, y = y_cor),
          inherit.aes = FALSE,
          aes(x = x, y = y),
          size = 4,
          hjust = 0,
          fontface = 'bold.italic',
          label = str_wrap(glue::glue('Overall correlation: {round(cor_ratio, 3)}'), n_chr_wrap)
        ) +
        guides(color = FALSE) +
        ggforce::geom_mark_ellipse(aes(label = section_label)) +
        expand_limits(x = 0, y = 0)
      viz
    }

    viz
  }
