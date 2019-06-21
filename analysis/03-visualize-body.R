

# viz_words_section_tfidf ----
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


# viz_words_tfidf ----
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

teproj::export_ext_png(
  viz_words_tfidf,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)
