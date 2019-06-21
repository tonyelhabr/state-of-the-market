

# viz_words_section_tfidf ----
summ_words_section_tfidf_filt  <-
  words_section_tfidf_filt %>%
  group_by(section_label) %>%
  summarise(x = mean(tf_idf)) %>%
  ungroup() %>%
  arrange(x) %>%
  mutate_at(vars(section_label), forcats::fct_inorder)
summ_words_section_tfidf_filt
words_section_tfidf_filt_lvls <- summ_words_section_tfidf_filt %>% pull(section_label)
words_section_tfidf_filt_lvls
viz_words_section_tfidf <-
  words_section_tfidf_filt %>%
  mutate(x = dplyr::if_else(tf_idf > 0.001, 0.001, tf_idf)) %>%
  mutate_at(
    vars(section_label),
    ~as.character(.) %>% factor(levels = words_section_tfidf_filt_lvls)
  ) %>%
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
  geom_point(
    data = summ_words_section_tfidf_filt,
    inherit.aes = FALSE,
    aes(y = section_label, x = x),
    shape = 10,
    size = 5
  ) +
  geom_text(
    data = section_labels,
    inherit.aes = FALSE,
    aes(x = 0.0011, y = section_label, label = section_label),
    fontface = 'bold',
    # size = 10,
    hjust = 0
  ) +
  guides(
    # color = guide_legend(override.aes = list(size = 5))
    color = FALSE,
    size = FALSE,
    alpha = FALSE
  ) +
  scale_color_section() +
  theme_sotmreport_dark() +
  scale_x_continuous(
    limits = c(0, 0.00125),
    breaks = seq(0, 0.001, length.out = 5),
    labels = c("0", "", "0.0005", "", "0.001")
  ) +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    legend.position = 'right',
    plot.caption = element_text(hjust = 0)
  ) +
  labs(
    # color = 'Section',
    title = glue::glue('Which section is the most "unique" relative to the others?'),
    subtitle = '',
    caption = .viz_footer,
    x = glue::glue('TFIDF of words in {.viz_label_potamac}.'),
    y = NULL
  )
viz_words_section_tfidf

teproj::export_ext_png(
  viz_words_section_tfidf,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 7,
  width = 10
)

# viz_words_tfidf ----
# Reference: https://towardsdatascience.com/rip-wordclouds-long-live-chatterplots-e76a76896098
viz_words_tfidf <-
  words_tfidf_filt %>%
  ggplot() +
  aes(x = year, y = n, color = year) +
  ggrepel::geom_text_repel(
    aes(label = word, size = n),
    fontface = 'bold',
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
    y = 'Count',
    # color = 'Year',
    title = 'Which words were the most unique in each report?',
    subtitle = paste0(
      str_wrap(
        glue::glue(
          'Counts of top 10 most unique words (quantified by TFIDF) appearing in
          the {.viz_label_potamac} between 2016 and 2018.'
    ),
    caption =
        glue::glue(
          'The most uniuqe words were those corresponding to regions (e.g. Denton in 2016) ,
           and causes (e.g. Hurrican Harvey in 2017) of electric transmissoin congestion'
    )
  ) +
  theme(
    legend.position = 'right',
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
