
# library('cowplot')
# viz_content_section_n ----
.viz_sentences_section_n <-
  sentences_section_n %>%
  do_visualize_x_vs_y(
    arrws = FALSE,
    x_cor = 31,
    y_cor = 6,
    x_arrw = 40,
    # x_arrw_up_buffer = -0.5,
    # y_arrw_up_buffer = -1,
    # x_arrw_down_buffer = 1,
    # y_arrw_down_buffer = -1,
    label_arrw = 'sentences'
  )
.viz_sentences_section_n

viz_sentences_section_n <-
  .viz_sentences_section_n +
  scale_color_section() +
  theme_sotmreport() +
  labs(
    x = 'Number of pages in section',
    y = 'Count of total sentences in each section',
    title = glue::glue(
      'Which Sections Have a Disproportionate Number of Sentences?'
    ),
    subtitle = glue::glue(
      'As with the number of pages per section and the number of {.viz_label_content} per section,
      there is NO evidence for a correlation between the number of pages per section
      and the number of sentences per section.'
    ),
    caption = .viz_footer
  )
viz_sentences_section_n

teproj::export_ext_png(
  viz_sentences_section_n,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 10
)

# viz_sentences_section_n_yr ----
viz_sentences_section_n_yr <-
  sentences_section_n %>%
  mutate_at(vars(year), factor) %>%
  ggplot() +
  aes(x = section_label, y = n, group = year, fill = year) +
  # ggalt::geom_lollipop(horizontal = FALSE) +
  geom_col(position = 'dodge') +
  scale_fill_section() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  theme_sotmreport() +
  theme(
    # legend.position = 'bottom',
    # plot.caption = element_text(hjust = 1),
    # axis.text.x = element_text(hjust = 1, vjust = 1, angle = 20),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    fill = 'Year',
    y = 'Count of Sentences',
    x = NULL,
    title = str_to_title('How Has the Count of Sentences Per Section Changed Over Time?'),
    subtitle = glue::glue(
      'The number of sentences has increased from 2016 to 2018 for all sections, but some sections
      either saw a small increase or a decrease going from 2016 to 2017, and two (Reliability
      and Resource Adequacy) experienced a noticeable increase going from 2017 to 2018.'
    ),
    caption = .viz_footer
  )
viz_sentences_section_n_yr

teproj::export_ext_png(
  viz_sentences_section_n_yr,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 10
)

# viz_words_section_tfidf ----
.data_y <-
  section_labels %>%
  inner_join(summ_words_section_tfidf_filt)
.data_y

.labs_y <-
  .data_y %>%
  arrange(idx) %>%
  mutate_at(vars(section_label), as.character) %>%
  pull(section_label)
.labs_y

.viz_words_section_tfidf <-
  words_section_tfidf_filt %>%
  mutate(tf_idf = dplyr::if_else(tf_idf > 0.001, 0.001, tf_idf)) %>%
  left_join(
    summ_words_section_tfidf_filt
  ) %>%
  ggplot() +
  aes(
    y = idx,
    x = tf_idf,
    size = tf_idf,
    # alpha = tf_idf,
    group = section_label,
    color = section_label,
  ) +
  # scale_alpha_continuous(range = c(0.5, 1)) +
  scale_size_continuous(range = c(0.5, 5)) +
  # geom_jitter() +
  ggbeeswarm::geom_quasirandom(
    groupOnX = FALSE
  ) +
  guides(
    # color = guide_legend(override.aes = list(size = 5))
    color = FALSE,
    size = FALSE,
    alpha = FALSE
  ) +
  geom_point(
    data = summ_words_section_tfidf_filt,
    inherit.aes = FALSE,
    aes(y = idx, x = tf_idf_mean),
    shape = 'x',
    fill = 'black',
    size = 20
  ) +
  geom_curve(
    inherit.aes = FALSE,
    aes(x = 0.00052, y = '1', xend = 0.00016, yend = '1'),
    size = 1,
    # angle = -75,
    curvature = -0.25,
    arrow = create_gg_arrw()
  ) +
  geom_text(
    inherit.aes = FALSE,
    aes(x = 0.00053, y = '1'),
    size = 4,
    hjust = 0,
    # family = 'Arial',
    fontface = 'italic',
    label = glue::glue('"x" marks the average.')
  ) +
  geom_segment(
    inherit.aes = FALSE,
    aes(
      x = 0.00089,
      y = '3',
      xend = 0.00096,
      yend = '3'
    ),
    size = 1,
    arrow = create_gg_arrw()
  ) +
  geom_text(
    inherit.aes = FALSE,
    aes(
      x = 0.00077,
      y = '3'
    ),
    size = 4,
    hjust = 0,
    # family = 'Arial',
    fontface = 'italic',
    label = glue::glue('More "unique"')
  ) +
  scale_y_discrete(
    labels = .labs_y
  ) +
  scale_color_section() +
  scale_x_continuous(
    # limits = c(0, 0.0014),
    limits = c(0, 0.001),
    breaks = seq(0, 0.001, length.out = 5),
    labels = c('0', '', '0.0005', '', '>=0.001')
  ) +
  theme_sotmreport() +
  theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    # axis.text.y = element_blank(),
    legend.position = 'right',
    plot.caption = element_text(hjust = 0)
  ) +
  labs(
    # color = 'Section',
    # title = glue::glue('Which section is the most 'unique' relative to the others?'),
    title = '',
    subtitle = '\n\n',
    caption = .viz_footer,
    x = glue::glue('TFIDF of words in {.viz_label_potamac}'),
    y = NULL
  )
.viz_words_section_tfidf

viz_words_section_tfidf <-
  cowplot::ggdraw(.viz_words_section_tfidf) +
  cowplot::draw_text(
    text = str_to_title('Which Section Has the Most "Unique" Words Relative to the Others?'),
    x = 0.01,
    y = 0.99,
    hjust = 0,
    vjust = 1,
    size = 22,
    fontface = 'bold',
    family = 'Arial Narrow'
  ) +
  cowplot::draw_text(
    text = glue::glue(
      'Term frequency-inverse document frequency (TFIDF) of stemmed words across the 3 reports indicates that
       the Analysis section has the most words (excluding stop words, numbers, and words related to dates).'
    ),
    x = 0.01,
    y = 0.95,
    hjust = 0,
    vjust = 1,
    size = 14,
    family = 'Arial'
  )
viz_words_section_tfidf

teproj::export_ext_png(
  viz_words_section_tfidf,
  # file = 'viz_words_section_tfidf',
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
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
  geom_text(
    aes(x = '2016', y = 14),
    size = 4,
    hjust = 1,
    color = 'black',
    # family = 'Arial',
    fontface = 'italic',
    label = glue::glue(
    '* This plot shows counts even though the
      words were identified by highest TFIDF.'
    )
  ) +
  scale_color_year() +
  scale_size_continuous(range = c(6, 9)) +
  expand_limits(y = 0) +
  guides(size = FALSE, color = FALSE) +
  theme_sotmreport() +
  theme(
    legend.position = 'right',
    panel.grid.major.x = element_blank()
  ) +
  # labs_xy_null() +
  labs(
    x = NULL,
    y = 'Count',
    # color = 'Year',
    title = str_to_title('Which Words were the Most Unique in Each Report?'),
    subtitle = glue::glue(
          'The top 10 most unique words identified by TFIDF (after stemming, filtering out stop words, etc.) in each year (relative to the others)
          were those corresponding to regions (e.g. Denton in 2016) and causes (e.g. Hurricane Harvey in 2017) of electric transmission congestion.'
    ),
    caption = .viz_footer
  )
viz_words_tfidf

teproj::export_ext_png(
  viz_words_tfidf,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 7,
  width = 13
)

