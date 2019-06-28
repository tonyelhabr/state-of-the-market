
# viz_content_section_n ----
.viz_sents_section_n <-
  sents_section_n %>%
  do_visualize_x_vs_y(
    arrws = FALSE,
    x_cor = 31,
    y_cor = 12,
    x_arrw = 40,
    # x_arrw_up_buffer = -0.5,
    # y_arrw_up_buffer = -1,
    # x_arrw_down_buffer = 1,
    # y_arrw_down_buffer = -1,
    label_arrw = 'sentences'
  )
.viz_sents_section_n

viz_sents_section_n <-
  .viz_sents_section_n +
  scale_color_section() +
  theme_sotmreport() +
  labs(
    x = 'Number of pages in section',
    y = 'Count of total sentences in each section',
    title = glue::glue(
      'Is there a Relationship Beween Counts of Pages and Sentences in Sections?'
    ),
    subtitle = glue::glue(
      'As with the number of pages per section and the number of {viz_label_content} per section,
      there is NO correlation between the number of pages per section and the number of sentences per section.'
    ),
    caption = viz_footer
  )
viz_sents_section_n

g_sents_section_n <- viz_sents_section_n %>% ggplotGrob()
g_sents_section_n %>% pluck ('layout') %>% as_tibble()
g_sents_section_n %>% pluck ('layout') %>% as_tibble() %>% filter(name %>% str_detect('title'))

teproj::export_ext_png(
  viz_sents_section_n,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 10
)

# viz_sents_section_n_yr ----
viz_sents_section_n_yr <-
  sents_section_n %>%
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
      'The number of sententences has increased from 2016 to 2018 for all sections, but some sections
      either saw a small increase or a decrease going from 2016 to 2017, and two (Reliability
      and Resource Adequacy) experienced a noticeable increase going from 2017 to 2018.'
    ),
    caption = viz_footer
  )
viz_sents_section_n_yr

g_sents_section_n_yr <- viz_sents_section_n_yr %>% ggplotGrob()
g_sents_section_n_yr %>% pluck ('layout') %>% as_tibble()
g_sents_section_n_yr %>% pluck ('layout') %>% as_tibble() %>% filter(name %>% str_detect('title'))

teproj::export_ext_png(
  viz_sents_section_n_yr,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 10
)


# viz_sents_section_sim ----
# sents_sim_lvls <- summ_sents_section_sim %>% pull_distinctly(section_label) %>% levels()
# sents_sim_lvls
.data_y <-
  section_labels %>%
  inner_join(summ_sents_section_sim)
.data_y

.labs_y_idx <-
  .data_y %>%
  arrange(idx) %>%
  mutate_at(vars(section_label), as.character) %>%
  distinct(idx, section_label)
.labs_y_idx

.labs_y <-
  .labs_y_idx %>%
  # NOTE: Not sure why, but `pull_distinctly()` isn't working here.
  distinct(section_label) %>%
  pull(section_label)
.labs_y

.colors_section <- ggthemes::ggthemes_data[['tableau']][['color-palettes']][['regular']][['Tableau 10']]

.colors_section_labels <-
  section_labels %>%
  inner_join(.colors_section %>% mutate(idx_section = row_number()))
.colors_section_labels

.colors_section_labels_new <-
  .labs_y_idx %>%
  inner_join(.colors_section_labels %>% select(-section_label, idx = idx_section))
.colors_section_labels_new

.colors_named <-
  .colors_section_labels %>%
  select(section_label, value) %>%
  deframe()
.colors_named

# NOTE: This is technically "wrong", but it gets "corrected" when the y-axis labels are re-ordered
.colors_renamed <-
  .colors_section_labels_new %>%
 #  select(idx, value) %>%
  select(section_label, value) %>%
  deframe()
.colors_renamed

viz_sents_section_sim <-
  sents_sim %>%
  left_join(summ_sents_section_sim %>% select(grp_label, idx)) %>%
  mutate_at(vars(idx), as.character) %>%
  ggplot() +
  aes(x = sim_max, y = section_label, color = section_label) +
  # ggridges::geom_density_ridges() +
  ggbeeswarm::geom_quasirandom(
    alpha = 0.2,
    groupOnX = FALSE
  ) +
  # scale_color_section() +
  scale_color_manual(values = .colors_renamed) +
  geom_text(
    data =
      summ_sents_section_sim %>%
      arrange(grp_label, desc(idx)) %>%
      mutate(lab = ifelse(
        row_number() == 1,
        sprintf('Mean:\n%.3f', sim_max_mean),
        sprintf('%.3f', sim_max_mean)
      )
      ) %>%
      mutate_at(vars(idx), as.character) %>%
      mutate(x = 0.4),
    aes(x = x, label = lab),
    color = 'black',
    # vjust = 1,
    nudge_y = 0.3,
    hjust = 1
  ) +
  scale_y_discrete(labels = .labs_y) +
  scale_x_continuous(
    breaks = c(0.25, 0.5, 0.75, 1),
    labels = c("<=0.25", "0.5", "0.75", "1"),
    limits = c(0.15, 1.1)
  ) +
  # ggthemes::scale_color_tableau(labels = .labs_y) +
  guides(color = FALSE) +
  facet_wrap(~grp_label) +
  # coord_cartesian(xlim = c(0, 1.2)) +
  theme_sotmreport() +
  theme(
    # plot.caption = element_text(hjust = 1),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    color = '',
    title = str_to_title('How Similar Are the Most Similar Sentences From Year to Year?'),
    subtitle = glue::glue(
      'Cosine similarity (a metric used to quantify text similarity) of sentences suggests that there is lots of
      across the reports, irregardless of section.'
    ),
    caption = viz_footer,
    x = 'L2 Normalized Cosine Similarity of Sentences',
    y = NULL
  )
viz_sents_section_sim

g_sents_section_sim <- viz_sents_section_sim %>% ggplotGrob()
g_sents_section_sim %>% pluck ('layout') %>% as_tibble() %>% filter(name %>% str_detect('title'))
g_sents_section_sim %>% pluck ('layout') %>% pull('name')
# g_sents_section_sim %>% gtable::gtable_show_layout()
g_sents_section_sim$layout$l[g_sents_section_sim$layout$name == 'title'] <- 4
g_sents_section_sim$layout$l[g_sents_section_sim$layout$name == 'subtitle'] <- 4
g_sents_section_sim$layout$l[g_sents_section_sim$layout$name == 'caption'] <- 4
g_sents_section_sim %>% grid::grid.draw()

teproj::export_ext_png(
  file = 'viz_sents_section_sim',
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 14
)

# viz_words_section_tfidf ----
# NOTE: This viz takes a different approach with labelling y-axis elements!
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

viz_words_section_tfidf <-
  words_section_tfidf_filt %>%
  mutate(tf_idf = dplyr::if_else(tf_idf > 0.001, 0.001, tf_idf)) %>%
  left_join(summ_words_section_tfidf_filt) %>%
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
    aes(x = 0.00061, y = '2', xend = 0.00016, yend = '1'),
    size = 1,
    # angle = -75,
    curvature = -0.25,
    arrow = create_gg_arrw()
  ) +
  geom_text(
    data = tibble(x = 0.00062, y = '2', lab = glue::glue('"x" = average.')),
    inherit.aes = FALSE,
    aes(x = x, y = y, label = lab),
    # size = 4,
    hjust = 0,
    fontface = 'italic'
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
    data = tibble(x = 0.00075, y = '3', lab = glue::glue('More "unique"')),
    inherit.aes = FALSE,
    aes(x = x, y = y, label = lab),
    # size = 4,
    hjust = 0,
    fontface = 'italic'
  ) +
  scale_y_discrete(
    labels = .labs_y
  ) +
  scale_color_section() +
  scale_x_continuous(
    limits = c(0, 0.001),
    breaks = seq(0, 0.001, length.out = 5),
    labels = c('0', '', '0.0005', '', '>=0.001')
  ) +
  theme_sotmreport() +
  theme(
    # plot.caption = element_text(hjust = 1),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs(
    # color = 'Section',
    title = str_to_title('Which Section Has the Most "Unique" Words Relative to the Others?'),
    subtitle = glue::glue(
      'Term frequency-inverse document frequency (TFIDF) of stemmed words across the 3 reports indicates that
       the Analysis section has the most words (excluding stop words, numbers, and words related to dates).'
    ),
    caption = viz_footer,
    x = glue::glue('TFIDF of words in {.viz_label_potamac}'),
    y = NULL
  )
viz_words_section_tfidf

g_words_section_tfidf <- viz_words_section_tfidf %>% ggplotGrob()
g_words_section_tfidf %>% pluck ('layout') %>% as_tibble() %>% filter(name %>% str_detect('title'))
g_words_section_tfidf %>% pluck ('layout') %>% pull('name')
# g_words_section_tfidf %>% gtable::gtable_show_layout()
g_words_section_tfidf$layout$l[g_words_section_tfidf$layout$name == 'title'] <- 4
g_words_section_tfidf$layout$l[g_words_section_tfidf$layout$name == 'subtitle'] <- 4
g_words_section_tfidf$layout$l[g_words_section_tfidf$layout$name == 'caption'] <- 4
g_words_section_tfidf %>% grid::grid.draw()

teproj::export_ext_png(
  viz_words_section_tfidf,
  # file = 'viz_words_section_tfidf',
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 12
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
    data = tibble(x = '2016', y = 14, lab = glue::glue(
      '* This plot shows counts even though the
      words were identified by highest TFIDF.'
    )),
    aes(x = x, y = y, label = lab),
    size = 4,
    hjust = 1,
    nudge_x = 0.3,
    color = 'black',
    # family = 'Arial',
    fontface = 'italic'
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
    caption = viz_footer
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

