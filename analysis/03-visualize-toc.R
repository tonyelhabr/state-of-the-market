
# viz_toc_n_1yr_tree ----
viz_toc_n_1yr_tree <-
  summ_toc_n_1yr %>%
  ggplot() +
  aes(area = n, fill = section_label) +
  treemapify::geom_treemap(color = 'black', size = 0.5) +
  treemapify::geom_treemap_text(
    aes(label = label),
    fontface = 'bold',
    place = 'centre'
  ) +
  coord_cartesian(clip = 'off') +
  scale_fill_section() +
  guides(fill = FALSE) +
  theme_sotmreport() +
  labs(
    x = NULL,
    y = NULL,
    fill = 'Section',
    title = str_to_title(
      glue::glue(
        'Composition of {.viz_label_potamac}
       in 2018, as Told by Counts of {str_to_title(.viz_label_content)}'
      )
    ),
    subtitle = glue::glue(
      'The Real Time Market (RTM) and Day Ahead Market (DAM) sections are filled with the most {.viz_label_content},
      while the Analysis section has the least. The RTM and DAM are arguably "easier" to quantify, so use of
      {.viz_label_content} may be more natural. On the other hand, written word may be a more
      natural form of exposition for a section that is focused on investigation (i.e. the Analysis section).'
    ),
    caption = .viz_footer
  )
viz_toc_n_1yr_tree

teproj::export_ext_png(
  viz_toc_n_1yr_tree,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 10
)

# viz_content_section_n_1yr ----
.viz_content_section_n <-
  section_rngs_n %>%
  do_visualize_x_vs_y(
    x_arrw = 8.5,
    x_cor = 31,
    y_cor = 6,
    x_arrw_up_buffer = -0.5,
    y_arrw_up_buffer = -1,
    x_arrw_down_buffer = 1,
    y_arrw_down_buffer = -1,
    label_arrw = .viz_label_content
  )
.viz_content_section_n

viz_content_section_n <-
  .viz_content_section_n +
  scale_color_section() +
  theme_sotmreport() +
  labs(
    x = 'Number of pages in section',
    y = glue::glue('Count of total {.viz_label_content} in each section'),
    title = str_to_title(
      glue::glue(
        'Which Sections Have a Disproportionate Number of {str_to_title(.viz_label_content)}?'
      )
    ),
    subtitle = glue::glue(
      'A higher number of pages in a section does NOT imply more {.viz_label_content}.
      Instead, the sections with higher counts of {.viz_label_content} per page seem to be those with more
      content that is easier to quantify in "standard" ways (i.e. RTM and DAM).'
    ),
    caption = .viz_footer
  )
viz_content_section_n

teproj::export_ext_png(
  viz_content_section_n,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 11
)

# viz_toc_content_n1 ----
viz_toc_content_n1 <-
  toc_content_n1 %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  ggplot() +
  aes(x = year, y = n, fill = section_label) +
  geom_col(color = 'black') +
  # guides(fill = guide_legend(override.aes = list(size = 2))) +
  scale_fill_section() +
  geom_curve(
    inherit.aes = FALSE,
    aes(
      x = 2017.2,
      y = 10.5,
      xend = 2017.5,
      yend = 8.5
    ),
    size = 1,
    # angle = -75,
    curvature = -0.5,
    arrow = create_gg_arrw()
  ) +
  geom_text(
    inherit.aes = FALSE,
    aes(x = 2017.2, y = 10.8),
    size = 4,
    hjust = 0,
    # family = 'Arial',
    fontface = 'italic',
    label = glue::glue('Each block indicates a
                       single figure or table.')
  ) +
  theme_sotmreport() +
  theme(panel.grid.major.y = element_blank()) +
  labs_xy_null() +
  labs(
    fill = 'Section',
    title = str_to_title(
      glue::glue(
        'How Much Content ({str_to_title(.viz_label_content)}) was Unique to Each Report?'
      )
    ),
    subtitle = glue::glue(
      'Identification of {.viz_label_content} appearing in only 1 of the 3 {.viz_label_potamac}
      between 2016 and 2018 indicates that Day-Ahead Market (DAM), Reliability Unit Commitments (RUCs),
      and Resource Adequacy received more attention in 2018 than in past years.'
    ),
    caption = .viz_footer
  ) +
  coord_flip()
viz_toc_content_n1

teproj::export_ext_png(
  viz_toc_content_n1,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 12
)
