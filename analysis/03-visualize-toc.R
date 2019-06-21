
# toc_n_1yr_wfl %>%
#   group_by(section_label) %>%
#   filter(x == max(x)) %>%
#   summarise(
#     y_max = max(y)
#   ) %>%
#   ungroup()

# viz_toc_n_1yr ----
viz_toc_n_1yr <-
  toc_n_1yr_wfl %>%
  ggplot() +
  aes(x = x, y = y, fill = section_label) +
  ggwaffle::geom_waffle(color = 'black', size = 0.5) +
  coord_equal() +
  scale_fill_section() +
  theme_sotmreport_dark() +
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # strip.background.x = element_blank(),
    legend.background = element_rect(fill = "grey90"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs_xy_null() +
  labs(
    fill = 'Section',
    title = glue::glue(
      'Composition of {.viz_label_potamac} for 2018,
      as told by counts of {.viz_label_content}'
    ),
    subtitle = glue::glue(
      'The Real Time Market (RTM) and Day Ahead Market (DAM) sections are filled with the most {.viz_label_content},
      while the Analysis section has the least. This may be expected because there are lots of a "standard" ways to track the RTM and DAM
      but the analysis is naturally going to invite more verbiage than may be better left to words alone.'
      ),
    caption = .viz_footer
  )
viz_toc_n_1yr

teproj::export_ext_png(
  viz_toc_n_1yr,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)

# viz_section_rngs_n_1yr ----
viz_section_rngs_n_1yr <-
  section_rngs_n_1yr %>%
  ggplot() +
  aes(x = section_label, y = list_pages_ratio, fill = section_label) +
  geom_col() +
  scale_fill_section() +
  theme_sotmreport_dark() +
  theme(
    # legend.position = 'left',
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  # theme_sotmreport_dark() +
  labs(
    x = NULL,
    y = glue::glue('Ratio of {.viz_label_content} per page (per section)'),
    fill = 'Section',
    title = str_wrap(
      'By coincidence (or not), the sections appearing earlier in the reports have more plots and tables.',
      .n_chr_title_wrap
    ),
    # title = 'Which Sections Have a Disproportionate Number of Figures And Tables?',
    subtitle = paste0(
      str_wrap(
        paste0(
          'A higher count of {.viz_label_content} per page (per section) in {.viz_label_potamac} in 2018',
          'is an indicator that these sections have '
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


teproj::export_ext_png(
  viz_section_rngs_n_1yr,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)

# viz_toc_content_n1 ----
viz_toc_content_n1 <-
  toc_content_n1 %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  ggplot() +
  aes(x = year, y = n, fill = section_label) +
  geom_col(color = 'white') +
  # geom_text(aes(label = section_label, y = max(idx) + 1), color = 'black') +
  guides(fill = FALSE) +
  scale_fill_section() +
  theme_sotmreport_dark() +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs_xy_null() +
  labs(
    fill = 'Section',
    title = 'How Much Content was Really Unique in Each Report?',
    subtitle = paste0(
      str_wrap(
        glue::glue(
          'Counts of {.viz_label_content} appearing in only 1 of the 3 {.viz_label_potamac} between 2016 and 2018.'
        ), .n_chr_title_wrap)
    ),
    caption = paste0(
      str_wrap(
        glue::glue(
          'Day-Ahead Market (DAM) Performance, Reliability Unit Commitments (RUCs),
          and Resource Adequacy received more attention in 2018 than in past years.'
        ), .n_chr_footer_wrap),
      .viz_footer
    )
  ) +
  coord_flip()
viz_toc_content_n1

teproj::export_ext_png(
  viz_toc_content_n1,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)
