

toc_n_1yr <- toc_content_n %>% filter(year == 2018)
toc_n_1yr

toc_n_1yr_wfl <-
  toc_n_1yr %>%
  # mutate_at(vars(section_label), as.character) %>%
  ggwaffle::waffle_iron(ggwaffle::aes_d(group = idx_section)) %>%
  as_tibble() %>%
  inner_join(section_labels %>% rename(group = idx_section)) %>%
  mutate_at(vars(section_label), ~forcats::fct_reorder(., group))
toc_n_1yr_wfl

# viz_toc_n_1yr ----
labs_toc_n_1yr <- function(...) {
  labs(
    ...,
    x = NULL,
    y = NULL,
    fill = 'Section',
    title = glue::glue(
      'Composition of {.viz_label_potamac} for 2018,
      as told by counts of {.viz_label_content}',
      .envir = .GlobalEnv
    ),
    subtitle = glue::glue(
      'The Real Time Market (RTM) and Day Ahead Market (DAM) sections are filled with the most {.viz_label_content},
      while the Analysis section has the least. Perhaps this is unsuprising because there are lots of
      "standard" ways to track trends in the RTM and DAM. On the other hand, written word seems like a
       more natural means of exposition for a section that is focused on pure investigation. (i.e. the Analysis section) ',
      .envir = .GlobalEnv
    ),
    caption = .viz_footer
  )
}

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
    legend.background = element_rect(fill = 'grey90'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  labs_toc_n_1yr()
viz_toc_n_1yr

teproj::export_ext_png(
  viz_toc_n_1yr,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = .height_wide,
  width = .width_wide
)

# viz_toc_n_1yr_tree ----
summ_toc_n_1yr <-
  toc_n_1yr %>%
  group_by(year, section_label) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(label = sprintf('%s - %d', section_label, n))

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
  theme_sotmreport_dark() +
  labs_toc_n_1yr()
viz_toc_n_1yr_tree

teproj::export_ext_png(
  viz_toc_n_1yr_tree,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 10
)

# viz_section_rngs_n_1yr ----
summ_section_rngs_n_1yr <-
  section_rngs_n_1yr %>%
  summarise_at(vars(n, n_pages), mean) %>%
  mutate(n_ratio = n / n_pages)

.m <- summ_section_rngs_n_1yr %>% pull(n_ratio)
.x1 <- 40
.x2 <- 40
.buffer <- 1
.len <- 2
.get_arrw <- function(x1, len = .len, buffer = .buffer, m = .m, b = 0) {
  m_recipr <- -1 / m
  # x1 <- x1 - buffer
  y1 <- m * x1 + b
  x2 <- x1 - len
  y2 <- m_recipr * (x2 - x1) + y1
  tibble(
    x = x1,
    y = y1,
    xend = x2,
    yend = y2
  )
}

.get_arrw1 <- partial(.get_arrw, x1 = .x1, len = .len)
.get_arrw2 <- partial(.get_arrw, x1 = .x2, len = -.len)

.arrw1 <- .get_arrw1()
.arrw1
.arrw2 <- .get_arrw2()
.arrw2

viz_section_rngs_n_1yr <-
  section_rngs_n_1yr %>%
  ggplot() +
  aes(x = n_pages, y = n, color = section_label) +
  geom_point(size = 5) +
  geom_abline(
    data = summ_section_rngs_n_1yr,
    aes(slope = n_ratio, intercept = 0),
    linetype = 'dashed',
    color = 'black',
    size = 2
  ) +
  geom_segment(
    data = .arrw1,
    inherit.aes = FALSE,
    aes(x = x, y = y, xend = xend, yend = yend),
    size = 2,
    arrow = arrow(length = unit(0.2, 'cm'))
  ) +
  geom_segment(
    data = .arrw2,
    inherit.aes = FALSE,
    aes(x = x, y = y, xend = xend, yend = yend),
    size = 2,
    arrow = arrow(length = unit(0.2, 'cm'))
  ) +
  geom_text(
    data = .arrw1,
    inherit.aes = FALSE,
    aes(x = xend - 2, y = yend + 1),
    # nudge_y = -1,
    # nudge_x = -1,
    # segment.color = NA,
    fontface = 'bold',
    label = glue::glue('More {.viz_label_content} per section')
  ) +
  # ggforce::geom_mark_rect(
  #   data = .arrw2,
  #   inherit.aes = FALSE,
  #   color = NA,
  #   aes(x = xend, y = yend, label = glue::glue('Less {.viz_label_content} per section.'))
  # ) +
  geom_text(
    data = .arrw2,
    inherit.aes = FALSE,
    aes(x = xend + 3, y = yend - 1),
    # nudge_y = -1,
    # nudge_x = 1,
    # segment.color = NA,
    fontface = 'bold',
    label = glue::glue('Less {.viz_label_content} per section')
  ) +
  guides(color = FALSE) +
  ggforce::geom_mark_circle(
    aes(label = section_label)
  ) +
  scale_color_section() +
  coord_cartesian(xlim = c(0, 60), ylim = c(0, 34), expand = TRUE) +
  theme_sotmreport_dark() +
  labs(
    x = 'Number of pages in section',
    y = glue::glue('Count of total {.viz_label_content} in each section'),
    title = glue::glue('Which Sections (in 2018) Have a Disproportionate Number of Figures And Tables?'),
    subtitle = glue::glue(
      'Those sections with higher counts of {.viz_label_content} per page seem to be those with more
          content that is easier to quantify in standard ways (i.e. RTM and DAM).'
    ),
    caption = .viz_footer
  )
viz_section_rngs_n_1yr

teproj::export_ext_png(
  viz_section_rngs_n_1yr,
  export = .export_viz,
  dir = .dir_viz,
  units = .units,
  height = 8,
  width = 11
)

# viz_toc_content_n1 ----
# FIXME! Need a better chart type!
toc_content_n1 <-
  toc_content_n %>%
  filter(n == 1) %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  group_by(section_label) %>%
  mutate(idx_max = max(idx)) %>%
  ungroup()
toc_content_n1

summ_toc_content_n1 <-
  toc_content_n1 %>%
  group_by(section_label) %>%
  summarise(idx_max = max(idx)) %>%
  ungroup()
summ_toc_content_n1

viz_toc_content_n1 <-
  toc_content_n1 %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  ggplot() +
  aes(x = year, y = n, fill = section_label) +
  geom_col(color = 'black') +
  # geom_text(aes(label = section_label, y = max(idx) + 1), color = 'black') +
  guides(fill = FALSE) +
  scale_fill_section() +
  theme_sotmreport_dark() +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  labs_xy_null() +
  # labs(
  #   fill = 'Section',
  #   title = 'How Much Content was Really Unique in Each Report?',
  #   subtitle = paste0(
  #     str_wrap(
  #       glue::glue(
  #         'Counts of {.viz_label_content} appearing in only 1 of the 3 {.viz_label_potamac} between 2016 and 2018.'
  #       ), .n_chr_title_wrap)
  #   ),
  #   caption = paste0(
  #     str_wrap(
  #       glue::glue(
  #         'Day-Ahead Market (DAM) Performance, Reliability Unit Commitments (RUCs),
  #         and Resource Adequacy received more attention in 2018 than in past years.'
  #       ), .n_chr_footer_wrap),
  #     .viz_footer
  #   )
  # ) +
  coord_flip()
viz_toc_content_n1

# teproj::export_ext_png(
#   viz_toc_content_n1,
#   export = .export_viz,
#   dir = .dir_viz,
#   units = .units,
#   height = .height_wide,
#   width = .width_wide
# )
