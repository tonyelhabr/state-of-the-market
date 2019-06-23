

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
  # () +
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
# teproj::export_ext_png(
#   viz_words_tern,
#   export = .export_viz,
#   dir = .dir_viz,
#   units = .units,
#   height = .height,
#   width = .width
# )
# pacman::p_unload("ggtern")
# theme_set(())

# TODO: How to add labels?
# UPDATE: It's giving me a lot of trouble (perhaps due to the package not
# being updated in a while), so forget about it.
viz_build <- ggplot_build(viz_words_tern)
# viz_build$data[[1]]
# viz_build$layout$render
