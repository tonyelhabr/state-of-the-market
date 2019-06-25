
.toc_n_1yr_wfl <-
  toc_n_1yr %>%
  # mutate_at(vars(section_label), as.character) %>%
  ggwaffle::waffle_iron(ggwaffle::aes_d(group = idx_section)) %>%
  as_tibble() %>%
  inner_join(section_labels %>% rename(group = idx_section)) %>%
  mutate_at(vars(section_label), ~forcats::fct_reorder(., group))
.toc_n_1yr_wfl

viz_toc_n_1yr_wfl <-
  .toc_n_1yr_wfl %>%
  ggplot() +
  aes(x = x, y = y, fill = section_label) +
  ggwaffle::geom_waffle(color = 'black', size = 0.5) +
  coord_equal() +
  scale_fill_section() +
  theme_sotmreport() +
  theme(
    # axis.text.y = element_blank(),
    # axis.text.x = element_blank(),
    # strip.background.x = element_blank(),
    legend.background = element_rect(fill = 'grey90'),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )
viz_toc_n_1yr_wfl

.edges_1 <-
  section_labels %>%
  mutate(year = "2018")  %>%
  mutate(to = section_label) %>%
  select(from = year, to, section_label) %>%
  mutate_at(vars(to), as.character) %>%
  arrange(from, to)
.edges_1

.to_2 <- c("figure", "table")
.edges_2 <-
  toc_n_1yr %>%
  distinct(section_label, line_type) %>%
  mutate(to = sprintf("%s - %s", section_label, line_type)) %>%
  # mutate(to = line_type) %>%
  mutate(from = section_label %>% as.character()) %>%
  arrange(from, to)
.edges_2

.edges_3 <-
  toc_n_1yr %>%
  mutate(from = sprintf("%s - %s", section_label, line_type)) %>%
  # mutate(from = line_type) %>%
  mutate(index = sprintf("%s - %s - %03d", section_label, line_type, index)) %>%
  select(from = line_type, to = index, section_label, line_type, index)
.edges_3

.edges <-
  bind_rows(.edges_1, .edges_2, .edges_3) %>%
  # filter(from != "2018") %>%
  as_tibble()
.edges
# .edges %>% drop_na()
.graph <-
  .edges %>%
  select(from, to) %>%
  igraph::graph_from_data_frame()
.graph
.graph_tbl <-
  .edges %>%
  select(from, to) %>%
  tidygraph::as_tbl_graph()
.graph_tbl
# .graph_tbl %>% tidygraph::edge_is_between()

viz_dendro <-
  # .graph %>%
  .graph_tbl %>%
  # tidygraph::activate(nodes) %>%
  ggraph::ggraph(layout = 'dendrogram', circular = FALSE) +
  # ggraph::geom_edge_arc() +
  ggraph::geom_edge_diagonal() +
  # ggraph::geom_edge_diagonal(aes(color = to)) +
  # ggraph::geom_node_point() +
  # ggraph::theme_graph() +
  labs(x = NULL, y = NULL)
viz_dendro

