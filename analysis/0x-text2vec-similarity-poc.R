
library(tidyverse)
library(text2vec)
.path_import <- 'analysis/analysis.RData'
load(.path_import)
# sents_redux_wo_delim
# lines_redux_w_delim

lines_raw <-
  body_pages %>%
  unnest_pages()
lines_raw
body <-
  lines_raw %>%
  group_by(year) %>%
  summarise(
    lines = paste(line, collapse = ' ', sep = '')
  )
body

sents_redux_wo_delim <-
  body %>%
  tidytext::unnest_tokens(
    output = sentence,
    input = lines,
    token = 'sents_redux_wo_delim'
  ) %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  select(year, idx, everything())
sents_redux_wo_delim

# proof of concept (to delete) ----
sentences1 <- sents_redux_wo_delim %>% filter(year == 2016)
sentences2 <- sents_redux_wo_delim %>% filter(year == 2017)
# sentences3 <- sents_redux_wo_delim %>% filter(year == 2018)

it1 <- sentences1 %>% pull(sentence) %>% text2vec::itoken()
it1
it2 <- sentences2 %>% pull(sentence) %>% text2vec::itoken()
it2
it <- sents_redux_wo_delim %>% pull(sentence) %>% text2vec::itoken()
it
v <- it %>% text2vec::create_vocabulary()
v
vectorizer <- v %>% text2vec::vocab_vectorizer()
vectorizer

dtm1 <- it1 %>% text2vec::create_dtm(vectorizer = vectorizer)
dtm1[1:10, 1:10]
dtm2 <- it2 %>% text2vec::create_dtm(vectorizer = vectorizer)
dtm2[1:10, 10:12]
# sim <- text2vec::sim2(dtm1, dtm2, method = 'jaccard', norm = 'none')
# sim %>% dim()
# sim[1:2, 1:5]
sim <- text2vec::sim2(dtm1, dtm2, method = 'cosine', norm = 'l2')
sim %>% dim()
sim[1:2, 1:5]

library(Matrix)
tmat_sim <- as(sim, "TsparseMatrix")
# tmat_sim <- as(sim_jac, "TsparseMatrix")
tmat_sim

library(data.table)

# we add 1 because indices in sparse matrices in Matrix package start from 1
dt_sim_all <- data.table(
  idx1 = tmat_sim@i + 1L,
  idx2 = tmat_sim@j + 1L,
  value = tmat_sim@x
)
dt_sim_all

dt_sim_max <- dt_sim_all[,
                 {
                   k = which.max(value)
                   list(idx2 = idx2[[k]], sim_max = value[[k]])
                 },
                 keyby = idx1]
dt_sim_max
sim_max <-
  dt_sim_max %>%
  as_tibble() %>%
  inner_join(
    sents_redux_wo_delim %>%
      filter(year == 2016) %>%
      select(idx1 = idx, sentence1 = sentence)
  ) %>%
  inner_join(
    sents_redux_wo_delim %>%
      filter(year == 2017) %>%
      select(idx2 = idx, sentence2 = sentence)
  )
sim_max
sim_max %>%
  arrange(sim_max)

viz_sim_max <-
  sim_max %>%
  ggplot() +
  aes(x = sim_max) +
  geom_histogram()
viz_sim_max

viz_sim_max + theme(plot.subtitle = element_text(vjust = 1),
    plot.caption = element_text(vjust = 1),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    plot.title = element_text(family = "serif"),
    panel.background = element_rect(fill = "gray90"),
    plot.background = element_rect(colour = "yellow")) +labs(title = "labels", x = NULL, y = NULL,
    subtitle = "lorem ipsum", caption = "by tony")
