
library(tidyverse)
library(text2vec)
.path_import <- 'analysis/analysis.RData'
load(.path_import)
# sentences
# lines_redux

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

sentences <-
  body %>%
  tidytext::unnest_tokens(
    output = sentence,
    input = lines,
    token = 'sentences'
  ) %>%
  group_by(year) %>%
  mutate(idx = row_number()) %>%
  ungroup() %>%
  select(year, idx, everything())
sentences

sentences1 <- sentences %>% filter(year == 2016)
sentences2 <- sentences %>% filter(year == 2017)

do_calcaulate_token_similirity <- function(data, col, value1, value2, ...)  {

}

calculate_token_similarity <- function(x1, x2, ...) {
  it1 <- sentences1 %>% pull(sentence) %>% text2vec::itoken()
  it1
  it2 <- sentences2 %>% pull(sentence) %>% text2vec::itoken()
  it2
  it <- sentences %>% pull(sentence) %>% text2vec::itoken()
  it
  v <- it %>% text2vec::create_vocabulary()
  v
  vectorizer <- v %>% text2vec::vocab_vectorizer()
  vectorizer
}

it1 <- sentences1 %>% pull(sentence) %>% text2vec::itoken()
it1
it2 <- sentences2 %>% pull(sentence) %>% text2vec::itoken()
it2
it <- sentences %>% pull(sentence) %>% text2vec::itoken()
it
v <- it %>% text2vec::create_vocabulary()
v
vectorizer <- v %>% text2vec::vocab_vectorizer()
vectorizer

dtm1 <- it1 %>% text2vec::create_dtm(vectorizer = vectorizer)
dtm1[1:10, 1:10]
dtm2 <- it2 %>% text2vec::create_dtm(vectorizer = vectorizer)
dtm2[1:10, 10:12]
sim_jac <- text2vec::sim2(dtm1, dtm2, method = 'jaccard', norm = 'none')
sim_jac %>% dim()
sim_jac[1:2, 1:5]
sim_cos <- text2vec::sim2(dtm1, dtm2, method = 'cosine', norm = 'l2')
sim_cos %>% dim()
sim_cos[1:2, 1:5]

library(Matrix)
tmat_sim_cos <- as(sim_cos, "TsparseMatrix")
# tmat_sim_cos <- as(sim_jac, "TsparseMatrix")
tmat_sim_cos

library(data.table)

# we add 1 because indices in sparse matrices in Matrix package start from 1
dt_sim_cos_all <- data.table(
  idx1 = tmat_sim_cos@i + 1L,
  idx2 = tmat_sim_cos@j + 1L,
  value = tmat_sim_cos@x
)
dt_sim_cos_all

dt_sim_cos_max <- dt_sim_cos_all[,
                 {
                   k = which.max(value)
                   list(idx1 = idx1[[k]], sim_max = value[[k]])
                 },
                 keyby = idx2]
dt_sim_cos_max
sim_cos_max <-
  dt_sim_cos_max %>%
  as_tibble() %>%
  inner_join(
    sentences %>%
      filter(year == 2016) %>%
      select(idx1 = idx, sentence1 = sentence)
  ) %>%
  inner_join(
    sentences %>%
      filter(year == 2017) %>%
      select(idx2 = idx, sentence2 = sentence)
  )
sim_cos_max
sim_cos_max %>%
  arrange(sim_max)
sim_cos_max %>%
  ggplot() +
  aes(x = sim_max) +
  geom_histogram()
