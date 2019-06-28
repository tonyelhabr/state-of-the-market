
# https://stackoverflow.com/questions/48821866/matching-documens-with-text2vec-scaling-problems.
library(tidyverse)
library(text2vec)
library(Matrix)
data("movie_review")

movie_review_clean <-
  movie_review %>%
  mutate_at(vars(review), tolower) %>%
  tidytext::unnest_tokens(output = sentence, input = review, token = 'sentences') %>%
  as_tibble()
movie_review_clean
n <- movie_review %>% nrow()
idx_split <- floor(n / 2)
idx1 <- 1:idx_split
idx2 <- (idx_split + 1):n
x1 <- movie_review_clean %>% slice(idx1) %>% pull(review)
x2 <- movie_review_clean %>% slice(idx2) %>% pull(review)
sim <- compute_token_sim(x1, x2)
sim
it <- movie_review_clean %>% pull(review) %>% itoken(tokenizer = word_tokenizer)
dtm = create_dtm(it, hash_vectorizer(2 ** 14))
dtm



mat_sim = sim2(dtm[1:100,], dtm[101:5000,])
mat_sim = as(mat_sim, "TsparseMatrix")

library(data.table)

# we add 1 because indices in sparse matrices in Matrix package start from 1
mat_sim_dt = data.table(
  row_index = mat_sim@i + 1L,
  col_index = mat_sim@j + 1L,
  value = mat_sim@x
)

res = mat_sim_dt[,
                 {
                   k = which.max(value)
                   list(max_sim = value[[k]], row_index = row_index[[k]])
                 },
                 keyby = col_index]
res
