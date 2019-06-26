
# https://stackoverflow.com/questions/48821866/matching-documens-with-text2vec-scaling-problems.
library(text2vec)
library(Matrix)
data("movie_review")
it = itoken(movie_review$review, tolower, word_tokenizer)
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
