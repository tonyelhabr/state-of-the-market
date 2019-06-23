

#+ words_pmi-1, include=F, eval=F, echo=F
# # Reference: https://juliasilge.com/blog/word-vectors-take-two/
# slide_windows <- function(tbl, doc_var, window_size) {
#   # each word gets a skipgram (window_size words) starting on the first
#   # e.g. skipgram 1 starts on word 1, skipgram 2 starts on word 2
#
#   each_total <- tbl %>%
#     group_by(!!doc_var) %>%
#     mutate(doc_total = n(),
#            each_total = pmin(doc_total, window_size, na.rm = TRUE)) %>%
#     pull(each_total)
#
#   rle_each <- rle(each_total)
#   counts <- rle_each[["lengths"]]
#   counts[rle_each$values != window_size] <- 1
#
#   # each word get a skipgram window, starting on the first
#   # account for documents shorter than window
#   id_counts <- rep(rle_each$values, counts)
#   window_id <- rep(seq_along(id_counts), id_counts)
#
#
#   # within each skipgram, there are window_size many offsets
#   indexer <- (seq_along(rle_each[["values"]]) - 1) %>%
#     purrr::map2(rle_each[["values"]] - 1,
#                 ~ seq.int(.x, .x + .y)) %>%
#     purrr::map2(counts, ~ rep(.x, .y)) %>%
#     flatten_int() +
#     window_id
#
#   tbl[indexer, ] %>%
#     bind_cols(data_frame(window_id)) %>%
#     group_by(window_id) %>%
#     filter(n_distinct(!!doc_var) == 1) %>%
#     ungroup
# }
#
# nearest_synonyms <- function(df, token) {
#   df %>%
#     widyr::widely(~ . %*% (.[token, ]), sort = TRUE)(item1, dimension, value) %>%
#     select(-item2)
# }
#
# words_pmi <-
#   lines_redux %>%
#   tidytext::unnest_tokens(output = word, input = line) %>%
#   add_count(word) %>%
#   filter(n >= 20) %>%
#   select(-n) %>%
#   slide_windows(rlang::quo(idx_line), 4) %>%
#   widyr::pairwise_pmi(word, window_id)
# words_pmi
# words_pmi %>% arrange(desc(pmi))
#
# word_vectors <-
#   words_pmi %>%
#   widyr::widely_svd(item1, item2, pmi, nv = 256, maxit = 1000)
# ngram_vectors
# # word_vectors %>% nearest_synonyms("west")


#+ lines_tfidf-1, include=F, eval=F, echo=F
# FIXME: Using this?
lines_tfidf <-
  # lines_redux %>%
  lines_redux_filt %>%
  count(year, line) %>%
  # mutate(n = 1) %>%
  tidytext::bind_tf_idf(line, year, n) %>%
  arrange(desc(tf_idf))
lines_tfidf

n_top <- 10
lines_tfidf_top <-
  lines_tfidf %>%
  group_by(year) %>%
  arrange(desc(tf_idf), .by_group = TRUE) %>%
  filter(line %>% str_detect("\\[|admin", negate = TRUE)) %>%
  slice(c(1:n_top)) %>%
  ungroup()
lines_tfidf_top

#+ lines_redux_n-1, include=F#, eval=F, echo=F
# FIXME: Using this?
lines_redux_n <-
  lines_redux_filt %>%
  count(line_type, line, sort = TRUE) %>%
  # filter(!(line_type %in% c("page_footer", "section_alphanumeric_label"))) %>%
  filter(line_type == "text")
lines_redux_n

lines_redux_n1 <-
  lines_redux_n %>%
  filter(n > 1)
lines_redux_n1

#+ ggtern-1
# library("ggtern")
# pacman::p_unload("ggtern")
# x  <- data.frame(
#   A = c( 0.33, 0.4 ),
#   B = c( 0.33, 0.5),
#   C = c(0.33,0.1)
# )
# ggtern(data=x,aes(A,B,C)) +
#   geom_path(color="green")+
#   geom_point(type="l",shape=21,size=1) +
#   geom_text(label=c("(1/3,1/3,1/3)","(2/5,1/2,1/10)"), color="red", hjust=0, vjust=-1)+
#   theme_classic()

