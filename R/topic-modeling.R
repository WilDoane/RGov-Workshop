# https://github.com/TommyJones/tidylda
suppressPackageStartupMessages({
  library(janitor)
  library(stringr)
  library(tidytext)
  library(ggplot2)
  library(tidylda)
  library(Matrix)
})

docs <- tidylda::nih_sample 

tidy_docs <- 
  docs |> 
  clean_names() |> 
  select(application_id, abstract_text) |> 
  unnest_tokens(output = word, 
                input = abstract_text,
                stopwords = stop_words$word,
                token = "ngrams",
                n_min = 1, n = 2) |> 
  count(application_id, word) |> 
  filter(n > 1) |> 
  
  filter(!str_detect(word, "^[0-9]+$")) 

colnames(tidy_docs)[1:2] <- c("document", "term")

d <- 
  tidy_docs %>% 
  cast_sparse(document, term, n)

# Any time you do anything probabilistic, set the seed to ensure reproducibility
set.seed(202211)

# LDA is a probabilistic algorithm
lda <- tidylda(
  data = d,
  k = 10,
  iterations = 200, 
  burnin = 175,
  alpha = 0.1, # prior for topics over documents
  eta = 0.05,  # prior for words over topics
  optimize_alpha = FALSE, # experimental
  calc_likelihood = TRUE,
  calc_r2 = TRUE, # see https://arxiv.org/abs/1911.11061
  return_data = FALSE
)

ggplot(aes(iteration, log_likelihood), data = lda$log_likelihood) +
  geom_line() +
  ggtitle("Model Convergence") +
  ylab(NULL)

lda

tidy(lda, matrix = "theta")   # P(topic|document)
tidy(lda, matrix = "beta")    # P(token|topic)
tidy(lda, matrix = "lambda")  # P(topic|token)

tidy(lda, matrix = "theta") |> 
  group_by(document) |> 
  filter(theta == max(theta))

tidy(lda, matrix = "theta") |> 
  group_by(document) |> 
  filter(theta == max(theta)) |> 
  ungroup() |> 
  arrange(topic)