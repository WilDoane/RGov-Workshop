suppressPackageStartupMessages({
  library(tidytext)
  
  library(RSQLite)
  library(dplyr)
  
  library(igraph)
  library(ggraph)
})

theme_set(theme_bw())

`%!in%` <- Negate(`%in%`)

con <- dbConnect(SQLite(), here("data", "eos-many.sqlite"))

text_df <- dbGetQuery(con, "SELECT source, seq, data FROM paragraphs")

dbDisconnect(con)

bigrams <- 
  text_df |> 
  filter(source == "2022-22834") |> 
  unnest_tokens(bigram, data, token = "ngrams", n = 2) |> 
  separate(bigram, c("word1", "word2"), sep = " ") |> 
  filter(
    word1 %!in% stop_words$word,
    word2 %!in% stop_words$word,
    !is.na(word1),
    !is.na(word2),
    !str_detect(word1, "^\\d+$"),
    !str_detect(word2, "^\\d+$")
  ) 

bigram_counts <- 
  bigrams |> 
  count(word1, word2, sort = TRUE)  

bigram_graph <- 
  bigram_counts %>%
  filter(n > 1) %>%
  graph_from_data_frame()

# Any time you do anything probabilistic, set the seed to ensure reproducibility
set.seed(202211)

# The "fr" layout is probabilistic
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(alpha = n), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
