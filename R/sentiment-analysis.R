suppressPackageStartupMessages({
  library(tidytext)
  
  library(dplyr)
  
  library(ggplot2)
})

theme_set(theme_bw())

con <- dbConnect(SQLite(), here("data", "eos-many.sqlite"))

text_df <- dbGetQuery(con, "SELECT source, seq, data FROM paragraphs")

dbDisconnect(con)

get_sentiments("afinn")
get_sentiments("bin")
get_sentiments("nrc")

nrc <- get_sentiments("nrc")

nrc_trust <- nrc |> 
  filter(sentiment == "trust")

words_df <- text_df |> 
  unnest_tokens(word, data) |> 
  inner_join(nrc_trust)

words_df |> 
  count(source, sort = TRUE) 

words_df |> 
  count(source, sort = TRUE) |> 
  top_n(10) |> 
  
  ggplot(aes(n, reorder(source, n))) +
  geom_col() +
  labs(y = NULL)