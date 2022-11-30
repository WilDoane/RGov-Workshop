suppressPackageStartupMessages({
  library(RSQLite)
  
  library(tidytext)
  
  library(ggplot2)
})

theme_set(theme_bw())

con <- dbConnect(SQLite(), here("data", "eos-many.sqlite"))

text_df <- dbGetQuery(con, "SELECT source, seq, data FROM paragraphs")

dbDisconnect(con)

text_df %>%
  unnest_tokens(word, data)

text_df %>%
  unnest_tokens(word, data) |> 
  count(word, sort = TRUE)

text_df %>%
  unnest_tokens(word, data) |> 
  anti_join(stop_words) |> 
  count(word, sort = TRUE)

text_df |> 
  unnest_tokens(word, data) |> 
  anti_join(stop_words) |> 
  count(word, sort = TRUE) |> 
  top_n(40) |> 
  mutate(word = reorder(word, n)) |> 
  
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

text_df |> 
  unnest_tokens(word, data) |> 
  anti_join(stop_words) |> 
  group_by(source) |> 
  count(word, sort = TRUE) |> 
  top_n(10) |> 
  ungroup() |> 
  arrange(source, desc(n)) 

text_df |> 
  unnest_tokens(word, data, token = "ngrams", n = 2) |> 
  # anti_join(stop_words) |> 
  group_by(source) |> 
  count(word, sort = TRUE) |> 
  top_n(10) |> 
  ungroup() |> 
  arrange(source, desc(n)) 

text_df |> 
  unnest_tokens(word, data, token = "ngrams", n = 3) |> 
  # anti_join(stop_words) |> 
  group_by(source) |> 
  count(word, sort = TRUE) |> 
  top_n(10) |> 
  ungroup() |> 
  arrange(source, desc(n)) 