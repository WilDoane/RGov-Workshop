suppressPackageStartupMessages({
  library(tidytext)
  
  library(RSQLite)
  library(dplyr)
  
  library(wordcloud)
})

theme_set(theme_bw())

con <- dbConnect(SQLite(), here("data", "eos-many.sqlite"))

text_df <- dbGetQuery(con, "SELECT source, seq, data FROM paragraphs")

dbDisconnect(con)

text_df |> 
  filter(source == "2022-22834") |> 
  unnest_tokens(word, data) |> 
  anti_join(stop_words) |> 
  
  count(word, sort = TRUE) |>
  with(wordcloud(word, n))