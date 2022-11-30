suppressPackageStartupMessages({
  library(here)
  library(purrr)
  library(dplyr)
  
  library(RSQLite)
})

agencies_df <- readRDS(here("data", "fr_agencies.rds"))

con <- dbConnect(SQLite(), here("data", "eos-many.sqlite"))

eos_df <- dbGetQuery(con, "SELECT * FROM paragraphs LIMIT 100")

mentions <-
  pmap_df(agencies_df, function(...) {
    args <- list(...)
    
    idx <- 
      str_detect(eos_df$data, regex(args$short_name, ignore_case = TRUE)) |> 
      which()
    
    eos_df |> 
      slice(idx) |> 
      select(source, seq) |> 
      mutate(tag = args$short_name)
  }) 

mentions

dbWriteTable(con, "mentions", mentions, append = TRUE)

str_detect(eos_df$data, regex("shall", ignore_case = TRUE)) |> 
  which()

eos_df[.Last.value, "data"]

str_detect(eos_df$data, regex("report", ignore_case = TRUE)) |> 
  which()

eos_df[.Last.value, "data"]

str_detect(eos_df$data, regex("american", ignore_case = TRUE)) |> 
  which()

eos_df[.Last.value, "data"]

dbDisconnect(con)
