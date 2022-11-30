# https://www.federalregister.gov/presidential-documents/executive-orders
# https://www.federalregister.gov/presidential-documents/executive-orders/joe-biden/2022
# https://www.federalregister.gov/documents/2022/10/19/2022-22834/lowering-prescription-drug-costs-for-americans

suppressPackageStartupMessages({
  library(here)
  library(hexView)
  
  library(purrr)
  library(tidyr)
  library(dplyr)
  
  library(httr)
  library(rvest)
  
  library(stringr)
  
  library(RSQLite)
})

out_path <- here("data-raw", "eos")
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

out_file <- here(out_path, "2022-22834")

if (!file.exists(out_file)) {
  GET(
    "https://www.federalregister.gov/documents/full_text/text/2022/10/19/2022-22834.txt",
    write_disk(out_file)
  ) 
}

# What is _really_ in the file?
viewRaw(out_file, nbytes = 200) 

read_html(out_file) |>
  html_node("body") |> 
  html_text() 

cat(.Last.value)

# Unit of analysis: Sentences ---------------------------------------------

txt <- 
  read_html(out_file) |>
  html_node("body") |> 
  html_text() |> 
  str_replace_all("[\n ]+", " ") 

sents <-
  data.frame(source = basename(out_file), data = txt) |> 
  mutate(data = str_split(data, "\\. ")) |> 
  unnest(data) |> 
  mutate(
    seq = row_number(),
    date_added = as.character(Sys.Date())
  )

# Unit of analysis: Paragraphs --------------------------------------------

txt <- 
  read_html(out_file) |>
  html_node("body") |> 
  html_text() |> 
  str_replace_all("[ ]+", " ") 

paras <- 
  data.frame(source = basename(out_file), data = txt) |> 
  mutate(data = str_split(data, "[\n]{2,}")) |> 
  unnest(data) |> 
  mutate(data = str_replace_all(data, "[\n ]+", " ")) |> 
  mutate(data = str_trim(data)) |> 
  mutate(
    seq = row_number(),
    date_added = as.character(Sys.Date())
  )

# Persist data ------------------------------------------------------------

con <- dbConnect(SQLite(), here("data", "mydata.sqlite"))

dbWriteTable(con, "sentences", sents, append = TRUE)
dbWriteTable(con, "paragraphs", paras, append = TRUE)

dbListTables(con)

dbListFields(con, "sentences")

dbGetQuery(con, "SELECT * FROM sentences WHERE seq % 2 = 0")

dbDisconnect(con)
