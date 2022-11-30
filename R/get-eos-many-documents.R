# https://www.federalregister.gov/presidential-documents/executive-orders
# https://www.federalregister.gov/developers/documentation/api/v1

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

# Get the summary list of EOs via API -------------------------------------

out_file <- here("data-raw", "all_eos.csv")

if (!file.exists(out_file)) {
  message("Downloading EOs list...")
  
  GET(
    "https://www.federalregister.gov/api/v1/documents.csv?fields[]=document_number&fields[]=raw_text_url&fields[]=president&fields[]=publication_date&fields[]=title&fields[]=topics&per_page=1000&page=1&order=executive_order_number&conditions[type][]=PRESDOCU&conditions[presidential_document_type][]=executive_order",
    write_disk(out_file)
  )
}

# What is _really_ in the file?
viewRaw(out_file, nbytes = 200)

eos <- read.csv(out_file, encoding = "UTF-8")

nrow(eos)

# Is that _all_ of the EOs?


# Harvest -----------------------------------------------------------------

out_path <- here("data-raw", "eos")
dir.create(out_path, recursive = TRUE, showWarnings = FALSE)

pmap(eos, function(...) {
  args <- list(...)
  
  out_file <- here(out_path, str_trim(args$document_number))
  
  if (!file.exists(out_file)) {
    message(args$document_number)
    GET(args$raw_text_url, write_disk(out_file))
    Sys.sleep(0.25)
  }
})

# What is _really_ in the files?
out_file <- here(out_path, str_trim(eos[1, "document_number"]))
viewRaw(out_file, nbytes = 200)


# Preprocess downloaded files ---------------------------------------------

filenames <- list.files(out_path, full.names = TRUE)

con <- dbConnect(SQLite(), here("data", "mydata.sqlite"))

walk(filenames, function(filename) {
  message(basename(filename))
  
  txt <- 
    read_html(filename) |>
    html_node("body") |> 
    html_text()
  
  sents <-
    str_replace_all(txt, "[\n ]+", " ") |> 
    (\(d) data.frame(source = basename(filename), data = d))() |> 
    mutate(data = str_split(data, "\\. ")) |> 
    unnest(data) |> 
    mutate(
      seq = row_number(),
      date_added = as.character(Sys.Date())
    )
  
  paras <- 
    str_replace_all(txt, "[ ]+", " ") |> 
    (\(d) data.frame(source = basename(filename), data = d))() |> 
    mutate(data = str_split(data, "[\n]{2,}")) |> 
    unnest(data) |> 
    mutate(data = str_replace_all(data, "[\n ]+", " ")) |> 
    mutate(data = str_trim(data)) |> 
    mutate(
      seq = row_number(),
      date_added = as.character(Sys.Date())
    )
  
  dbWriteTable(con, "sentences", sents, append = TRUE)
  dbWriteTable(con, "paragraphs", paras, append = TRUE)
})

dbDisconnect(con)

