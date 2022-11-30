library(httr)
library(jsonlite)

library(dplyr)

agencies <- GET("https://www.federalregister.gov/api/v1/agencies")

str(agencies)

txt <- content(agencies, as = "text")

print(txt)

cat(txt)

prettify(txt)

agencies_df <- 
  content(agencies, as = "text") |>
  fromJSON() |>
  as_tibble()

head(agencies_df)

names(agencies_df)

agencies_df |>
  select(short_name, name)