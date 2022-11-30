suppressPackageStartupMessages({
  library(janitor)
  
  library(stringr)
  library(humaniformat)
  
  library(tidylda)
})

nih_sample

pis <- 
  nih_sample |>
  select(PI_NAMEs) |> 
  magrittr::set_colnames("pi_names") 

pis <- 
  pis |> 
  mutate(pi_names = str_split(pi_names, ";")) |> 
  unnest(pi_names) |> 
  filter(str_trim(pi_names) != "") |> 
  
  mutate(pi_names = str_replace(pi_names, " \\(contact\\)", "")) |> 
  
  mutate(pi_names = format_reverse(pi_names))

pis

parse_names(pis$pi_names)