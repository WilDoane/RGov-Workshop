# Acquire data, if not already acquired by my past-self

src_path <- "~/brothers_grimm.txt"

if (!file.exists(src_path)) {
  message("File already downloaded")
  download.file("https://www.gutenberg.org/files/2591/2591-0.txt", src_path)
}

# Test a common character encoding
library(stringi)

stringi::stri_enc_list()

# Test a common character encoding
x <- readLines(src_path, encoding = "latin1")

x[1:11]

Encoding(x[1:11])

# Inspect the raw bytes in the file to understand the character encoding
library(hexView)

readRaw(src_path, nbytes = 100, width = 10)

## google ef bb bf
## google ascii table: hexadecimal 54, 68
## google e2 80 99

# Familiarize myself with common hexadecimal values
charToRaw("T")
charToRaw("h")
charToRaw("e")

charToRaw("a")
charToRaw("A")

## space
charToRaw(" ")

## non-breaking space
charToRaw(" ") # not to be confused with the new line character, 0a

charToRaw("\n") # not to be confused with the non-breaking space, c2 a0
charToRaw("\r") 
charToRaw("\t") 

charToRaw("\"") 
charToRaw("'") 
charToRaw("w") 
charToRaw(".") 

lapply(c("h", "t", "t", "p", "s", ":", "/", "/", "w", "w", "w"), charToRaw) |> unlist()


# Which lines contain apostrophes?

x <- readLines(src_path)

grep("'", x)

x[.Last.value]

# Try and fail to up-convert
x <- iconv(x, from = "latin1", to = "UTF-8") # "up-converting" doesn't work

x[1:11]

Encoding(x[1:11])

# Use tools to guess character encoding

stri_enc_detect(file(src_path))

readr::guess_encoding(src_path)

x <- readLines(src_path, encoding = "UTF-8")

Encoding(x[1:11])

x[1:11]

# Down-converting is system dependent... Linux and MacOS work better

x <- iconv(x, from = "UTF-8", to = "ASCII") # "down-converting" can work

x[1:11]

Encoding(x[1:11])

grep("'", x)

# Use regular expressions to find "document" breaks
rex <- "^[A-Z][-, 'A-Z]+$"

grep(rex, x)

length(.Last.value)

grep("^[A-Z][-, 'A-Z]{3,}", x, value = TRUE)

grep(rex, x, value = TRUE)

# Create a data.frame (tbl) with the document data

library(tibble)

x <- tibble(text = readLines(src_path, encoding = "UTF-8"))

x

# ID the start of each "document"

library(dplyr)
library(stringr)

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex))

View(.Last.value)

# Fill in missing document names

library(tidyr)

is.not.na <- Negate(is.na)

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |>                 ###
  filter(is.not.na(document))                            ###

View(.Last.value)

# Add line numbers within each document

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 

  mutate(line = 1:n(), .by = document)                  ###

View(.Last.value)

# Separate lines into words and compute term frequency

library(tidytext)

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  mutate(line = 1:n(), .by = document) |> 
  
  unnest_tokens(word, text) |>                         ###
  
  count(document, word, sort = TRUE)                   ###

# Remove common English language words

stop_words

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  mutate(line = 1:n(), .by = document) |> 
  
  unnest_tokens(word, text) |>                         ###
  
  anti_join(stop_words) |>                             ###
  
  count(document, word, sort = TRUE) |> 
  
  slice_max(n, n = 10, by = document) |>               ###
  
  arrange(document, desc(n))

View(.Last.value)

# Inspect just the top 10 words in each document and visualize

library(ggplot2)

theme_set(theme_bw())

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  mutate(line = 1:n(), .by = document) |> 
  
  unnest_tokens(word, text) |> 
  
  anti_join(stop_words) |> 
  
  count(document, word, sort = TRUE) |> 
  
  slice_max(n, n = 10, by = document) |>               ### top 10ish terms
  
  arrange(document, desc(n)) |> 
  
  ggplot(aes(x = n, y = word)) +                       ### Poor y-axis ordering
  geom_col() +
  
  facet_wrap(.~document,  scales = "free_y")
  

# Too much information, too little order

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  mutate(line = 1:n(), .by = document) |> 
  
  unnest_tokens(word, text) |> 
  
  anti_join(stop_words) |> 
  
  count(document, word, sort = TRUE) |>               ### Notice: no slice
  
  ggplot() +
  geom_col(aes(x = n, y = word)) +
  
  facet_wrap(.~document,  scales = "free_y")

# Taming the high-value data
tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  mutate(line = 1:n(), .by = document) |> 
  
  unnest_tokens(word, text) |> 
  
  anti_join(stop_words) |> 
  
  count(document, word, sort = TRUE) |> 
  
  slice_max(n, n = 5, by = document) |>                          ### and now we slice
  
  ggplot() +
  geom_col(aes(x = n, y = reorder_within(word, n, document))) +  ### reorder_within!!
  scale_y_reordered() + 
  
  facet_wrap(.~document,  scales = "free_y")


#  Some documents appear to carry little data

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  mutate(line = 1:n(), .by = document) |> 
  
  unnest_tokens(word, text) |> 
  
  anti_join(stop_words) |> 
  
  count(document, word, sort = TRUE) |> 
  
  slice_max(n, n = 5, by = document) |>
  
  ggplot() +
  geom_col(aes(x = n, y = reorder_within(word, n, document))) +
  scale_y_reordered() + 
  
  facet_wrap(.~document,  scales = "free_y")


# Remove boilerplate

omit <- c("THE BROTHERS GRIMM FAIRY TALES", "PLEASE READ THIS BEFORE",  ###
"THE FULL PROJECT GUTENBERG", "LIABILITY, BREACH OF WARRANTY",
"TRADEMARK OWNER", "LIABLE TO YOU FOR ACTUAL",
"INCIDENTAL DAMAGES", "OTHER WARRANTIES OF ANY KIND")

omit <- paste(omit, collapse = "|")

tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  mutate(line = 1:n(), .by = document) |> 
  
  filter(!str_starts(document, omit)) |>                                 ###
  
  unnest_tokens(word, text) |> 
  
  anti_join(stop_words) |> 
  
  count(document, word, sort = TRUE) |> 
  
  slice_max(n, n = 5, by = document) |>
  
  ggplot() +
  geom_col(aes(x = n, y = reorder_within(word, n, document))) +
  scale_y_reordered() + 
  
  facet_wrap(.~document,  scales = "free_y", labeller = label_wrap_gen(20)) +
  
  labs(
    x = "Frequency",
    y = "Terms"
  )



# Visualize bigrams
tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  mutate(line = 1:n(), .by = document) |> 
  
  filter(!str_starts(document, omit)) |> 
  
  unnest_tokens(word, text) |>                                   ###
  
  anti_join(stop_words) |>                                       ###
  
  group_by(document, line) |>                                    ###
  summarize(text = paste(word, collapse = " ")) |>               ###
  ungroup() |>                                                   ###
  
  unnest_tokens(word, text, token = "ngrams", n = 2) |>          ###
  
  filter(is.not.na(word)) |> 
  
  count(document, word, sort = TRUE) |> 
  
  slice_max(n, n = 5, with_ties = FALSE, by = document) |>
  
  ggplot() +
  geom_col(aes(x = n, y = reorder_within(word, n, document))) +
  scale_y_reordered() + 
  
  facet_wrap(.~document,  scales = "free_y", labeller = label_wrap_gen(20)) +
  
  labs(
    x = "Frequency",
    y = "Terms"
  )


# Topic Model

library(tidylda)

# Any time you do anything probabilistic, set the seed to ensure reproducibility
set.seed(202310)

model <-
  tibble(text = readLines(src_path, encoding = "UTF-8")) |> 
  mutate(document = str_extract(text, rex)) |> 
  fill(document, .direction = "down") |> 
  filter(is.not.na(document)) |> 
  
  unnest_tokens(word, text) |>          
  anti_join(stop_words) |>
  
  group_by(document) |> 
  summarize(text = paste(word, collapse = " ")) |> 
  ungroup() |> 
  
  unnest_tokens(word, text, token = "ngrams", n = 2) |>          
  count(document, word) |> 
  cast_sparse(document, word, n) |> 
  
  tidylda(
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

# What did we create? A Model!
model

# Did the model "converge"? If not, perhaps more iterations
ggplot(aes(iteration, log_likelihood), data = model$log_likelihood) +
  geom_line() +
  ggtitle("Model Convergence") +
  ylab(NULL)



tidy(model, matrix = "theta")   # P(topic|document)
tidy(model, matrix = "beta")    # P(term|topic)
tidy(model, matrix = "lambda")  # P(topic|term)

model$summary


# What is the most likely topic for each document?
tidy(model, matrix = "theta") |> 
  group_by(document) |> 
  filter(theta == max(theta))

# Which documents belong to each topic?
tidy(model, matrix = "theta") |> 
  group_by(document) |> 
  filter(theta == max(theta)) |> 
  ungroup() |> 
  arrange(topic)

View(.Last.value)

# Visualize which terms are associated with which topics
tidy(model, matrix = "beta")  |> 
  slice_max(beta, n = 5, by = topic) |> 

  ggplot(aes(beta, reorder_within(within = topic, token, by = beta))) +
  geom_bar(stat = "identity") +
  
  facet_wrap(vars(topic), scales = "free") +
  
  scale_y_reordered() +
  
  labs(y = "Term")


