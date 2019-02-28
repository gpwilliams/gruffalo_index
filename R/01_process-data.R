# prepare data for analysis ----

# libraries
library(here)
library(tidyverse)
library(stringdist)

# row-wise max string length function
rmax_string <- function(eng, scots, ...) {
  max(str_length(c(eng, scots)))
}

# read raw data
book <- read_csv(here("data", "01_raw-data", "book_data.csv"))
claws <- read_csv(here("data", "01_raw-data", "CLAWS_data.csv"))

token_freq <- left_join(book, claws, by = "eng") %>%
  mutate(
    book = tolower(book),
    informant = tolower(informant),
    shift_category = case_when(
      diff == 0 ~ "unchanged",
      diff == 1 & lex == 1 ~ "lexical",
      diff == 1 & phon == 1 ~ "phonological",
      diff == 1 & unsure == 1 ~ "unsure"
    ),
    LED = stringdist(eng, scots),
    max_length = pmap_dbl(., rmax_string),
    nLED = LED/max_length
  )

type_freq <- token_freq %>%
  group_by(eng) %>%
  summarise(
    count = n(),
    total_diff = sum(diff, na.rm = TRUE),
    lex = sum(lex, na.rm = TRUE),
    phon = sum(phon, na.rm = TRUE),
    unsure = sum(unsure, na.rm = TRUE),
    none = count - sum(lex, phon, unsure, na.rm = TRUE)
  ) %>%
  select(eng, count, total_diff, none, everything())

# save data
write_csv(token_freq, here("data", "02_processed-data", "token_freq.csv"))
write_csv(type_freq, here("data", "02_processed-data", "type_freq.csv"))