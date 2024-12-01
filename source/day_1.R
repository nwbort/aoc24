#--- Script details ------------------------------------------------------------
# Creation date: 1 December 2023
# Project:       aoc23
# Description:   day_1
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)

source(here("source/auth.R"))

#--- Import data ---------------------------------------------------------------

day_number <- rstudioapi::getActiveDocumentContext()$path |> 
    str_remove_all(".*source/|\\.R") |> 
    parse_number()

prep_data(day_number)

data <- read_csv(here(glue::glue("data/raw/{day_number}.csv")))

data <- data |>
    filter(!is.na(value))

#--- Go go go ------------------------------------------------------------------

data <- data |> 
  separate(value, into = c("a", "b"), sep = " +") |> 
  mutate(across(everything(), as.integer))

#--- Part 1 --------------------------------------------------------------------

tibble(a = sort(data$a), b = sort(data$b)) |> 
  mutate(diff = abs(a - b)) |> 
  summarise(s = sum(diff)) |> 
  pull(s) |> 
  as.character() |> 
  writeClipboard()

#--- Part 2 --------------------------------------------------------------------

right_count <- data |> 
  count(b)

data |> 
  select(a) |> 
  left_join(right_count, by = c("a" = "b")) |> 
  replace_na(list(n = 0)) |> 
  mutate(score = a * n) |> 
  summarise(s = sum(score)) |> 
  pull(s) |> 
  as.character() |> 
  writeClipboard()
