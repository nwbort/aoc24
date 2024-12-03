#--- Script details ------------------------------------------------------------
# Creation date: 3 December 2024
# Project:       aoc23
# Description:   day_3
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
  filter(!is.na(value)) |> 
  summarise(s = paste0(value, collapse = "")) |> 
  pull(s)


#--- Part 1 --------------------------------------------------------------------

data |> 
  str_extract_all("mul\\([0-9]{1,3},[0-9]{1,3}\\)") |> 
  as_tibble_col() |> 
  unnest(value) |> 
  mutate(nums = str_extract_all(value, "[0-9]{1,3}")) |> 
  unnest_wider(nums, names_sep = "") |> 
  mutate(across(c(nums1, nums2), as.integer)) |> 
  mutate(val = nums1 * nums2) |> 
  summarise(v = sum(val)) |> 
  pull(v) |> 
  as.character() |> 
  writeClipboard()

#--- Part 2 --------------------------------------------------------------------

data |> 
  str_extract_all("mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\)") |> 
  as_tibble_col() |> 
  unnest(value) |> 
  mutate(enabler = case_when(row_number() == 1 | value == "do()" ~ TRUE, value == "don't()" ~ FALSE, TRUE ~ NA)) |> 
  fill(enabler, .direction = "down") |> 
  mutate(nums = str_extract_all(value, "[0-9]{1,3}")) |> 
  unnest_wider(nums, names_sep = "") |> 
  mutate(across(c(nums1, nums2), as.integer)) |> 
  mutate(val = nums1 * nums2 *  enabler) |> 
  filter(!is.na(val)) |> 
  summarise(v = sum(val)) |> 
  pull(v) |> 
  as.character() |> 
  writeClipboard()
