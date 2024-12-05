#--- Script details ------------------------------------------------------------
# Creation date: 1 December 2024
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
  separate(value, into = c("a", "b"), sep = " +", convert = TRUE)

#--- Part 1 --------------------------------------------------------------------

data |>
  summarise(s = sum(abs(sort(a)-sort(b)))) |> 
  pull(s) |> 
  as.character() |> 
  writeClipboard()

#--- Part 2 --------------------------------------------------------------------

data |> 
  count(a = b) |> 
  inner_join(data) |> 
  summarise(s = sum(a * n)) |> 
  pull(s) |> 
  as.character() |> 
  writeClipboard()
