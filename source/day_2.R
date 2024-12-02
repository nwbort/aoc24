#--- Script details ------------------------------------------------------------
# Creation date: 2 December 2024
# Project:       aoc23
# Description:   day_2
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

split_data <- data |> 
  mutate(value2 = str_split(value, " ")) |> 
  mutate(value2 = map(value2, as.integer)) |> 
  unnest(cols = value2) |> 
  group_by(value) |> 
  mutate(rn = row_number()) |> 
  group_split()

is_safe <- function(vec) {
  
  test <- vec[2:length(vec)] - vec[1:(length(vec)-1)]
  
  direction <- n_distinct(sign(test)) == 1
  if (!direction) {
    return(FALSE)
  }
  
  rdiff <- range(abs(test))
  if (rdiff[1] >= 1 & rdiff[2] <=3) {
    return(TRUE)
  }
  return(FALSE)
  
}

#--- Part 1 --------------------------------------------------------------------

split_data |> 
  map_lgl(\(df) {is_safe(df$value2)}) |> 
  sum() |>
  as.character() |> 
  writeClipboard()

#--- Part 2 --------------------------------------------------------------------

split_data |> 
  map_lgl(\(df) {
    tester <- df$value2
    if (is_safe(tester)) {
      return(TRUE)
    }
    
    for (i in 1:length(tester)) {
      if (is_safe(tester[-i])) {
        return(TRUE)
      }
    }
    return(FALSE)
    
  }
  ) |> 
  sum() |>
  as.character() |> 
  writeClipboard()
