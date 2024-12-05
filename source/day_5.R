#--- Script details ------------------------------------------------------------
# Creation date: 5 December 2024
# Project:       aoc23
# Description:   day_5
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

rules <- data |> 
  filter(str_detect(value, "\\|")) |> 
  separate(value, c("a", "b"), "\\|") |> 
  mutate(across(c(a, b), as.integer))

cases <- data |> 
  filter(!str_detect(value, "\\|")) |> 
  mutate(value = str_split(value, ",")) |> 
  mutate(value = map(value, as.integer))

#--- Part 1 --------------------------------------------------------------------

is_valid <- function(case, rules) {
  
  rules |> 
    mutate(ain = a %in% case, bin = b %in% case) |> 
    filter(ain & bin) |> 
    rowwise() |> 
    mutate(apos = which(a == case), bpos = which(b == case)) |> 
    ungroup() |> 
    mutate(check = apos < bpos) |> 
    summarise(check = all(check)) |> 
    pull(check)
  
  
}

ans <- 0

for (i in 1:nrow(cases)) {
  
  if (is_valid(cases$value[[i]], rules)) {
    ans <- ans + cases$value[[i]][floor(length(cases$value[[i]])/2)+1]
  }
  
}



#--- Part 2 --------------------------------------------------------------------

make_valid <- function(case, rules) {
  
  rel_rules <- rules |> 
    mutate(ain = a %in% case, bin = b %in% case) |> 
    filter(ain & bin) |> 
    rowwise() |> 
    mutate(apos = which(a == case), bpos = which(b == case)) |> 
    ungroup() |> 
    mutate(check = apos < bpos)
  
  while (!is_valid(case, rel_rules |> select(a, b))) {
    new_poss <- rel_rules |> 
      filter(!check) |> 
      head(1) |> 
      select(apos, bpos)
    
    case <- replace(case, c(new_poss$apos, new_poss$bpos), case[c(new_poss$bpos, new_poss$apos)])
    
    rel_rules <- rel_rules |> 
      rowwise() |> 
      mutate(apos = which(a == case), bpos = which(b == case)) |> 
      ungroup() |> 
      mutate(check = apos < bpos)
    
  }
  
  case[floor(length(case)/2)+1]
  
}


ans <- 0

for (i in 1:nrow(cases)) {
  
  if (!is_valid(cases$value[[i]], rules)) {
    
    ans <- ans + make_valid(cases$value[[i]], rules)
    
    
  }
  
}


ans |> 
  as.character() |> 
  writeClipboard()
