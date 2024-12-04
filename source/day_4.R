#--- Script details ------------------------------------------------------------
# Creation date: 4 December 2024
# Project:       aoc23
# Description:   day_4
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

df <- data |> 
  mutate(value = str_split(value, "")) |> 
  unnest_wider(value, names_sep = "") |> 
  as.matrix()

#--- Part 1 --------------------------------------------------------------------

ans <- 0

for (i in 1:ncol(df)) {
  for (j in 1:nrow(df)) {
    print(i)
    print(j)
    if (df[i, j] != "X") {
      next
    }
    # Look right
    if ((i + 3) <= ncol(df)) {
      if (df[i + 1, j] == "M" & df[i + 2, j] == "A" & df[i + 3, j] == "S") {
        ans <- ans + 1
        
      }
    }
    # Look left
    if ((i - 3) >= 1) {
      if (df[i - 1, j] == "M" & df[i - 2, j] == "A" & df[i - 3, j] == "S") {
        
        ans <- ans + 1
      }
    }
    # Look down
    if ((j + 3) <= nrow(df)) {
      if (df[i, j + 1] == "M" & df[i, j + 2] == "A" & df[i, j + 3] == "S") {
        ans <- ans + 1 
      }
    }
    # Look up
    if ((j - 3) >= 1) {
      if (df[i, j - 1] == "M" & df[i, j - 2] == "A" & df[i, j - 3] == "S") {
        
        ans <- ans + 1
      }
    }
    # Look downright
    if ((i + 3) <= ncol(df) && (j + 3) <= nrow(df)) {
      if (df[i + 1, j + 1] == "M" & df[i + 2, j + 2] == "A" & df[i + 3, j + 3] == "S") {
        
        ans <- ans + 1
      }
    }
    # Look downleft
    if ((i - 3) >= 1 && (j + 3) <= nrow(df)) {
      if (df[i - 1, j + 1] == "M" & df[i - 2, j + 2] == "A" & df[i - 3, j + 3] == "S") {
        
        ans <- ans + 1
      }
    }
    # Look upright
    if ((i + 3) <= ncol(df) && (j - 3) >= 1) {
      if (df[i + 1, j - 1] == "M" & df[i + 2, j - 2] == "A" & df[i + 3, j - 3] == "S") {
        
        ans <- ans + 1
      }
    }
    # Look upleft
    if ((i - 3) >= 1 && (j - 3) >= 1) {
      if (df[i - 1, j - 1] == "M" & df[i - 2, j - 2] == "A" & df[i - 3, j - 3] == "S") {
        
        ans <- ans + 1
      }
    }
    
    
  }
}


as.character() |> 
  writeClipboard()

#--- Part 2 --------------------------------------------------------------------

ans <- 0

for (i in 2:(ncol(df)-1)) {
  for (j in 2:(nrow(df)-1)) {
    if (df[i, j] != "A") {
      next
    }
    
    if (all(sort(c(df[i-1, j-1], df[i+1, j+1])) == c("M", "S"))) {
      if (all(sort(c(df[i-1, j+1], df[i+1, j-1])) == c("M", "S"))) {
        ans <- ans+1
      }
    }
  }
  
}

ans |> 
  as.character() |> 
  writeClipboard()
