library(httr)
library(here)
library(tidyverse)

cookie <- Sys.getenv("AOC_COOKIE")

prep_data <- function(day_number) {
  
  if (!file.exists(here(glue::glue("data/raw/{day_number}.csv")))) {
    
    library(httr)
    
    cookies = c(
      session = cookie
    )
    
    headers = c(
      accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
      `accept-language` = "en,en-AU;q=0.9,en-NZ;q=0.8,en-GB;q=0.7,en-US;q=0.6",
      `cache-control` = "no-cache",
      dnt = "1",
      pragma = "no-cache",
      priority = "u=0, i",
      referer = glue::glue("https://adventofcode.com/2024/day/{day_number}"),
      `sec-ch-ua` = '"Not)A;Brand";v="99", "Google Chrome";v="127", "Chromium";v="127"',
      `sec-ch-ua-mobile` = "?0",
      `sec-ch-ua-platform` = '"Windows"',
      `sec-fetch-dest` = "document",
      `sec-fetch-mode` = "navigate",
      `sec-fetch-site` = "same-origin",
      `sec-fetch-user` = "?1",
      `upgrade-insecure-requests` = "1",
      `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36"
    )
    
    resp <- httr::GET(
      url = glue::glue("https://adventofcode.com/2024/day/{day_number}/input"),
      httr::add_headers(.headers=headers),
      httr::set_cookies(.cookies = cookies)
      )
    
    raw_data <- str_split(content(resp, encoding = "UTF-8"), "\n") |> 
      unlist() |> 
      as_tibble() |>
      mutate(value = ifelse(value == "", NA_character_, value))
    
    write_csv(raw_data, here(glue::glue("data/raw/{day_number}.csv")))
    
  }
}
