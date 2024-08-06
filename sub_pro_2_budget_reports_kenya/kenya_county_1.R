# A list of Kenyan counties

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# (B) Get the data from Wikipedia

link <- "https://en.wikipedia.org/wiki/Counties_of_Kenya"
kenya_county <- link %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(header = TRUE, fill = TRUE)

kenya_county_1 <- kenya_county[[3]]
