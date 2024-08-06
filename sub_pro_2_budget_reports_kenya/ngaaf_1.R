# An analysis of NGAAF distributions to counties

# (A) Load the required libraries

library(tidyverse)
library(rvest)
library(stringr)
library(janitor)
library(gghighlight)
library(readr)

# (B) Get the data from Wikipedia

link <- "https://www.ngaaf.go.ke/index.php/projects"
ngaaf <- link %>%
  read_html("[class='wikitable sortable']") %>% 
  html_table(header = TRUE, fill = TRUE)

ngaaf_1 <- ngaaf[[1]]
