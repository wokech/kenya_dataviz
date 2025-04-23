# National Parks in Kenya

# Extract Data
# https://crimebythenumbers.com/scrape-table.html
# https://blog.djnavarro.net/posts/2023-06-16_tabulizer/

# 1) Load the relevant packages and libraries

### DID NOT WORK ON THIS FILE SO I USED AN ONLINE TOOL ###
#install.packages("pdftools")
#library(pdftools)

library(readxl)
library(tidyverse)
library(janitor)

# 2) Load the data

# Set the appropriate working directory

nat_parks_ke <- read_excel("sub_pro_7_national_parks_kenya/data/parks.xlsx")

# 3) Wrangle data

nat_parks_ke <- nat_parks_ke[1:32,]

nat_parks_ke[1] <- lapply(nat_parks_ke[1], as.character)
nat_parks_ke[2:6] <- lapply(nat_parks_ke[2:6], as.numeric)

str(nat_parks_ke)

# Clean data
nat_parks_ke_clean <- nat_parks_ke %>%
  clean_names()

nat_parks_ke_clean <- nat_parks_ke_clean %>%
  rename(
    "2018" = "x2018",
    "2019" = "x2019",
    "2020" = "x2020",
    "2021" = "x2021",
    "2022" = "x2022"
  )

nat_parks_ke_clean <- nat_parks_ke_clean %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# EDA
# Yearly totals

nat_parks_ke_clean %>%
  select('2018', '2019', '2020', '2021', '2022') %>%  
  summarise(across(everything(), sum))
  

# 4) Create new datasets

# a) Total Visitors

# b) Parks

# c) Reserves

# d) Mountain

# e) Sanctuary

# f) Urban vs Rural

# g) Nairobi / Kisumu / Mombasa

# h) Lake and Marine

# i) Island