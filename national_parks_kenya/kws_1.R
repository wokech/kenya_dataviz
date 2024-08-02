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

nat_parks_ke <- read_excel("parks.xlsx")

# 3) Wrangle data

nat_parks_ke[2:6] <- lapply(nat_parks_ke[2:6], as.character)
nat_parks_ke[2:6] <- lapply(nat_parks_ke[2:6], as.numeric)

str(nat_parks_ke)

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