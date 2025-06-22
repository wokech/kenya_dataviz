# Kenyan Universities and Basic Info
# Data Sources: 
# https://imis.cue.or.ke/RecognitionAndEquationforQualifications/AccreditedUniversities


#####################
#####PART A
#####################

# 1) Load all the required packages and libraries required for accessing the census data

#install.packages("devtools")
#devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus) # Contains the 2019 Kenya Census data
library(tidyverse)
library(readxl)
library(janitor)
library(scales) # control axis/scale format
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot) # plotting theme
library(gghighlight) # highlight specific data
#install.packages("sf")
library(sf) # simple features
#install.packages("tmap") #Thematic maps 
library(tmap)
#install.packages("leaflet") # Used for creating interactive maps
library(leaflet)
#install.packages("ggbreak")
library(ggbreak)
library(patchwork)
library(ggrepel)
library(ggsflabel)

# 2) View the data available in the data catalogue

data("DataCatalogue")

# 3) Generate the maps using shapefiles and sf package

# The rKenyaCensus package includes a built-in county boundaries 
# dataset to facilitate mapping of the various indicators in the 
# Census (KenyaCounties_SHP)

kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Plot a void map of Kenya
full_map_kenya <- ggplot(kenya_counties_sf) + 
  geom_sf(fill = 'goldenrod3', linewidth = 0.1, color = "black") + 
  theme_void() +
  theme(plot.title = element_text(family = "Helvetica",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12),
        plot.background = element_rect(fill = "azure2", color = "azure2"), 
        panel.background = element_rect(fill = "azure2", color = "azure2"))

full_map_kenya

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/full_map_kenya.png", width = 3, height = 3, dpi = 300)

# Void map for Canva

full_map_kenya_canva <- ggplot(kenya_counties_sf) + 
  geom_sf(fill = 'goldenrod3', linewidth = 0.05, color = 'goldenrod3') + 
  theme_void()

full_map_kenya_canva

# Save the plot
ggsave("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/images/full_map_kenya_canva.png", width = 3, height = 3, dpi = 300)

# Change the title case of the counties and fix the names

unique(kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("/", " ", kenya_counties_sf$County)
kenya_counties_sf$County <- gsub("-", " ", kenya_counties_sf$County)
kenya_counties_sf <- kenya_counties_sf |> 
  mutate(County = tools::toTitleCase(tolower(County)))

# Change to uni data

# # Headquarters data
# # Read in the data
# headquarters <- read_excel("sub_pro_1_kenya_county_sub_county/kenya_single_county_map/datasets/kenyan_county_headquarters.xlsx")
# # Split the latitude and longitude data
# headquarters_split <- headquarters |>
#   separate("Latitude, Longitude", into = c("latitude", "longitude"), sep = ",")  |>
#   mutate(across(everything(), ~trimws(.)))
# # transform to sf data
# headquarters_sf <- st_as_sf(headquarters_split, coords = c("longitude", "latitude"), crs = 4326)
# # transform to match the shapefile CRS
# headquarters_sf <- st_transform(headquarters_sf, st_crs(kenya_counties_sf))


