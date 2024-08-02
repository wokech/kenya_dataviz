# Title: Disturbed and Dangerous
# Author: William Okech
# Date Modified: 11/28/2023

# In February 2023, the government of Kenya described six counties as 
# "disturbed" and "dangerous." This is because in the preceding six months, 
# over 100 civilians and 16 police officers have lost their lives as criminals 
# engaged in banditry and livestock theft have terrorized the rural villages.

# References
# 1) https://nation.africa/kenya/news/killing-fields-kdf-police-versus-bandits-who-will-prevail--4122912
# 2) https://www.youtube.com/watch?v=nOqzHeeSS2A

# One of the main causes of the conflict is livestock theft, therefore, the goal 
# of this analysis is to perform an exploratory data analysis of livestock
# numbers from the Kenya Population and Housing Census (2019) report.

# Section 1: Load all the required libraries

library(tidyverse) # a collection of packages used to model, transform, and visualize data
library(rKenyaCensus) # tidy datasets obtained from the Kenya Population and Housing Census results
library(patchwork) # combine separate ggplots into the same graphic
library(janitor) # initial data exploration and cleaning for a new data set
library(ggrepel)# repel overlapping text labels
library(ggthemes) # Extra Themes, Scales and Geoms for 'ggplot2'
library(scales) # tools to override the default breaks, labels, transformations and palettes
# install.packages("treemapify")
library(treemapify) # allows the creation of treemaps in ggplot2

library(sf) # simple features, a method to encode spatial vector data
#install.packages("devtools")
library(devtools) # helps to install packages not on CRAN
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel) # place labels on maps

library(knitr) # a tool for dynamic report generation in R
#install.packages("kableExtra")
library(kableExtra) # build common complex tables and manipulate table styles

# Note: If you have package loading issues use options (timeout = ___) to 
# increase package loading time.

# Section 2: Create a map of the "dangerous and disturbed" counties

# The rKenyaCensus package includes a built-in county boundaries dataset to 
# facilitate mapping of the various indicators in the Census.

# The required shapefile is KenyaCounties_SHP

# a) Sample plot of the map of Kenya

# Load the shapefile
kenya_counties_sf <- st_as_sf(KenyaCounties_SHP)

# Plot the map of Kenya
p0 <- ggplot(kenya_counties_sf) + 
  geom_sf(fill = "bisque2", linewidth = 0.6, color = "black") + 
  theme_void()

p0

# Save the image
#ggsave("sub_pro_1_danger_disturb/images/kenya_map_1.png", width = 11.25, height = 11.25, dpi = 72)


# b) Highlight the dangerous and disturbed counties in Kenya

# First, remove the "/" from the county names

kenya_counties_sf$County <- gsub("/", 
                                 " ", 
                                 kenya_counties_sf$County)

# c) Highlight the required area

# Select the six counties to highlight
highlight_counties <- c("TURKANA", "WEST POKOT", "ELGEYO MARAKWET", "BARINGO", "LAIKIPIA", "SAMBURU")

# Filter the counties dataset to only include the highlighted counties
highlighted <- kenya_counties_sf %>% filter(County %in% highlight_counties)

# Plot the highlighted counties in the map
p1 <- ggplot() + 
  geom_sf(data = kenya_counties_sf, fill = "bisque2", linewidth = 0.6, color = "black") + 
  geom_sf(data  = highlighted, fill = "chocolate4", linewidth = 0.8, color = "black") +
  theme_void()
p1

#ggsave("sub_pro_1_danger_disturb/images/kenya_map_2.png", width = 11.25, height = 11.25, dpi =600)

# create a ggplot2 plot with the highlighted counties
p2 <- ggplot(data = highlighted) +
  geom_sf(aes(fill = County), linewidth = 1, show.legend = FALSE) +
  geom_sf_label_repel(aes(label = County), size = 3) +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "",
       caption = "") +
  theme(plot.title = element_text(family = "Helvetica",size = 10, hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption = element_text(family = "Helvetica",size = 12)) +
  theme_void() 
p2

#ggsave("sub_pro_1_danger_disturb/images/county_map_1.png", width = 11.25, height = 11.25, dpi =600)

# Combine the plots using patchwork to clearly highlight the counties of interest 
p1 + 
  p2 + 
  plot_annotation(title = "",
                  subtitle = "",
                  caption = "",
                  theme = theme(plot.title = element_text(family="Helvetica", face="bold", size = 25),
                                plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
                                plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
                                plot.background = element_rect(fill = "bisque1"))) &
  theme(text = element_text('Helvetica'))

#ggsave("sub_pro_1_danger_disturb/images/county_map_combi.png", width = 11.25, height = 11.25, dpi =600)

# Section 3: Load the livestock data from the census report and generate the
# dataframes required for analysis.

# a) View the data available in the data catalogue

data("DataCatalogue")
DataCatalogue

# b) Load the livestock data

# Select the livestock data from the census report
df_livestock <- V4_T2.24
livestock <- df_livestock[2:393, ]
livestock <- livestock %>%
  clean_names()

# Remove the "/" from the county names in the dataset
livestock$county <- gsub("/", " ", livestock$county)
livestock$sub_county <- gsub("/", " ", livestock$sub_county)

# Select the variables of interest from the dataset
# These include the county, subcounty, land area, number of farming households, 
# sheep, goats, and indigenous cattle.

# New variables listed below include:
# pasto_livestock is the total number of sheep, goats, and indigenous cattle
# ind_cattle_household is the number of indigenous cattle per household
# goats_household is the number of goats per household
# sheep_household is the number of sheep per household
# pasto_livestock_household is the number of pastoral livestock per household

livestock_select <- livestock %>%
  select(county, sub_county, admin_area, farming, sheep, goats, indigenous_cattle) %>%
  mutate(pasto_livestock = sheep + goats + indigenous_cattle) %>% 
  mutate(ind_cattle_household = round(indigenous_cattle/farming)) %>%
  mutate(goats_household = round(goats/farming)) %>%
  mutate(sheep_household = round(sheep/farming)) %>%
  mutate(pasto_livestock_household = round(pasto_livestock/farming))

# c) Filter data for the selected "disturbed and dangerous" counties

# Select the data for the "dangerous and disturbed" counties
dan_dist <- c("TURKANA", "WEST POKOT", "ELGEYO MARAKWET", "BARINGO", "LAIKIPIA", "SAMBURU")

livestock_select_county <- livestock_select %>%
  filter(admin_area == "County") %>%
  filter(county %in% dan_dist)

# Select subcounty data for the "disturbed and dangerous" counties
livestock_select_subcounty <- livestock_select %>%
  filter(admin_area == "SubCounty") %>%
  filter(county %in% dan_dist)

# Create an area dataset for the "dangerous and disturbed" counties
df_land_area <- V1_T2.7
land_area <- df_land_area[2:396,]
land_area <- land_area %>%
  clean_names()

# Create a function to remove the "/", " County" from label, and change the label to UPPERCASE

clean_county_names <- function(dataframe, column_name) {
  dataframe[[column_name]] <- toupper(gsub("/", " ", gsub(" County", "", dataframe[[column_name]])))
  return(dataframe)
}

land_area <- clean_county_names(land_area, 'county')
land_area <- clean_county_names(land_area, 'sub_county')

# The code above does the processes listed below:

# land_area$county <- gsub("/", " ", land_area$county)
# land_area$county <- gsub(" County", "", land_area$county)
# land_area$county <- toupper(land_area$county)
# land_area$sub_county <- toupper(land_area$sub_county)

# Obtain the area data for "disturbed and dangerous" counties
land_area_county <- land_area %>%
  filter(admin_area == "County") %>%
  select(county, land_area_in_sq_km) %>%
  filter(county %in% dan_dist)

# Get the subcounty area data for "disturbed and dangerous" counties
land_area_subcounty <- land_area %>%
  filter(admin_area == "SubCounty") %>%
  select(county, sub_county, land_area_in_sq_km) %>%
  filter(county %in% dan_dist) %>%
  select(-county)

# d) Create the final datasets to be used for analysis. 
#    Use inner_join() and creating new variables.

# Create a county dataset with area and livestock numbers for the disturbed and dangerous regions

livestock_area_county <- inner_join(livestock_select_county, land_area_county, by = "county")

# New variables listed below include:
# ind_cattle_area is the number of indigenous cattle per area_in_sq_km
# goats_area is the number of goats per household per area_in_sq_km
# sheep_area is the number of sheep per area_in_sq_km
# pasto_livestock_area is the number of pastoral livestock per area_in_sq_km

livestock_area_county <- livestock_area_county %>%
  mutate(ind_cattle_area = round(indigenous_cattle/land_area_in_sq_km),
         sheep_area = round(sheep/land_area_in_sq_km),
         goats_area = round(goats/land_area_in_sq_km),
         pasto_livestock_area = round(pasto_livestock/land_area_in_sq_km))

# Create a subcounty dataset with area and livestock numbers
# for the disturbed and dangerous regions

livestock_area_subcounty <- inner_join(livestock_select_subcounty, land_area_subcounty, by = "sub_county")

# New variables listed below include:
# ind_cattle_area is the number of indigenous cattle per area_in_sq_km
# goats_area is the number of goats per household per area_in_sq_km
# sheep_area is the number of sheep per area_in_sq_km
# pasto_livestock_area is the number of pastoral livestock per area_in_sq_km

livestock_area_subcounty <- livestock_area_subcounty %>%
  mutate(ind_cattle_area = round(indigenous_cattle/land_area_in_sq_km),
         sheep_area = round(sheep/land_area_in_sq_km),
         goats_area = round(goats/land_area_in_sq_km),
         pasto_livestock_area = round(pasto_livestock/land_area_in_sq_km))

# Section 4: Create a table with land area (sq km) for the six counties

livestock_area_county %>%
  select(county, land_area_in_sq_km) %>%
  mutate(county = str_to_title(county)) %>%
  arrange(desc(land_area_in_sq_km)) %>%
  adorn_totals("row") %>%
  rename("County" = "county",
         "Land Area (sq. km)" = "land_area_in_sq_km") %>%
  kbl(align = "c") %>%
  kable_classic() %>% 
  row_spec(row = 0, font_size = 28, color = "white", background = "#000000") %>%
  row_spec(row = c(1:7), font_size = 20) %>%
  row_spec(row = 6, extra_css = "border-bottom: 1px solid;") %>%
  row_spec(row = 7, bold = T) #%>%
  save_kable(file = "sub_pro_1_danger_disturb/images/area_table.png",
             zoom = 5)

# Section 5: Perform and exploratory data analysis to gain key insights about the data

# a) Farming Households

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, farming), y = farming, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Farming Households",
       title = "",
       subtitle = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20), 
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

#ggsave("sub_pro_1_danger_disturb/images/county_farm_house_1.png", width = 11.25, height = 11.25, dpi = 600)

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, farming), y = farming, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Farming Households",
       title = "",
       subtitle = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.position = "top") 

#ggsave("sub_pro_1_danger_disturb/images/subcounty_farm_house_1.png", width = 11.25, height = 11.25, dpi = 600)

# b) Pastoral Livestock

#label = paste(county, comma(pasto_livestock), sep = "\n")

ggplot(livestock_area_county, aes(area = pasto_livestock, fill = county, label = comma(pasto_livestock)
                              )) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 24) +
  scale_fill_brewer(palette = "OrRd") +
  labs(x = "",
       y = "",
       title = "",
       caption = "") +
  theme_minimal() +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 28),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.position = "bottom") 

#ggsave("sub_pro_1_danger_disturb/images/county_past_live_1.png", width = 11.25, height = 11.25, dpi = 600)

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, pasto_livestock), y = pasto_livestock, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Pastoral Livestock",
       title = "",
       subtitle = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none") 

#ggsave("sub_pro_1_danger_disturb/images/county_past_live_2.png", width = 11.25, height = 11.25, dpi = 600)

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock), y = pasto_livestock, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "Subcounty",
       y = "Number of Pastoral Livestock",
       title = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.position = "top")

#ggsave("sub_pro_1_danger_disturb/images/subcounty_past_live_1.png", width = 11.25, height = 11.25, dpi = 600)

# c) Pastoral Livestock per household

livestock_area_county %>%
  ggplot() + 
  geom_col(aes(x= reorder(county, pasto_livestock_household), y = pasto_livestock_household, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "County",
       y = "Number of Pastoral Livestock per household",
       title = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.position = "none")

#ggsave("sub_pro_1_danger_disturb/images/county_past_live_house_1.png", width = 11.25, height = 11.25, dpi = 600)

livestock_area_subcounty %>%
  ggplot() + 
  geom_col(aes(x= reorder(sub_county, pasto_livestock_household), y = pasto_livestock_household, fill = county)) + 
  scale_fill_brewer(palette = "OrRd") +
  coord_flip() + 
  labs(x = "Subcounty",
       y = "Number of Pastoral Livestock per household",
       title = "",
       caption = "") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title.x =element_text(size = 20),
        axis.title.y =element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(family="Helvetica", face="bold", size = 20),
        plot.subtitle = element_text(family="Helvetica", face="bold", size = 15),
        plot.caption = element_text(family = "Helvetica",size = 12, face = "bold"),
        plot.background = element_rect(fill = "azure2", colour = "azure2"),
        panel.background = element_rect(fill = "azure2", colour = "azure2"),
        legend.title = element_blank(),
        legend.text=element_text(size=12),
        legend.position = "top")

#ggsave("sub_pro_1_danger_disturb/images/subcounty_past_live_house_1.png", width = 11.25, height = 11.25, dpi = 600)