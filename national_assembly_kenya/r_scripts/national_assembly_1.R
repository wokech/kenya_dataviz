## Kenyan parliament composition

# 1) Libraries and packages

library(tidyverse)
library(tidyr)
library(janitor)
library(tidyr)
library(readxl)
library(scales)
library(devtools)
#devtools::install_github('bbc/bbplot')
library(bbplot)
#install.packages("wordcloud")
library(wordcloud)
# install.packages("ggwordcloud")
library(ggwordcloud)
# install.packages("treemapify")
library(treemapify)
library(treemapify)
# install.packages("ggrepel")
library(ggrepel)
library(janitor)

#install.packages("ggpol")
library(ggpol)

#install.packages("devtools")
#devtools::install_github("zmeers/ggparliament")
library(ggparliament)

# To export the images
# camcorder::gg_record()

# library(camcorder)
# 
# gg_record(
#   dir = 'sub_pro_4_national_assembly/images',
#   width = 12,
#   height = 12 * 9 / 16,
#   dpi = 300,
#   bg = 'white'
# )

# 2) Load the required datasets

parl_mps <- read_excel("sub_pro_4_national_assembly/processed_tables/Parliament_Sep_2022.xlsx")
parl_composition_condensed <- read_excel("sub_pro_4_national_assembly/processed_tables/Party_Membership_Condensed_Sep_2022.xlsx")
parl_composition <- read_excel("sub_pro_4_national_assembly/processed_tables/Party_Membership_Sep_2022.xlsx")

# 3) Clean data filter out the relevant datasets

parl_composition <- parl_composition %>%
  clean_names()
parl_mps <- parl_mps %>%
  clean_names()
parl_composition_condensed <- parl_composition_condensed %>%
  clean_names()

# Filter out the relevant datasets

parl_mps_regular <- parl_mps  %>%
  filter(!str_detect(constituency, "CWR|Nominated"))

parl_mps_nom <- parl_mps %>%
  filter(constituency == "Nominated")

parl_mps_women_rep <- parl_mps  %>%
  filter(str_detect(constituency, "CWR"))

## Will need to install either ggpol or ggparliament

ke_semicircle <- parliament_data(election_data = parl_composition_condensed,
  type = "semicircle",
  parl_rows = 10,
  party_seats = parl_composition_condensed$total_membership_national_assembly) 

ggplot(ke_semicircle, aes(x=x, y=y, colour = abbreviation)) +
  geom_parliament_seats() + 
  draw_totalseats(n = 349, type = "semicircle", size = 32) +
  theme_ggparliament() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size =18, hjust = 0.5),
        plot.caption = element_text(size = 16, hjust = 0.9),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        plot.background = element_rect(fill = "azure2", color = "azure2"),
        panel.background = element_rect(fill = "azure2", color = "azure2")) +
  labs(title = "The 13th Parliament of the Republic of Kenya",
       subtitle = "Composition of MPs elected in August 2022",
       color = "Party",
       caption = "* Small parties have 5 or less MPs\nSource: National Assembly of Kenya | By: @afro_dataviz") +
  scale_colour_manual(values = ke_semicircle$colors,
                      limits = ke_semicircle$abbreviation)

#### Write a note to include the parties with 5 or less MPs and their numbers  
