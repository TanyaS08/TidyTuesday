#' ------------------------------------------------------------------#
#'  PENGUIN DATASET
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-28/readme.md)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
#devtools::install_github("coolbutuseless/ggblur")
library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(extrafont)
library(ggtext)
library(ggblur)
library(mapproj) # For the polar orthographic map projection
library(ggthemes) # For theme_map()

#todays prompts
options(prompt = "\U1F427",
        continue = "\U1F4D0")

### >> d) Data import ----

tuesdata <- tidytuesdayR::tt_load(2020, week = 31)

penguins <- tuesdata$penguins %>%
  na.omit()# %>%
  #mutate(Lat = case_when(island == "Biscoe" ~ -65.4333,
  #                       island == "Dream" ~ -64.73333,
  #                       island == "Torgersen" ~ -64.76666),
  #       Long = case_when(island == "Biscoe" ~ -65.5000,
  #                        island == "Dream" ~ -64.23333,
  #                        island == "Torgersen" ~ -64.0833))

# Get geospatial data for Antarctica only
antarctica <- map_data("world", region = "Antarctica")

### 1) Plotting ----
### >> a) Chinstrap - bill depth:bill length ---- 

chinstrap <- penguins %>%
  filter(species == "Chinstrap")

plot_chin <- ggplot(chinstrap) +
  geom_point(aes(x = bill_length_mm,
                 y = bill_depth_mm,
                 colour = sex)) +
  stat_smooth(aes(x = bill_length_mm,
                    y = bill_depth_mm,
                    colour = sex),
              method = "lm",
              alpha = 0.2)


### >> a) Chinstrap - bill depth:bill length ---- 
ggplot(antarctica, aes(long, lat, group = group)) +
  geom_polygon(fill = "#506B8E") +
  # This is where the magic happens
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  theme_map()

