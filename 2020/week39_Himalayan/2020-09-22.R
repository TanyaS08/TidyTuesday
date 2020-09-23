#' ------------------------------------------------------------------#
#' HIMALAYAN CLIMBING EXPEDITIONS
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-09-22)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)
library(ggforce)
library(ggridges)
library(ggtext)
library(broom)
library(here)
library(showtext)
library(cowplot)
library(stringr)
library(plyr)

#todays prompts
options(prompt = c("\U1F3D4"),
        continue = "\U1F9D7")

### >> b) Data ----
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')


annapurna_massive <- 
    expeditions %>%
    #remove unclimbed peaks ane erroneous dates
    filter(peak_name %in% c("Annapurna I", "Annapurna II", "Annapurna III", "Annapurna IV", "Annapurna South", 
                            "Gangapurna", "Tilicho"))  %>% 
    mutate(termination_reason = case_when(termination_reason == "Success (main peak)" ~ "Success",
                                            termination_reason == "Success (subpeak)" ~ "Success",
                                            termination_reason == "Success (claimed)" ~ "Success",
                                            termination_reason == "Bad weather (storms, high winds)" ~ "Bad conditions",
                                            termination_reason == "Bad conditions (deep snow, avalanching, falling ice, or rock)" ~ "Bad conditions",
                                            termination_reason == "Did not attempt climb" ~ "Other",
                                            termination_reason == "Did not reach base camp" ~ "Other",
                                            termination_reason == "Lack of time" ~ "Other",
                                            termination_reason == "Unknown" ~ "Other",
                                            TRUE ~ as.character(termination_reason)))

# Arranging values in a simplified (and aesthetically necessary) order
termination_reason_values <- annapurna_massive %>% pull(termination_reason) %>% unique()
termination_reason_values <- 
  termination_reason_values[c(3, 2, 1, 6, 4, 5)]

summary(as.factor(annapurna_massive$termination_reason))

levels(annapurna_massive$termination_reason) <- termination_reason_values[c(3, 2, 1, 6, 4, 5)]

year = annapurna_massive$year/10
round_any(year,0.5)

exp <- 
annapurna_massive %>%
  select(expedition_id, peak_id, peak_name, year, termination_reason) %>% 
  group_by(peak_name) %>%
  mutate(year = year/10,
        decade = round_any(scale(round_any(year,0.5)),0.05),
        year_pos = case_when(peak_name == "Tilicho" ~ decade,
                            peak_name == "Annapurna I" ~ decade + 4,
                            peak_name == "Annapurna South" ~ decade + 8,
                            peak_name == "Gangapurna" ~ decade + 12,
                            peak_name == "Annapurna III" ~ decade + 16,
                            peak_name == "Annapurna IV" ~ decade + 20,
                            peak_name == "Annapurna II" ~ decade + 24))  %>% 
  ungroup()  %>% 
  arrange(peak_name, year_pos, termination_reason) %>%
  dplyr::mutate(id = row_number()) 

# Bar colors
mtn_colors <- c('#ffffffff','#aec0ceff','#7acbefff',
                '#225997ff','#003062ff','#6a8550ff','#626262ff')


ggplot(exp, 
       aes(x = year_pos,
            y = 1,
            group = id,
            fill = termination_reason,
            colour = peak_name)) +
  geom_bar(stat = 'identity',
           size = 0.2)

ggplot(exp) + 
  geom_bar(aes(x = year_pos, 
                group = id, 
                fill = termination_reason), 
            alpha = 0.9,
            color = "#101924ff",
            width = 0.3)  +
  labs(
    title = "EVEREST EXPEDITIONS",
    subtitle = "Journeys to the top of the world end for many reasons. This graphic\nlooks at the results of all Everest expeditions since 1921.\nEverest has had a colorful history, but even\nstill most expeditions reach the peak.",
    caption = "Source: The Himalayan Database  |  Visualization: @charliegallaghr"
  ) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(name = "Number of Expeditions", expand = c(0,0)) + 
  scale_fill_manual(values = mtn_colors) +
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(10, "pt"),
    legend.key.width = unit(40, "pt"),
    legend.position = c(0.24, 0.92),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#252f39ff"),
    plot.margin = margin(20, 40, 5, 40),
    plot.background = element_rect(color = NA),
    plot.title.position = "panel",
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = grey(0.4)),
    axis.text.y = element_text(color = grey(0.4)),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )

