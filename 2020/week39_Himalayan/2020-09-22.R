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
library(extrafont)
library(here)
library(showtext)
library(plyr)

#todays prompts
options(prompt = c("\U1F3D4"),
        continue = "\U1F9D7")

### >> b) Data ----
members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')


annapurna_massif <- 
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
termination_reason_values <- annapurna_massif %>% pull(termination_reason) %>% unique()
termination_reason_values <- 
  termination_reason_values[c(3, 2, 1, 6, 4, 5)]

exp <- 
annapurna_massif %>%
  select(expedition_id, peak_id, peak_name, year, termination_reason) %>% 
  group_by(peak_name) %>%
  mutate(reason = factor(termination_reason, 
                    levels = termination_reason_values,
                    labels = termination_reason_values),
        year = year/10,
        decade = round_any(scale(round_any(year,0.5)),0.05),
        year_pos = case_when(peak_name == "Tilicho" ~ decade - 1,
                            peak_name == "Annapurna I" ~ decade + 4.5,
                            peak_name == "Annapurna South" ~ decade + 9,
                            peak_name == "Gangapurna" ~ decade + 13.5,
                            peak_name == "Annapurna III" ~ decade + 18,
                            peak_name == "Annapurna IV" ~ decade + 24,
                            peak_name == "Annapurna II" ~ decade + 30))  %>% 
  ungroup()  %>% 
  arrange(year_pos, reason) %>%
  dplyr::mutate(id = row_number()) 

peak_names <- 
    exp  %>% 
    group_by(peak_name)  %>% 
    summarise_at(vars(year_pos), mean) %>%
    left_join(.,
              exp  %>% 
                group_by(peak_name, year_pos)  %>% 
                tally()  %>% 
                group_by(peak_name) %>%
                summarise_at(vars(n), max),
            by = "peak_name")  %>% 
    mutate(peak_name = case_when(peak_name == "Tilicho" ~ "Tilicho<br/>7 134m",
                            peak_name == "Annapurna I" ~ "Annapurna I<br/>8 091m",
                            peak_name == "Annapurna South" ~ "Annapurna South<br/>7 219m",
                            peak_name == "Gangapurna" ~ "Gangapurna<br/>7 455m",
                            peak_name == "Annapurna III" ~ "Annapurna III<br/>7 555m",
                            peak_name == "Annapurna IV" ~ "Annapurna IV<br/>7 525m",
                            peak_name == "Annapurna II" ~ "Annapurna II<br/>7 937m"))

# Bar colors
mtn_colors <- c('#ffffffff','#aec0ceff','#7acbefff',
                '#225997ff','#6a8550ff','#003062ff','#626262ff')


plot<-
ggplot(exp) + 
  geom_bar(aes(x = year_pos, 
                group = id, 
                fill = termination_reason), 
            alpha = 0.9,
            color = "#101924ff",
            width = 0.3)  +
  labs(
    title = "ANNAPURNA MASSIF",
    subtitle = "The Annapurna massif is home to several world-class treks and has 30 peaks over 6000m in altitude.\nAnnapurna I is the only peak to reach above 8000m but is also the first of the 8000ers to be climbed\nBelow are 7 of the more prominent peaks of the massive\narranged as if one was looking out over the massif from the south\nEach box represents one expedition and the colour indicates the outcome",
    caption = "Source: The Himalayan Database  |  Visualization: @TanyaS_08"
  ) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(name = "Number of Expeditions", expand = c(0,0)) + 
  scale_fill_manual(values = mtn_colors) +
  theme_bw() + 
  theme(
    text = element_text(family = "Courier New", 
                        color = grey(0.9)),
    plot.caption = element_text(family = "Courier New", 
                        size = 8, color = grey(0.4)),
    title = element_text(family = "Courier New", size = 30),
    axis.title.y = element_text(family = "Courier New", size = 10,
                                color = grey(0.9)),
    plot.subtitle = element_text(family = "Courier New", 
                                 size = 12, margin = margin(0,0,20,0)),
    legend.title = element_blank(),
    legend.text = element_text(family = "Courier New", size = 11,
                                color = grey(0.9)),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(10, "pt"),
    legend.key.width = unit(40, "pt"),
    legend.position = c(0.66, 0.87),
    legend.background = element_rect(fill = NA),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#101924ff"),
    panel.grid.major.y = element_line(color = "#252f39ff"),
    plot.margin = margin(20, 40, 5, 40),
    plot.background = element_rect(fill = "#101924ff"),
    plot.title.position = "panel",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = grey(0.4)),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  geom_richtext(data = peak_names,
        aes(y= n + 5,
            x = year_pos,
            label = peak_name),
            angle = 0,
            vjust = 1,
            family = "Courier New") +
  annotate(
        geom = "text",
        x = 1,
        y = 46,
        label = 'Annapurna I has the highest\nfatility ratio of the 8000ers\nthis is in part due to the steep south face\nwhich consists of a 3000m wall of rock',
        family = "Courier New",
        size = 2.5,
        colour = grey(0.9))

ggsave("2020/week39_Himalayan/AnnapurnaMassif.png", 
       plot, 
       height = 8, width = 13, 
       units = "in", dpi = 600)
