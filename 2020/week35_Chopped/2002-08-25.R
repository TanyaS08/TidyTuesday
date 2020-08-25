#' ------------------------------------------------------------------#
#'  CHOPPED
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-08-25)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)
library(ggforce)
library(patchwork)
library(ggtext)
library(broom)
library(here)
library(showtext)

#Font Choices
font_add_google("Roboto Mono", "Roboto Mono")

#todays prompts
options(prompt = c("\U1F52A"),
        continue = "\U1F378")


### >> b) Import data and clean ----
chopped_raw <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')


### 1) Data Prep ----
chopped_avg <-
  chopped_raw %>% 
  group_by(season) %>% 
  na.omit(episode_rating)  %>% 
  arrange(series_episode) %>% 
  mutate(episode_id = row_number())  %>% 
  mutate(
    avg = mean(episode_rating),
    episode_mod = series_episode + (45 * season),
    mid = mean(episode_mod)
  ) %>% 
  ungroup() %>% 
  mutate(season = factor(season))

chopped_lines <-
  chopped_avg %>% 
  group_by(season) %>% 
  summarize(
    start_x = min(episode_mod) - 5,
    end_x = max(episode_mod) + 5,
    y = unique(avg)
  ) %>% 
  pivot_longer(
    cols = c(start_x, end_x),
    names_to = "type",
    values_to = "x"
  ) %>% 
  mutate(
    x_group = if_else(type == "start_x", x + .1, x - .1),
    x_group = if_else(type == "start_x" & x == min(x), x_group - .1, x_group),
    x_group = if_else(type == "end_x" & x == max(x), x_group + .1, x_group)
  )

### 1) Plotting ----
chopped_avg %>% 
  ggplot(aes(episode_mod, episode_rating)) +
    geom_hline(data = tibble(y = 7:10),
               aes(yintercept = y),
               color = "grey82",
               size = .5) +
    geom_segment(aes(xend = episode_mod,
                     yend = avg, 
                     color = season)) +
    geom_line(data = chopped_lines,
              aes(x, y),
              color = "grey40") +
    geom_line(data = chopped_lines,
              aes(x_group, y, 
                  color = season),
              size = 2.5) +
    geom_point(aes(color = season)) +
    geom_label(aes(mid, 10.12,
                   label = glue::glue(" Season {season} "),
                   color = season),
               fill = NA,
               fontface = "bold",
               label.padding = unit(.2, "lines"),
               label.r = unit(.25, "lines"),
               label.size = .5) +
    scale_x_continuous(expand = c(.015, .015)) +
    scale_y_continuous(expand = c(.03, .03),
                       limits = c(6.5, 10.2),
                       breaks = seq(6.5, 10, by = .5),
                       sec.axis = dup_axis(name = NULL)) +
    scale_color_manual(values = c("#486090", "#D7BFA6", "#6078A8", "#9CCCCC", 
                                  "#7890A8", "#C7B0C1", "#B5C9C9", "#90A8C0", "#A8A890"),
                       guide = F) +
    scale_size_binned(name = "Votes per Episode",
                      range = c(.3, 3)) +
    labs(x = NULL, y = "IMDb Rating",
        caption = "Visualization by Cédric Scherer  •  Data by IMDb via data.world  •  Fanart Logo by ArieS") +
    guides(size = guide_bins(show.limits = T,
                             direction = "horizontal",
                             title.position = "top",
                             title.hjust = .5)) +
    theme(legend.position = c(.5, .085), 
          legend.key.width = unit(2, "lines"))

# End of script ----

