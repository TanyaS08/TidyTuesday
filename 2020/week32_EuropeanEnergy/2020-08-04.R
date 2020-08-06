#' ------------------------------------------------------------------#
#'  European Energy
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-08-04/readme.md)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(treemapify)
library(geofacet) #Using `europe_countries_grid2`
library(sysfonts)
library(showtext)
library(grid)
library(gridExtra)

font_add_google("Bebas Neue",
                "Bebas")
font_add_google("Montserrat",
                "Montserrat")


font_paths()  
font_files()
font_families()

#for rendering fonts
trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)


#todays prompts
options(prompt = "\U1F343",
        continue = "\U1F50C")

### >> d) Data import ----

tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types %>%
  #Rename NA to UK
  mutate(country_name = case_when(is.na(country_name) ~ "UK",
                                  country_name == "Bosnia & Herzegovina" ~ "Bosnia & H.",
                                  country_name == "North Macedonia" ~ "N. Macedonia",
                                  TRUE ~ country_name),
         #lump hydro for cleaner plotting
         type = case_when(type == "Pumped hydro power" ~ "Hydro",
                          type == "Conventional thermal" ~ "Conventional",
                          TRUE ~ type)) %>%
  group_by(country_name, type) %>%
  #calcualte new totals
  summarise_at(vars(c(`2016`, `2017`, `2018`)),
               sum)

energy_grid <-
  energy_types %>%
  distinct(country_name) %>%
  inner_join(.,
            europe_countries_grid2,
            by = c("country_name" = "name")) %>%
  rename("name" = "country_name") %>% 
  ungroup() %>%
  add_row(row = 5, col = 10, code = "GE", name = "Georgia")

### 0) Plotting ----

#TreeMaps

p2016 <- ggplot(energy_types, 
       aes(fill = type, 
           area = `2016`,
           label = type)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(colour = "white", 
                    place = "centre",
                    grow = TRUE,
                    family = "Bebas") +
  facet_geo(~ country_name, 
            grid = energy_grid, 
            scales = "free_y") +
  scale_fill_manual(values = c("#820707",
                               "#7EE67E",
                               "#35D45A",
                               "#2d2b29",
                               "#25943F",
                               "#35613F",
                               "#507157")) +
  labs(title = "2016") +
  theme(plot.background = element_rect(fill="#F4F9F5", color = NA),
        panel.background = element_rect(fill="#F4F9F5", color = NA),
        strip.background = element_rect(fill="#F4F9F5", 
                                        color = "#F4F9F5"),
        text = element_text(family = "Montserrat",
                            size = 5),
        plot.title = element_text(family = "Bebas",
                                  size = 16))

p2017 <- ggplot(energy_types, 
                aes(fill = type, 
                    area = `2017`,
                    label = type)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(colour = "white", 
                    place = "centre",
                    grow = TRUE,
                    family = "Bebas") +
  facet_geo(~ country_name, 
            grid = energy_grid, 
            scales = "free_y") +
  scale_fill_manual(values = c("#820707",
                               "#7EE67E",
                               "#35D45A",
                               "#2d2b29",
                               "#25943F",
                               "#35613F",
                               "#507157")) +
  labs(title = "2017") +
  theme(plot.background = element_rect(fill="#F4F9F5", color = NA),
        panel.background = element_rect(fill="#F4F9F5", color = NA),
        strip.background = element_rect(fill="#F4F9F5", 
                                        color = "#F4F9F5"),
        text = element_text(family = "Montserrat",
                            size = 5),
        plot.title = element_text(family = "Bebas",
                                  size = 16))

p2018 <- ggplot(energy_types, 
                aes(fill = type, 
                    area = `2018`,
                    label = type)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(colour = "white", 
                    place = "centre",
                    grow = TRUE,
                    family = "Bebas") +
  facet_geo(~ country_name, 
            grid = energy_grid, 
            scales = "free_y") +
  scale_fill_manual(values = c("#820707",
                               "#7EE67E",
                               "#35D45A",
                               "#2d2b29",
                               "#25943F",
                               "#35613F",
                               "#507157")) +
  labs(title = "2018") +
  theme(plot.background = element_rect(fill="#F4F9F5", color = NA),
        panel.background = element_rect(fill="#F4F9F5", color = NA),
        strip.background = element_rect(fill="#F4F9F5", 
                                        color = "#F4F9F5"),
        strip.text = element_text(family = "Montserrat",
                                  size = 5),
        plot.title = element_text(family = "Bebas",
                                  size = 16))

#inset <-
  ggplot(data = tibble(x = seq(from = 0,
                               to = 1,
                               by = 0.1),
                       y = seq(from = 0,
                               to = 1,
                               by = 0.1)),
         aes(x = x,
             y = x)) +
  geom_point(
    colour = "#fffff0"
  ) +
  labs(title = "EUROPEAN ENERGY",
       subtitle = "Is Europe going Green?") +
  geom_textbox(aes(
    label = "Here we have the proportion of different enrgy types 
    that make up the total energy production (in GWh) for 37 Europenan countries.
    ",
    x = 0,
    y = 1,
    vjust = 1,
    hjust = 0),
    fill = "#F4F9F5",
    box.colour = "#F4F9F5",
    size = 3.5,
    width = 0.8,
    family = "Montserrat") +
  theme(plot.title =  element_text(family = "Bebas",
                                   vjust = 1,
                                   size = 42),
        plot.background = element_rect(fill="#F4F9F5", color = NA),
        panel.background = element_rect(fill="#F4F9F5", color = NA),
        plot.subtitle = element_markdown(hjust = 0,
                                         size = 17,
                                         family = "Montserrat"),
        plot.caption = element_text(family = "Montserrat",
                                    hjust = 0),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


plot_grid(inset,
          p2016,
          p2017,
          p2018)

# End of script ----
