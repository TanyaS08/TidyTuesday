#' ------------------------------------------------------------------#
#'  AVATAR
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-08-18)
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

#todays prompts
options(prompt = "\U1F33E",
        continue = "\U1F331")


### >> b) Import data and clean ----

#Get data for the week
plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv') %>%
    mutate_all(as.factor)
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

a <- threats %>% count(year_last_seen, continent, red_list_category) %>% 
  drop_na(year_last_seen,red_list_category,continent) %>% 
  filter(!year_last_seen == "Before 1900") %>% 
  mutate(red_list_num=case_when(red_list_category == "Extinct" ~ 1,
                                red_list_category == "Extinct in the Wild" ~ 0)) %>% 
  group_by(red_list_category) %>% 
  mutate(percentage=((n*red_list_num)/sum(n)*100)) %>% 
  mutate(year_last_seen = fct_rev(year_last_seen)) %>% 
  mutate(continent = fct_reorder(continent, percentage, .desc = TRUE))

sa_plants <- plants %>%
  filter(country == "South Africa")

africa_plants <- plants %>%
  filter(continent == "Africa")

ggplot(a, aes(fill = continent, alpha = n)) +
  geom_ellipse(aes(x0 = 7, y0 = 0, a = 6, b = 3, angle = 0, m1 = 2),  color = "transparent") + #right
  geom_ellipse(aes(x0 = -7, y0 = 0, a = 6, b = 3, angle = pi, m1 = 2),  color = "transparent") + #left
  geom_ellipse(aes(x0 = 0, y0 = 7, a = 6, b = 3, angle = pi / 2, m1 = 2), color = "transparent") + #top
  geom_ellipse(aes(x0 = 5, y0 = 5, a = 6, b = 3, angle = pi / 4, m1 = 2), color = "transparent") + #dia_top_right
  geom_ellipse(aes(x0 = -5, y0 = 5, a = 6, b = 3, angle = 3*pi / 4, m1 = 2),  color = "transparent") + #dia_top_left
  geom_ellipse(aes(x0 = 5, y0 = -5, a = -6, b = 3, angle = 7*pi / 4, m1 = 2),  color = "transparent") + #dia_bottom_right
  geom_ellipse(aes(x0 = -5, y0 = -5, a = -6, b = 3, angle = 5*pi / 4, m1 = 2), color = "transparent") + #dia_bottom_left
  geom_ellipse(aes(x0 = 0, y0 = -7, a = 6, b = 3, angle = 3*pi / 2, m1 = 2),  color = "transparent") + #bottom
  geom_circle(aes(x0 = 0, y0 = 0, r= log(percentage*10)), fill = "orange", alpha = 1, color = "darkorange") +
  coord_fixed(xlim = c(-15, 15)) +
  facet_grid(year_last_seen ~ continent, switch = "y") +
  theme_void() +
  theme(
    plot.background = element_rect(fill="ivory", color = "ivory"),
    strip.text.y.left = element_text(angle = 0, size = 14),
    strip.text.x = element_text( size = 14),
    legend.position = "none",
    plot.margin = margin(2, 0, 2, 2)
  )


p2 <- ggplot() +
  geom_ellipse(aes(x0 = 5, y0 = -17, a = 6, b = 1.5, angle = pi / 4, m1 = 2), fill="#919c4c", alpha = .7, color = "transparent") + #dia_top_right
  geom_ellipse(aes(x0 = -4.5, y0 = -17, a = 6, b = 1.5, angle = 3*pi / 4, m1 = 2), fill="#919c4c", alpha = .7, color = "transparent") +
  geom_bspline_closed(aes(x = c(0, -2, 2, 0), y = c(0,-10, -50, 5) ), color="#919c4c", alpha = .9) +
  geom_ellipse(aes(x0 = 7, y0 = 0, a = 6, b = 3, angle = 0, m1 = 2), fill= "#f5c04a", alpha = 0.5, color = "transparent") + #right
  geom_ellipse(aes(x0 = -7, y0 = 0, a = 6, b = 3, angle = pi, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #left
  geom_ellipse(aes(x0 = 0, y0 = 7, a = 6, b = 3, angle = pi / 2, m1 = 2), fill= "#f5c04a", alpha = 0.5, color = "transparent") + #top
  geom_ellipse(aes(x0 = 5, y0 = 5, a = 6, b = 3, angle = pi / 4, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #dia_top_right
  geom_ellipse(aes(x0 = -5, y0 = 5, a = 6, b = 3, angle = 3*pi / 4, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #dia_top_left
  geom_ellipse(aes(x0 = 5, y0 = -5, a = -6, b = 3, angle = 7*pi / 4, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #dia_bottom_right
  geom_ellipse(aes(x0 = -5, y0 = -5, a = -6, b = 3, angle = 5*pi / 4, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #dia_bottom_left
  geom_ellipse(aes(x0 = 0, y0 = -7, a = 6, b = 3, angle = 3*pi / 2, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #bottom
  geom_circle(aes(x0 = 0, y0 = 0, r= 3), fill = "orange", alpha = 1, color = "transparent") +
  geom_circle(aes(x0 = 0, y0 = 0, r= 2.8), fill = "white", alpha = 0.2, color = "transparent") +
  geom_arc(aes(x0 = 0, y0 = -22, r = 6, start = -1.3, end = -5), color="blue", alpha = .0) +
  theme_void() +
  coord_fixed(xlim = c(-25, 25), ylim = c(-70,15), clip = "off") +
  geom_textbox(aes(x= -25, y = 14, label = "**Size**: Log scaled percentages of\n extinct plants"),
               size = 4,
               fill = NA,
               box.colour = NA,
               hjust = 0,
               family = "Merienda") +
  geom_textbox(aes(x= 2, y = -25, label = "**Transparance**: amount of\n plants extinct in the wild"),
               size = 4,
               fill = NA,
               box.colour = NA,
               hjust = 0,
               family = "Merienda")+
  geom_textbox(aes(x= -25, y = -50, label = "Major threats to biodiversity, especially in areas of exceptional plant diversity, 
                   primarily in the tropics, are often linked to industrial-scale activities such as timber exploitation or large plantations, 
                   mining, and agriculture.
                   In this graphic continents are sorted on percentage of most extinct plant species. Africa has had the most extinct plants while Europe has the least.
                   Still in 2000-2020 plant species are going extinct."), 
               width = unit(5, "inch"),
               color = "black",
               lineheight = 1.7,
               size = 4,
               fill = NA,
               box.colour = NA,
               hjust = 0,
               family = "Merienda") +
  geom_curve(aes(x = 0, y = 0, xend = -20, yend = 10),
             arrow = arrow(length = unit(0.07, "inch")), 
             size = 0.4,
             color = "gray50", 
             curvature = -0.3) +
  geom_curve(aes(x = 8, y = 8, xend = 20, yend = -20),
             arrow = arrow(length = unit(0.07, "inch")), 
             size = 0.4,
             color = "gray50", 
             curvature = -0.5) +
  theme(plot.background = element_rect(fill="ivory", color = "ivory"),
        plot.margin = margin(2, 2, 2, 0))


p1 + p2  + 
  plot_annotation(title = 'Extinct and Endangered Plants\n',
                  caption = "Source: IUCN Red List | Graphics: Daniel Vaisanen",
                  theme = theme(plot.title = element_text(size = 50, 
                                                          hjust = 0.5, 
                                                          family = "Playfair Display",
                                                          face = "bold"))) & 
  theme(text = element_text('Merienda'),
        plot.background = element_rect(fill="ivory", color = "ivory"),
        plot.margin = margin(20, 30, 20, 30))

ggsave(here("2020/week_34_plants_in_danger/plants.png"), dpi=300, width = 19, height = 12)





