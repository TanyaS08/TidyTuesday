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
library(ggtext)
library(ggrepel)
library(ggridges)
library(cowplot)

#todays prompts
options(prompt = "\U1F427",
        continue = "\U1F4D0")

### >> d) Data import ----

tuesdata <- tidytuesdayR::tt_load(2020, week = 31)

penguins <- tuesdata$penguins %>%
  na.omit() %>% 
  mutate(species = if_else(species == "Adelie", "Adélie", species))

# Get geospatial data for Antarctica only
antarctica <- map_data("world", region = "Antarctica")

# Island locations
penguin_location <-
  tibble(
    island = c("Dream", "Biscoe", "Torgersen"),
    lat_y = c(-64.7333, -65.4333, -64.7666636),
    long_x = c(-64.2333, -65.5000, -64.083333)
  ) 



url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/man/figures/lter_penguins.png"
img <- png::readPNG(RCurl::getURLContent(url))
cute_penguins <- grid::rasterGrob(img, interpolate = T)

pal <- c("#FF8C00", "#A034F0", "#159090")

### 1) Plotting ----
### >> a) Chinstrap - bill depth:bill length ---- 

chinstrap <- penguins %>%
  filter(species == "Chinstrap")

plot_chin <- ggplot(chinstrap) +
  geom_point(aes(x = bill_length_mm,
                 y = bill_depth_mm,
                 colour = sex),
             show.legend = FALSE) +
  stat_smooth(aes(x = bill_length_mm,
                  y = bill_depth_mm,
                  colour = sex),
              method = "lm",
              alpha = 0.2,
              show.legend = FALSE) +
  scale_colour_manual(values = c("#C07EF2","#A034F0"))  +
  theme_void() +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)") +
  theme(plot.background = element_rect(fill="#fffff003", color = "#fffff003"),
        panel.background = element_rect(fill="#fffff003", color = "#fffff003"),
        legend.box.background = element_rect(fill="#fffff003", color = "#fffff003"),
        legend.background = element_rect(fill="#fffff003", color = "#fffff003"),
        legend.title = element_blank(),
        strip.background = element_rect(fill="#fffff003", color = "#fffff003"),
        legend.position = c(0.1, 0.85))


### >> b) Gentoo - boxplots ---- 

gentoo <- penguins %>%
  filter(species == "Gentoo",
         year == 2007) %>%
  select(-c(species,island,year, body_mass_g)) %>%
  pivot_longer(cols = -sex) %>%
  group_by(name) %>%
  mutate(sex = as.factor(sex),
         value = scale(value)) 

gentoo <- ggplot(data = gentoo) +
  geom_violin(aes(x = name,
                  y = value,
                  fill = sex,
                  colour = sex), 
              position = position_dodge(0.8),
              show.legend = FALSE,
              alpha = 0.3,
              scale = "area") +
  geom_dotplot(aes(x = name,
                   y = value,
                   fill = sex),
               binaxis = "y", 
               stackdir = "center",
               stackratio = 0.9, 
               position = position_dodge(0.8),
               alpha = 0.5,
               colour = "#808078",
               dotsize = 0.7,
               show.legend = FALSE) +
  scale_fill_manual(values = c("#159090","#0C4F4F")) +
  scale_colour_manual(values = c("#15909005","#0C4F4F05")) +
  labs(x = "",
       y = "Value (scaled)") +
  theme_void()
  theme(plot.background = element_rect(fill="#fffff0", color = "#fffff0"),
        panel.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.box.background = element_rect(fill="#fffff0", color = "#fffff0"),
        panel.grid.major.y = element_line(colour = "#808078"),
        legend.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.85))

### >> c) All - ridgeplots ---- 

ridge <- ggplot(data = penguins %>%
         select(-c(year,island)) %>%
         pivot_longer(cols = -c(sex, species)) %>%
         mutate(species_sex = paste(species, "_", sex))) +
  stat_density_ridges(aes(x = value,
                          y = species,
                          fill = species_sex,
                          colour = species_sex),
                      alpha = 0.7,
                      show.legend = FALSE) +
  scale_fill_manual(values = c("#FFAF4D","#FF8C00", 
                               "#C07EF2","#A034F0", 
                               "#159090","#0C4F4F")) +
  scale_colour_manual(values = c("#FFAF4D03","#FF8C0003", 
                                 "#C07EF203","#A034F003", 
                                 "#15909003","#0C4F4F03")) +
  facet_wrap(vars(name),
             scales = "free_x",
             labeller = as_labeller(c("bill_depth_mm" = "Bill depth (mm)",
                                      "bill_length_mm" = "Bill length (mm)",
                                      "body_mass_g" = "Body mass (g)",
                                      "flipper_length_mm" = "Flipper length (mm)")))  +
  labs(x = "",
       y = "") +
  theme_void() +
  theme(plot.background = element_rect(fill="#fffff0", color = "#fffff0"),
        panel.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.box.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.title = element_blank(),
        strip.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.position = c(0.1, 0.85))

### >> a) Main Map ---- 
p <- ggplot(antarctica, 
            aes(long, lat, group = group)) +
  geom_polygon(fill = "#506B8E02", 
               alpha = .8) +
  coord_map("ortho", 
            orientation = c(-90, 0, 0),
            xlim = c(-62, -55),
            ylim = c(-75, - 60)) +
  scale_color_manual(values = c("#53868B", "#c02728", "#1874CD")) +
  labs(title = "Move over Iris dataset",
       subtitle = "a new dataset - the Palmer Penguins by Gorman, Williams and Fraser (2014)\n that can be used as an alternative to making pretty visuals the iris dataset",
       caption = "Source: Gorman, Williams & Fraser (2014) DOI: 10.1371/journal.pone.0090081|Visualization: @TanyaS_08|Illustrations: Allison Horst") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle =  element_text(hjust = 0.5),
        plot.background = element_rect(fill="#fffff0", color = "#fffff0"),
        plot.margin = unit(c(0,4,0,3), "cm")) 

inset <- ggplot(antarctica, aes(long, lat, group = group)) +
  geom_polygon(fill = "#506B8E") +
  # This is where the magic happens
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  annotate("rect", 
           color = "#808078", 
           fill = "transparent",
           xmin = -68, xmax = -54,
           ymin = -75, ymax = -60) +
  theme_map()

ggdraw(p) +
  draw_plot(inset, .6, .0, .45, .45) +
  draw_plot(plot_chin, .0, .55, .4, .3) +
  draw_plot(ridge, .44, .45, .5, .4) +
  draw_plot(gentoo, .0, .0, .4, .3) +
  draw_image("https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/man/figures/lter_penguins.png", 
             0.45, 0.1, .3, .3)



