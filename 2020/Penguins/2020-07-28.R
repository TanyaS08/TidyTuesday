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
library(mdthemes)
library(gt)
library(sysfonts)
library(showtext)

font_add_google("Fredericka the Great",
                "Fredericka")
font_add_google("Josefin Slab",
                "Josefin")

font_paths()  
font_files()
font_families()

trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)


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

#Colour palette (Adelie, Chinstrap, Gentoo)
pal <- c("#FF8C00", "#A034F0", "#159090")

#data dictionary

data_dictionary <- 
  tribble(
    ~Variable, ~Description,
    "species", "a factor denoting penguin species (Adélie, Chinstrap and Gentoo)",
    "island", "a factor denoting island in Palmer Archipelago, Antarctica (Biscoe, Dream or Torgersen)",
    "bill_length_mm", "a number denoting bill length (millimeters)",
    "bill_depth_mm", "a number denoting bill depth (millimeters)",
    "flipper_length_mm", "an integer denoting flipper length (millimeters)",
    "body_mass_g", "an integer denoting body mass (grams)",
    "sex", "a factor denoting penguin sex (female, male)"
  )

data_table <-
  gt(data_dictionary)%>%
  tab_style(
    style = cell_fill(color = "#fffff0"),
    locations = cells_body(
      columns = vars(Variable, Description))) %>%
  tab_header(title = "{penguins} data dictionary"
  ) %>%
  tab_style(
    style = cell_text(font = "Josefin"),
    locations = cells_body(
      columns = vars(Variable, Description)))

### 1) Plotting ----
### >> a) Chinstrap - bill depth:bill length ---- 

chinstrap <- penguins %>%
  filter(species == "Chinstrap")

cinstarp_summ <- chinstrap %>%
  select(sex, bill_length_mm, bill_depth_mm) %>%
  group_by(sex) %>%
  summarise_all(list(median = median,
                     sd = sd))


plot_chin <- 
  ggplot(chinstrap) +
  geom_errorbar(
    data = cinstarp_summ,
    aes(x = bill_length_mm_median,
        ymin = bill_depth_mm_median - bill_depth_mm_sd,
        ymax = bill_depth_mm_median + bill_depth_mm_sd,
        color = sex),
    width = .8,
    size = .8,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = cinstarp_summ,
    aes(y = bill_depth_mm_median,
        xmin = bill_length_mm_median - bill_length_mm_sd,
        xmax = bill_length_mm_median + bill_length_mm_sd,
        color = sex),
    width = .3,
    size = .8,
    show.legend = FALSE
  ) +
  geom_point(aes(x = bill_length_mm,
                 y = bill_depth_mm,
                 fill = sex),
             shape = 21,
             color = "transparent",
             alpha = .7,
             show.legend = FALSE) +
  scale_colour_manual(values = c("#C07EF2","#A034F0")) +
  scale_fill_manual(values = c("#C07EF2","#A034F0")) +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)",
       subtitle = "Regression analyses between continuous variables are still an option<br/> here we can  look at the ratio of bill length to depth <br/> for  <b style='color:#C07EF2'>female</b> and <b style='color:#A034F0'>male</b> Chinstrap penguins<br/>") +
  theme(plot.background = element_rect(fill="transparent", color = NA),
        panel.background = element_rect(fill="transparent", color = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.subtitle = element_markdown(hjust = 0,
                                         size = 9),
        panel.grid = element_blank(),
        text = element_text(family = "Josefin"))


### >> b) Gentoo - boxplots ---- 

adelie <- penguins %>%
  filter(species == "Adélie") %>%
  select(c(island, body_mass_g, sex)) %>%
  pivot_longer(cols = -c(sex, island)) %>%
  mutate(sex = as.factor(sex),
         island = as.factor(island)) 

adelie_plot <- 
  ggplot(data = adelie) +
  geom_violin(aes(x = island,
                  y = value,
                  fill = sex,
                  colour = sex), 
              position = position_dodge(0.8),
              show.legend = FALSE,
              alpha = 0.6,
              scale = "area") +
  geom_dotplot(aes(x = island,
                   y = value,
                   fill = sex),
               binaxis = "y", 
               stackdir = "center",
               stackratio = 0.9, 
               position = position_dodge(0.8),
               alpha = 0.5,
               color = "transparent",
               dotsize = 0.7,
               show.legend = FALSE) +
  scale_fill_manual(values = c("#FFAF4D","#FF8C00")) +
  scale_colour_manual(values = c("#FFAF4D05","#FF8C0005")) +
  labs(x = "",
       y = "Body mass (g)",
       subtitle = "Multiple categorical data present different grouping options <br/> here we can  look at body mass between  <b style='color:#FFAF4D'>female</b> and <b style='color:#FF8C00'>male</b> Adélie penguins<br/> on different islands") +
  theme(plot.background = element_rect(fill="transparent", color = NA),
        panel.background = element_rect(fill="transparent", color = NA),
        legend.box.background = element_rect(fill="#fffff0", color = "#fffff0"),
        panel.grid = element_blank(),
        legend.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.85),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.subtitle = element_markdown(hjust = 0,
                                         size = 9),
        text = element_text(family = "Josefin")) +
  scale_x_discrete(position = "top")

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
       y = "",
       caption = "With four continuous variables to choose from there are many ways to 'torture' this dataset<br/> here we can see a density distribution of the body measurements for<br/> <b style='color:#FF8C00'>Adélie</b>, <b style='color:#A034F0'>Chinstrap</b> and <b style='color:#0C4F4F'>Gentoo</b> penguins (females are the lighter shade)") +
  theme(plot.background = element_rect(fill="transparent", color = NA),
        panel.background = element_rect(fill="#fffff0", color = NA),
        legend.box.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.title = element_blank(),
        strip.background = element_rect(fill="#fffff0", color = "#fffff0"),
        legend.position = c(0.1, 0.85),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Josefin"),
        plot.caption = element_markdown(hjust = 1,
                                        size = 9))

### >> a) Main Map ---- 
antarctica <- 
  ggplot(antarctica, 
            aes(long, lat, group = group)) +
  geom_polygon(fill = "#506B8E", 
               alpha = .8) +
  coord_map("ortho", 
            orientation = c(-90, 0, 0),
            xlim = c(-62, -55),
            ylim = c(-75, - 60))  +
  theme_void()

main <-
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
  labs(title = "THERE'S A NEW DATASET IN TOWN",
       subtitle = "Presenting the {penguins} dataset.<br/> An alternative to {iris} that can be used to demonstrate many data science concepts like:<br/>correlation, regression, classification.<br/>With cute penguins and without a problematic past!",
       caption = "Source: Gorman, Williams & Fraser (2014) DOI: 10.1371/journal.pone.0090081| Visualization: @TanyaS_08| Illustrations: Allison Horst") +
  geom_textbox(aes(
    label = "**DATA DICTIONARY**
    <br/>**species** - Penguin species (Adelie, Gentoo, Chinstrap)<br/>
    **island** - Island where recorded (Biscoe, Dream, Torgersen)<br/>
    **bill_length_mm** - Bill length in millimeters<br/>
    **bill_depth_mm** - Bill depth in millimeters<br/>
    **flipper_length_mm** - Flipper length in mm<br/>
    **body_mass_g** -Body mass in grams<br/>
    **sex** - sex of the animal<br/>
    **year** - year recorded",
    x = 0.57,
    y = 1,
    vjust = 1),
    fill = "#fffff0",
    box.colour = "#fffff0",
    size = 3.5,
    width = 0.3) +
  theme(plot.title =  element_text(family = "Fredericka",
                                   vjust = 1,
                                   size = 42),
        plot.background = element_rect(fill="#fffff0", 
                                       color = "#fffff0"),
        panel.background = element_rect(fill="#fffff0", 
                                        color = NA),
        plot.subtitle = element_markdown(hjust = 0,
                                         size = 17,
                                         family = "Josefin"),
        plot.caption = element_text(family = "Josefin",
                                    hjust = 0),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#plot <- 
  ggdraw(main) +
  draw_plot(antarctica, .02, .0, 1, .8) +
  draw_plot(plot_chin, .01, .45, .43, .35) +
  draw_plot(ridge, .53, .19, .45, .35) +
  draw_plot(adelie_plot, .01, .0, .4, .37) +
  draw_image("https://raw.githubusercontent.com/allisonhorst/palmerpenguins/master/man/figures/lter_penguins.png", 
             0.67, 0.65, .3, .3)


ggsave("2020/Penguins/NewDatasetInTown.png", 
       plot, 
       height = 10.8, width = 17, 
       units = "in", dpi = 600)


# End of script ----
