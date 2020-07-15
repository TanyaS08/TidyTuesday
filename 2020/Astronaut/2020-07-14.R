#' ------------------------------------------------------------------#
#'  ASTRONAUT DATABASE
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-14/readme.md)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(lubridate)
library(ggwaffle)
library(extrafont)
library(ggtext)

#todays prompts
options(prompt = "\U1F680",
        continue = "\U1F31F")

### >> d) Data import ----
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

astronauts <- tuesdata$astronauts

#transform data for waffle plot
astro_waff <- astronauts %>%
  #select only US and Russia
  filter(nationality %in% c("U.K./U.S.", "U.S.", "U.S.S.R/Russia", "U.S.S.R/Ukraine")) %>%
  #combine UK and Ukranke 'hybrids'
  mutate(nationality = case_when(
    #change UK/US
    nationality == "U.K./U.S." ~ "U.S.",
    #Change Ukarane/USSR
    nationality == "U.S.S.R/Ukraine" ~ "U.S.S.R/Russia",
    #keep all other
    TRUE ~ as.character(nationality))) %>%
  #Group by USvsRUS and year
  group_by(nationality, year_of_mission) %>%
  #Calc total sum of hrs
  summarise_at(vars(total_hrs_sum),
               sum)

#'blank' template for years

decades <- expand_grid(
  #each year
  year = 1960:2019,
  #for the two countries
  nationality = c("U.S.", "U.S.S.R/Russia")
) %>%
  mutate(decade = as.factor(year - year %% 10))

astro_waff <- astro_waff %>%
  full_join(.,
            decades, by = c("nationality" = "nationality",
                            "year_of_mission" = "year")) %>%
  arrange(year_of_mission) %>%
  mutate(#total_hrs_sum = replace_na(total_hrs_sum,
         #                           0),
         #create unique id
         index = row_number(),
         total_hrs_sum = round(total_hrs_sum))

##CREATE COLOUR PALETTE  

for (i in 1:2) {
  
  max.colour <- c("red", "blue")
  country <- c("U.S.S.R/Russia", "U.S.")
  x <- astro_waff %>% filter(nationality == paste(country[i]))
  #creates a function my.colors which interpolates n colors between blue, white and red
  my.colors <- colorRampPalette(c("white", paste(max.colour[i]))) 
  #generates 2001 colors from the color ramp
  astro_waff <- astro_waff %>%
    left_join(.,
              data.frame(COLOR_VALUE= seq(1,
                                          max(x$total_hrs_sum,
                                              na.rm = T),1), 
                         color.name = my.colors(max(x$total_hrs_sum,
                                                    na.rm = T)),
                         nationality = rep(paste(country[i]))),
              by = c("nationality" = "nationality",
                     "total_hrs_sum" = "COLOR_VALUE"))
  
}

astro_waff <- astro_waff %>% 
  mutate(colour.name = coalesce(color.name.x,color.name.y))

colour.name <- astro_waff %>%
  arrange(nationality, index) %>%
  #filter(nationality == "U.S.S.R/Russia") %>%
  ungroup() %>%
  pull(colour.name) %>%
  na.omit()

### >> c) Plotting ----

plotRUS <-
  ggplot(astro_waff %>%
         filter(nationality == "U.S.S.R/Russia"), 
       aes(decade, 
           1, 
           group = index, 
           fill = total_hrs_sum)) +
  geom_bar(stat = 'identity',
           color = "#1b1f2b",
           size = 3,
           show.legend = FALSE) +
  scale_fill_distiller(palette = "Reds",
                       na.value = "#1b1f2b",
                       direction = 1) +
  labs(x = "DECADE",
       title = "RUSSIA",
       y = "YEAR") +
  theme(panel.background = element_rect("#1b1f2b"),
        panel.grid = element_blank(),
        rect = element_rect("#1b1f2b"),
        text = element_text(colour = "#888c97",
                            family = "Courier New"),
        plot.title = element_text(size= 25,
                                  hjust = 0.5),
        plot.caption = element_text(hjust = 1,
                                    size= 6),
        plot.margin = unit(c(90, 20, 5, 90), "pt")) +
  scale_y_continuous(breaks=NULL)

plotUSA <- 
  ggplot(astro_waff %>%
         filter(nationality == "U.S."), 
       aes(decade, 
           1, 
           group = index, 
           fill = total_hrs_sum)) +
  geom_bar(stat = 'identity',
           color = "#1b1f2b",
           size = 3,
           show.legend = FALSE) +
  scale_fill_distiller(palette = "Blues",
                       na.value = "#1b1f2b",
                       direction = 1) +
  labs(x = "DECADE",
       title = "USA") +
  theme(panel.background = element_rect("#1b1f2b"),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        rect = element_rect("#1b1f2b"),
        text = element_text(colour = "#888c97",
                            family = "Courier New"),
        plot.title = element_text(size = 25,
                                  hjust = 0.5),
        plot.caption = element_text(hjust= 1,
                                    size= 8),
        plot.margin = unit(c(90, 90, 5, 20), "pt")) +
  scale_y_continuous(breaks=NULL)


cowplot::ggdraw(grid.arrange(
  plotRUS,
  plotUSA,
  nrow = 1,
  top = textGrob("THE RACE FOR SPACE",
                 gp = gpar(fontface = 3, fontsize = 50, fontfamily = "Courier New", col = "#888c97")),
  bottom = textGrob(
    "Source: the Astronaut Database (https://data.mendeley.com/datasets/86tsnnbv2w/1) | Graphic: @tanyas_08",
    gp = gpar(fontsize = 9, fontfamily = "Courier New", col = "#888c97"),
    hjust = 1,
    x = 1
  ))) +
  theme(plot.background = element_rect(fill="#1b1f2b", color = "#1b1f2b"))

### >> d) Lollipop plots ---- 

ann_arrows <- tribble(
  ~xend, ~nationality, ~yend, ~x, ~y, ~event,
  #--|--|--|--|--|----
  1963, "U.S.S.R/Russia", 569, 1964, 80000,"1963: Valentina Tereshkova is the first woman in space",
  1983, "U.S.", 13906, 1970, 50000, "1983: Sally K. Ride is the first American woman in space",
  1965, "U.S.S.R/Russia", 195, 1970, 50000, "1965: First extravehicular activity",
  1965, "U.S.", 4676, 1960, 70000, "1965: First extravehicular activity",
  1994, "U.S.S.R/Russia", 103266, 2005, 90000, "1994: Most total mission hours (10 3266 hours)",
  1997, "U.S.", 83198, 1990, 90000, "1997: Most total mission hours (83 198 hours)"
)

strip.labs <- c("USA", "RUSSIA")
names(strip.labs) <- c("U.S.", "U.S.S.R/Russia")

plot1 <- ggplot(astro_waff,
       aes(x= year_of_mission, 
           y= total_hrs_sum,
           colour = nationality)) +
  geom_point(aes(fill=alpha(total_hrs_sum, 0.3)),
             alpha = 0.4, 
             shape = 21, 
             stroke= 1.5,
             show.legend = FALSE) + 
  geom_segment(aes(x = year_of_mission, 
                   xend = year_of_mission, 
                   y = 0, 
                   yend = total_hrs_sum),
               alpha = 0.3,
               size = 0.1,
               show.legend = FALSE) +
  facet_wrap(vars(nationality), 
             labeller = labeller(nationality = strip.labs)) +
  geom_segment(data = ann_arrows, 
             aes(y = y,
                 x = x,
                 xend = xend,
                 yend = yend),
             show.legend = FALSE,
             arrow = arrow(length = unit(0.01, "npc"))) +
  geom_textbox(data = ann_arrows, 
               aes(y = y,
                   x = x,
                   label = event),
               show.legend = FALSE,
               fill = "#1b1f2b",
               color = "#888c97",
               family = "Courier New",
               size = 3) +
  coord_polar() +
  scale_fill_brewer(palette = "Greys",
                       na.value = "#1b1f2b",
                       direction = 1) +
  scale_color_manual(values = c("#413BA3", "#A34E4F")) +
  labs(title = "THE RACE FOR SPACE",
       caption = "Source: The Astronaut Database (https://data.mendeley.com/datasets/86tsnnbv2w/1) | Graphic: @tanyas_08",
       subtitle = "Russia had its first mission into space in 1961, USA followed in 1962. \n Up to 2019 Russia has spent 2 058 416 hours \n and the USA 1 438 251 hours exploring space") +
  theme(panel.background = element_rect("#1b1f2b"),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        rect = element_rect("#1b1f2b"),
        text = element_text(colour = "#888c97",
                            family = "Courier New"),
        strip.background = element_rect("#1b1f2b"),
        strip.text = element_text(colour = "#888c97",
                                  size = 14),
        plot.title = element_text(size = 25,
                                  hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust= 1,
                                    size= 8)) +
  scale_y_continuous(breaks=NULL)

ggsave("2020/Astronaut/RaceForSpace.png", 
       plot1, 
       height = 10.5, width = 16.8, 
       units = "in", dpi = 600)

# End of script ----