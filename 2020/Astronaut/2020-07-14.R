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
               sum)  %>%
  mutate(
    #round up hours
    total_hrs_sum = round(total_hrs_sum),
    #calcualte decade
    decade = as.factor(year_of_mission - year_of_mission %% 10))

### >> c) Lollipop plot ---- 

#labels for strip plot titles
strip.labs <- c("USA", "RUSSIA")
names(strip.labs) <- c("U.S.", "U.S.S.R/Russia")

#text annotations
ann_arrows <- tribble(
  ~xend, ~nationality, ~yend, ~x, ~y, ~event,
  #--|--|--|--|--|----
  1963, "U.S.S.R/Russia", 569, 1970, 80000,"1963: Valentina Tereshkova is the first woman in space",
  1983, "U.S.", 13906, 1980, 50000, "1983: Sally K. Ride is the first American woman in space",
  1965, "U.S.S.R/Russia", 195, 1980, 90000, "18 March, 1965: First 'space walk' by Alexei Leonov for 12 minutes and 9 seconds",
  1969, "U.S.", 6961, 1970, 70000, "20 July, 1969: Neil Armstrong and Buzz Aldrin land on the moon",
  1968, "U.S.", 2154, 1960, 70000, "1968: The Apollo 8 is the first crewed spacecraft to orbit the moon",
  1994, "U.S.S.R/Russia", 103266, 2005, 90000, "1994: Most total mission hours (10 3266 hours)",
  1997, "U.S.", 83198, 1990, 90000, "1997: Most total mission hours (83 198 hours)",
  1961, "U.S.S.R/Russia", 27, 1964, 90000, "April 12, 1961: Yuri Gagarin is the world's frist cosmonaut aboard Vostok 1"
)


plot1 <- 
  ggplot(astro_waff,
         aes(x= year_of_mission, 
             y= total_hrs_sum,
             colour = nationality)) +
  #points of lollipops
  geom_point(aes(
    #play with transparency to create star glow
    fill=alpha(total_hrs_sum, 0.3)),
    alpha = 0.4, 
    shape = 21, 
    stroke= 1.5,
    show.legend = FALSE) + 
  #sticks of lollipops
  geom_segment(aes(x = year_of_mission, 
                   xend = year_of_mission, 
                   y = 0, 
                   yend = total_hrs_sum),
               alpha = 0.3,
               size = 0.1,
               show.legend = FALSE) +
  facet_wrap(vars(nationality), 
             labeller = labeller(nationality = strip.labs)) +
  #annotation arrows
  geom_segment(data = ann_arrows, 
               aes(y = y,
                   x = x,
                   xend = xend,
                   yend = yend),
               show.legend = FALSE,
               arrow = arrow(length = unit(0.01, "npc"))) +
  #annotation textboxes
  geom_textbox(data = ann_arrows, 
               aes(y = y,
                   x = x,
                   label = event),
               show.legend = FALSE,
               fill = "#1b1f2b",
               color = "#888c97",
               family = "Courier New",
               size = 3) +
  #flip to make circular
  coord_polar() +
  #set fill of points
  scale_fill_brewer(palette = "Greys",
                    na.value = "#1b1f2b",
                    direction = 1) +
  #set colour for two nationalities
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
  #remove y axis vals
  scale_y_continuous(breaks=NULL)

ggsave("2020/Astronaut/RaceForSpace.png", 
       plot1, 
       height = 8.5, width = 15, 
       units = "in", dpi = 600)

# End of script ----