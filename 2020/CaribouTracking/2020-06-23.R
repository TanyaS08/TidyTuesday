#' ------------------------------------------------------------------#
#'  CARIBOU LOCATION TRACKING
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-06-23)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(lubridate)
library(cartography)
library(sf)
library(spData)
library(geosphere)

#todays prompts
options(prompt = "\U1F98C",
        continue = "\U2744")

### >> d) Data import ----
tuesdata <- tidytuesdayR::tt_load(2020, week = 26)
individuals <- tuesdata$individuals %>%
  #set factors
  mutate_at(vars("animal_id", "sex", "life_stage", "study_site", "death_cause"),
            as.factor)

locations <- tuesdata$locations %>%
  #set factors
  mutate_at(vars("animal_id", "study_site", "season"),
            as.factor) %>%
  mutate(timestamp = as_datetime(timestamp))

semi_join(individuals,
          locations,
          by = "animal_id")

### 0) Site location map ----
### >> a) Create df with co-ords for sites ----

#using locations dataset
sites <- locations %>%
  #group by study site
  group_by(study_site) %>%
  #calcualte mean lat and long based on where indivs were collared
  summarise_at(vars(longitude, latitude),
               mean)

canada <- world %>%
  #select south africa
  filter(name_long == "Canada")

### >> b) Create map with co-ords as an inset ----

inset <- ggplot(canada) +
  #add canada geom as base map
  geom_sf(aes(geometry = geom)) +
  #add points of sites
  geom_point(data = sites,
             aes(x = longitude,
                 y = latitude)) +
  theme_void()

### 2) Calcualte distance travelled ----
### >> a) tidy and transform data ----

distance <-
  locations %>%
  #group by indiv
  group_by(animal_id) %>%
  #create new long/lat for change in location between time stamps
  mutate(
    longitude2 = lag(longitude),
    latitude2 = lag(latitude)
  ) %>%
  #remove rows with NA
  na.omit() %>%
  #calcaualte distance between readings - note this is in meters
  mutate(
    dist = distHaversine(cbind(longitude, latitude), 
                         cbind(longitude2, latitude2))) %>%
  #group by summary vars of interest
  group_by(animal_id,
           season) %>%
  #calculate total distance per day
  summarise_at(
    vars(dist),
    mean
  ) %>%
  #join with info about individual
  left_join(.,
            individuals,
            by = "animal_id") %>%
#group by grouping vars
  group_by(sex,
           season,
           study_site) %>%
  summarise_at(vars(dist),
               mean) %>%
  #set as chararcter strings
  mutate_at(vars("sex", "study_site", "season"),
            as.character) %>%
  ungroup()

#specify number of rows for loop 
row_num <- nrow(distance)

for (i in 1:row_num) {
  
  if((round(distance[i,4]/500) -1) > 0){  
    
    distance <- 
      distance  %>%
      add_row(sex = rep(paste(.[i, 1]), (round(.[i,4]/500) -1)),
              season = rep(paste(.[i, 2]), (round(.[i,4]/500) -1)),
              study_site = rep(paste(.[i, 3]), (round(.[i,4]/500) -1)))
  }
}

distance <-
  distance %>%
  #set as chararcter strings
  mutate_at(vars("sex", "study_site", "season"),
            as.factor) %>%
  #reorder for plotting
  arrange(.,
          sex, season, study_site) %>%
  #create unique id
  mutate(
    index = row_number()
  ) 
  

### >> b) plotting

## This function allows us to specify which facet to annotate
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

plot1 <- ggplot(distance, 
                aes(study_site, 
                    1, 
                    group = index, 
                    fill = sex)) +
  geom_bar(stat = 'identity',
           color = "#1b1f2b",
           size = 2.5) +
  facet_wrap(vars(season)) +
  labs(caption = 'Source: Seip DR, Price E (2019) Data from: Science update for the South Peace Northern Caribou (Rangifer tarandus caribou pop. 15) in British Columbia. Movebank Data Repository. https://doi.org/10.5441/001/1.p5bn656k | Graphic: @tanyas_08',
       x = "Study Site",
       title = expression("Average dailiy distance travelled by the \n South Peace Northern Caribou"),
       fill = "Sex") +
  scale_y_continuous(name = "Mean daily distance (km)",
                     breaks = seq(0, 17, by = 2),
                     minor_breaks = seq(0, 17, by = 2),
                     labels = paste0(seq(0, 8, by = 1), "km"),
                     expand = c(0, 1)) +
  scale_fill_brewer(palette = "Dark2",
                    guide = guide_legend(direction = "vertical", nrow = 2),
                    labels = c("Female", "Male")) +
  theme(panel.background = element_rect("#1b1f2b"),
        panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        #legend.position = c(0.90, 0.70),
        legend.text = element_text(size = 10),
        strip.background = element_rect("#1b1f2b"),
        strip.text = element_text(colour = "#888c97",
                                  size = 14),
        rect = element_rect("#1b1f2b"),
        text = element_text(colour = "#888c97"),
        plot.title = element_text(size= 22,
                                  hjust= 0.5,
                                  margin = margin(b = -0.1, t = 1, l = 2, unit = "cm")),
        plot.caption = element_text(hjust= 1,
                                    size= 6)) +
  annotation_custom2(grob=ggplotGrob(inset), 
                     data = distance[38,],
                     ymin = 11, ymax=19, xmin=5, xmax=10)

ggsave("2020/CaribouTracking/DailyDist.png", 
       plot1, 
       height = 7, width = 11, 
       units = "in", dpi = 600)

#' ------------------------------------------------------------------#
#'  Change Canada map so that we calcualte the mean range based on
#'  tracking data
#'  ALTERNATIVELY
#'  maybe instead of putting Canada as the inset put a monthly mean
#'  dist line plot for each season (not splitting out anything...)
#' ------------------------------------------------------------------#

# End of script ----