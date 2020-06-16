#' ------------------------------------------------------------------#
#'  AFRICAN AMERICAN ACHIEVEMENTS
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-06-16)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)
library(lubridate)
library(ggforce)
library(plotly)
library(rjson)
library(geojsonio)
library(rgdal)
library(broom)
library(viridis)
library(gganimate)
library(transformr)
library(rgeos)
library(ggtext)
library(ggrepel)

devtools::install_github("thebioengineer/tidytuesdayR")

#prompt option choice of the day
options(prompt = "\U1F98E",
        continue = "\U1F98A")

### >> b) Import data and clean ----

#Get data for the week
tuesdata <- tidytuesdayR::tt_load(2020, week = 25)
blackpast <- tuesdata$blackpast
slave_routes <- tuesdata$slave_routes
african_names <- tuesdata$african_names

### 1) Tracking the liberation of slaves across the USA ----
### >> a) Clean data - census ----

census <- tuesdata$census %>%
  #remove totals for USA and for division
  filter(region != "USA Total" &
           division != is.na(division)) %>%
  mutate(division = as.factor(division),
         #calcualte proportion free:all
         ratio = black_free/black)

### >> b) Mapping ----
#Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
#load map
spdf <- geojson_read("2020/AfricanAmericanHistory/us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
map_census <- tidy(spdf, region = "google_name") %>%
  #create lables for the divisions
  mutate(division = as.factor(str_replace_all(id, c("Alabama|Kentucky|Mississippi|Tennessee" = "East South Central",
                                    "New Jersey|New York|Pennsylvania" = "Middle Atlantic",
                                    "Connecticut|Maine|Massachusetts|New Hampshire|Rhode Island|Vermont" = "New England",
                                    "Delaware|Florida|Georgia|Maryland|North Carolina|South Carolina|Virginia|District of Columbia|West Virginia" = "South Atlantic",
                                    "Illinois|Indiana|Michigan|Ohio|Wisconsin" = "East North Central",
                                    "Iowa|Kansas|Minnesota|Missouri|Nebraska|North Dakota|South Dakota" = "West North Central",
                                    "Arkansas|Louisiana|Oklahoma|Texas" = "West South Central",
                                    "Alaska|California|Hawaii|Oregon|Washington" = "Pacific",
                                    "Arizona|Colorado|Idaho|Montana|Nevada|New Mexico|Utah|Wyoming" = "Mountain")))) %>%
  full_join(.,
            census,
            by = "division") %>%
  #remove NAs
  na.omit()

#Static map
panel_census <- ggplot() +
  geom_polygon(data = map_census, 
               aes(x = long, 
                   y = lat, 
                   fill = ratio, 
                   group = group), 
               size = 0) +
  facet_wrap(vars(year)) +
  scale_fill_gradientn( 
    colours = viridis(5), 
    name = "") +
  ggtitle( "Proportion of free African-Americans through the decades" ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22,
                              hjust=0.5,
                              color = "#4e4d47",
                              margin = margin(b = -0.1, t = 0.5, l = 2, unit = "cm"))) 

ggsave("2020/AfricanAmericanHistory/StaticMap_nolabels.png", 
       panel_census, 
       height = 7, width = 11, 
       units = "in", dpi = 600)

#dynamic map
gif_census <- ggplot() +
  geom_polygon(data = map_census, 
               aes(x = long, 
                   y = lat, 
                   fill = ratio, 
                   group = id), 
               size = 0) +
  transition_states(year) +
  labs(subtitle = "Year: {closest_state}") +
  scale_fill_gradientn( 
    colours = viridis(5), 
    name = "")  +
  ggtitle( "Proportion of free African-Americans" ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 18,
                              hjust=0.5,
                              color = "#4e4d47",
                              #margin = margin(b = -0.1, t = 0.5, l = 2, unit = "cm")
                              ),
    plot.subtitle  = element_text(size= 18,
                                  hjust=0.5,
                                  color = "#4e4d47",
                                  #margin = margin(b = -0.1, t = 0.3, l = 2, unit = "cm")
                                  ))

anim_save("animate_nolabels.gif", 
          animation = animate(gif_census, 
                              height = 461, width = 644), 
          path = "2020/AfricanAmericanHistory/")

### 2) Tracking the liberation of slaves across the USA - adding firsts ----
### >> a) Clean data - firsts (from previous week) ----  

#for centers of states - needed for arrows
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), 
                                       id=spdf@data$iso3166_2)) %>%
  mutate(id = as.factor(id))

tuesdata_09 <- tidytuesdayR::tt_load(2020, week = 24)
firsts <- tuesdata_09$firsts %>% 
  mutate(
    #calcualte the decade by subtracting the 10th unit and make factor
    year = as.numeric(year - year %% 10),
    #turn work categories into a factor
    category = as.factor(category),
    #convert gender to factor
    gender = as.factor(
      #for brevity rename to male and female
      ifelse(gender == "African-American Firsts",
             "male",
             "female")),
    #find entries that have a state location
    index = str_extract(person, "Alabama|Kentucky|Mississippi|Tennessee|New Jersey|New York|Pennsylvania|Connecticut|Maine|Massachusetts|New Hampshire|Rhode Island|Vermont|Delaware|Florida|Georgia|Maryland|North Carolina|South Carolina|Virginia|District of Columbia|West Virginia|Illinois|Indiana|Michigan|Ohio|Wisconsin|Iowa|Kansas|Minnesota|Missouri|Nebraska|North Dakota|South Dakota|Arkansas|Louisiana|Oklahoma|Texas|Alaska|California|Hawaii|Oregon|Washington|Arizona|Colorado|Idaho|Montana|Nevada|New Mexico|Utah|Wyoming")) %>%
  filter(
    #keep only entries that have a location name and are correct decade
    year %in% c(1790:1870) &
      index != is.na(index)) %>%
  mutate(id = as.factor(str_replace_all(index, c("New York" = "NY",
                                    "Vermont" = "VT",
                                    "Pennsylvania" = "PA",
                                    "Louisiana" = "LA",
                                    "South Carolina" = "SC",
                                    "Ohio" = "OH",
                                    "Maine" = "ME",
                                    "Massachusetts" = "MA")))) %>%
  left_join(.,
            centers,
            by = "id") %>%
  #for brevity
  select(accomplishment, year, id, x, y, gender) %>%
  #add arrow end points
  mutate(yend = rep(34.18126, nrow(.)),
         xend = rep(-87.51723, nrow(.)))
firsts <- firsts[-c(13, 15, 1, 3, 11, 7, 12, 16, 17, 18 ,19),]

### >> b) Mapping ----

#static map
panel_firsts <- panel_census +
  geom_curve(
    data = firsts,
    aes(xend = x, 
        yend = y, 
        x = xend, 
        y = yend),
    curvature = -0.3,
    arrow = arrow(length = unit(0.03, "npc"))) +  
  geom_textbox(
    data = firsts,
    aes(xend, yend, label = accomplishment),
    width = grid::unit(0.27, "npc"), # 22% of plot panel width
    hjust = 0, vjust = 1,
    size = 2)

ggsave("2020/AfricanAmericanHistory/StaticMap_WithFirsts.png", 
       panel_firsts, 
       height = 7, width = 11, 
       units = "in", dpi = 600)

#dynamic map

ggplot() +
  geom_polygon(
    data = map_census, 
    aes(x = long, 
        y = lat, 
        fill = ratio, 
        group = id), 
    size = 0) +
  geom_curve(
    data = firsts,
    aes(xend = x, 
        yend = y, 
        x = xend, 
        y = yend),
    curvature = -0.3,
    arrow = arrow(length = unit(0.03, "npc"))) +  
  geom_textbox(
    data = firsts,
    aes(xend, yend, label = accomplishment),
    width = grid::unit(0.22, "npc"), # 22% of plot panel width
    hjust = 0, vjust = 1,
    size = 3) +
  transition_states(year) +
  labs(subtitle = "Year: {closest_state}") +
  scale_fill_gradientn( 
    colours = viridis(5), 
    name = "")  +
  ggtitle( "Proportion of free African Americans" ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22,
                              hjust=0.5,
                              color = "#4e4d47",
                              margin = margin(b = -0.1, t = 0.5, l = 2, unit = "cm")),
    plot.subtitle  = element_text(size= 22,
                                  hjust=0.5,
                                  color = "#4e4d47",
                                  margin = margin(b = -0.1, t = 0.3, l = 2, unit = "cm")))

# End of script ----