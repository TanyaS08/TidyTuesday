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
library(ggmap)

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
                              margin = margin(b = -0.1, t = 0.5, l = 2, unit = "cm"))) +
  labs(caption = 'Source: "Historical Census Statistics on Population Totals By Race, 1790 to 1990"')

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
                                  )) +
  labs(caption = 'Source: "Historical Census Statistics on Population Totals By Race, 1790 to 1990"')

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
  #new var with Two letter codes for states (matching with centers df)
  mutate(id = as.factor(str_replace_all(index, c("New York" = "NY",
                                    "Vermont" = "VT",
                                    "Pennsylvania" = "PA",
                                    "Louisiana" = "LA",
                                    "South Carolina" = "SC",
                                    "Ohio" = "OH",
                                    "Maine" = "ME",
                                    "Massachusetts" = "MA")))) %>%
  #join with centres df by two letter country code
  left_join(.,
            centers,
            by = "id") %>%
  #for brevity remove non-essential vars
  select(accomplishment, year, id, x, y, gender) %>%
  #add arrow end points
  mutate(yend = rep(34.18126, nrow(.)),
         xend = rep(-87.51723, nrow(.)))

#Remove duplicate years to prevent overcrowding
#' ------------------------------------------------------------------#
#' PERHAPS POSSIBLE TO TO ACCOMODATE ALL ACHIEVEMENTS IN THE 
#' ANIMATED PLOT.
#' ------------------------------------------------------------------#
firsts <- firsts[-c(13, 15, 1, 3, 11, 7, 12, 16, 17, 18 ,19),]

### >> b) Mapping ----

#static map
panel_firsts <- 
  #use previous panel plot
  panel_census +
  #add arrows
  geom_curve(
    data = firsts,
    aes(xend = x, 
        yend = y, 
        x = xend, 
        y = yend),
    curvature = -0.3,
    arrow = arrow(length = unit(0.03, "npc"))) + 
  #add achievements
  geom_textbox(
    data = firsts,
    aes(xend, yend, label = accomplishment),
    width = grid::unit(0.27, "npc"), # 27% of plot panel width
    hjust = 0, vjust = 1,
    halign = 0.5, # centered text
    size = 2) +
  #update caption for new data source
  labs(caption = 'Source: Wikipedia, "List of African-American firsts" | "Historical Census Statistics on Population Totals By Race, 1790 to 1990"')

ggsave("2020/AfricanAmericanHistory/StaticMap_WithFirsts.png", 
       panel_firsts, 
       height = 7, width = 11, 
       units = "in", dpi = 600)

#dynamic map

gif_firsts <- ggplot() +
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
    halign = 0.5,
    size = 8) +
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
    plot.title = element_text(size= 22,
                              hjust=0.5,
                              color = "#4e4d47",
                              margin = margin(b = -0.1, t = 0.5, l = 2, unit = "cm")),
    plot.subtitle  = element_text(size= 22,
                                  hjust=0.5,
                                  color = "#4e4d47",
                                  margin = margin(b = -0.1, t = 0.3, l = 2, unit = "cm"))) +
  labs(caption = 'Source: Wikipedia, "List of African-American firsts" | "Historical Census Statistics on Population Totals By Race, 1790 to 1990"')

anim_save("animate_fists.gif", 
          animation = animate(gif_firsts, 
                              width = 1000, height = 800), 
          path = "2020/AfricanAmericanHistory/")


### 3) Histroy of the slave trade - global (using Plotly) ----
### >> a) data crunching ----  

#' ------------------------------------------------------------------#
#' 
#' THIS CHUNK IS COMPUTATIONALLY TIME CONSUMING - IMPORT THE PRODUCT 
#'    - "routes_geocoded.csv"
#' 
#'slave_routes_paired <-
#'  slave_routes  %>%
#'  #give value to for slaves arriving
#'  mutate(n_arrived = replace_na(n_slaves_arrived,
#'                                1),
#'    place_of_purchase = str_replace_all(place_of_purchase,
#'                                        c(", port unspecified" = "",
#'                                          ", unspecified" = "")),
#'    port_arrival = str_replace_all(port_arrival,
#'                                   c(", port unspecified" = "",
#'                                     ", unspecified" = "",
#'                                     ", colony unspecified" = ""))) %>%
#'  #group by arrival and purchase to find routes
#'  group_by(place_of_purchase, port_arrival) %>%
#'  #summate for the various routes
#'  summarise(n_arrived = sum(n_arrived)) %>%
#'  #remove NA's i.e. incomplete routes
#'  na.omit(.) %>%
#'  #find geocode based on location name query
#'  mutate_geocode(., 
#'                 place_of_purchase) %>%
#'  rename(lon_start = lon,
#'         lat_start = lat) %>%
#'  #find geocode based on location name query
#'  mutate_geocode(., 
#'                 port_arrival) %>%
#'  rename(lon_end = lon,
#'         lat_end = lat) 
#'         
#' write.csv(slave_routes_paired,
#'          file = "2020/AfricanAmericanHistory/routes_geocoded.csv")
#' ------------------------------------------------------------------#

slave_routes_paired <- read.csv("2020/AfricanAmericanHistory/routes_geocoded.csv",
                                row.names = 1) %>%
  na.omit()

slave_routes_origin <- 
  slave_routes_paired %>%
  #pivot into long format
  group_by(place_of_purchase) %>%
  summarise(n_left = sum(n_arrived)) %>%
  mutate_geocode(.,
                 place_of_purchase) %>%
  rename(Port = place_of_purchase)

slave_routes_list <- 
  slave_routes_paired %>%
  #pivot into long format
  group_by(port_arrival) %>%
  summarise(n_left = sum(n_arrived)) %>%
  mutate_geocode(.,
                 port_arrival) %>%
  rename(Port = port_arrival) %>%
  bind_rows(.,
            slave_routes_origin)

geo <- list(
  scope = 'world',
  projection = list(type = 'azimuthal equal area'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

fig <- plot_geo(locationmode = 'world', color = I("red"))
fig <- fig %>% add_markers(
  data = slave_routes_list, x = ~lon, y = ~lat, text = ~Port,
  size = ~n_left, hoverinfo = "text", alpha = 0.5
)
fig <- fig %>% add_segments(
  data = slave_routes_paired,
  x = ~lon_start, xend = ~lon_end,
  y = ~lat_start, yend = ~lat_end,
  alpha = 0.3, size = I(1), hoverinfo = "none"
)
fig <- fig %>% layout(
  title = 'Slave trade routes<br>(Hover for port names)',
  geo = geo, showlegend = FALSE
)

# End of script ----