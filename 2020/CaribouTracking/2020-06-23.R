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
               mean)

### >> b) plotting

ggplot(data = distance) +
  geom_dotplot(aes(x )) 


# End of script ----