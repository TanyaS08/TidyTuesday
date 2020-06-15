#' ------------------------------------------------------------------#
#'  AFRICAN AMERICAN ACHIEVEMENTS
#'  - README can be found at 
#'    (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-06-09/readme.md)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)
library(lubridate)
library(ggforce)
devtools::install_github("thebioengineer/tidytuesdayR")

### >> b) Import data and clean ----

#Get data for the week
tuesdata <- tidytuesdayR::tt_load(2020, week = 24)

### 1) Visualising Data ----
### >> a) Firsts ----

#extract the first df
firsts <- tuesdata$firsts %>% 
  mutate(
    #calcualte the decade by subtracting the 10th unit and make factor
    decade = as.factor(year - year %% 10),
    #turn work categories into a factor
    category = as.factor(category),
    #convert gender to factor
    gender = as.factor(
      #for brevity rename to male and female
      ifelse(gender == "African-American Firsts",
             "male",
             "female"))) %>%
  #select only target variables
  select(.,
         gender,
         category,
         decade) %>%
  #group by 'target' categories
  group_by(decade,
           category,
           gender) %>%
  #tally the number for each year, gender, category combo
  summarise(value = n()) %>%
  #gather into tidy-er format for ggforce plotting
  gather_set_data(., 1:3)
  

#Visusalising

p1 <- ggplot(data = firsts, 
             aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = category), 
                     alpha = 0.5, 
                     axis.width = 0.4) +
  geom_parallel_sets_axes(axis.width = 0.4) +
  geom_parallel_sets_labels(colour = 'white',
                            angle = 0) +
  scale_fill_brewer(palette = "Set3")  +
  theme_void() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "grey50"))

ggsave("2020/AfricanAmericanFirsts/2020-06-09_African-American_Achievements.png", 
       p1, 
       height = 6, width = 11, 
       units = "in", dpi = 300)

##Clean data for tabulation

firsts <- tuesdata$firsts %>% 
  mutate(
    #calcualte the decade by subtracting the 10th unit and make factor
    Decade = as.factor(year - year %% 10),
    #turn work categories into a factor
    category = as.factor(category),
    #convert gender to factor
    Gender = as.factor(
      #for brevity rename to male and female
      ifelse(gender == "African-American Firsts",
             "male",
             "female")))

### >> b) Science ----

#extract the science df
science <- tuesdata$science

# End of script ----