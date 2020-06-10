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
devtools::install_github("thebioengineer/tidytuesdayR")

### >> b) Import data and clean ----

#Get data fro the week
tuesdata <- tidytuesdayR::tt_load(2020, week = 24)

#extract the first df
firsts <- tuesdata$firsts %>%
  mutate(
    #turn work categories into a factor
    category = as.factor(category),
    #convert gender to factor
    gender = as.factor(
      #for brevity rename to male and female
      ifelse(gender == "African-American Firsts",
             "male",
             "female")),
    year = year(as.Date(as.character(year), format = "%Y")))

#extract the science df
science <- tuesdata$science

### 1) Visualising Data ----
### >> a) Firsts ----

ggplot(data = firsts %>%
         #group by 'target' categories
         group_by(year,
                  category,
                  gender) %>%
         #tally the number for each year, gender, category combo
         summarise(cumsum = n()) %>%
         #now only group by category and gender
         group_by(category,
                  gender) %>%
         #calculate cumulative frequency for each category and gender
         mutate(cumsum = cumsum(cumsum))) +
  geom_line(aes(x = year,
                y = cumsum,
                colour = gender)) +
  facet_wrap(vars(category)) +
  ylab("Cummulative number of achievements by African Americans") +
  xlab("Year") +
  theme_bw()

# End of script ----