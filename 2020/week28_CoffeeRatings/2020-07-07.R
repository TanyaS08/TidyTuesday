#' ------------------------------------------------------------------#
#'  COFFEE RATINGS
#'  - Data and README can be found at:
#'    https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-07/readme.md
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidytuesdayR)
library(tidyverse)
library(tidylog)

#todays prompts
options(prompt = "\U2615",
        continue = "\U1F331")

### >> b) Dataset ----

tuesdata <- tidytuesdayR::tt_load(2020, week = 28)

coffee_ratings <- tuesdata$coffee_ratings %>%
  group_by(species, country_of_origin) %>%
  summarise_at(vars(total_cup_points),
               mean,
               na.rm = TRUE)


# End of script ----