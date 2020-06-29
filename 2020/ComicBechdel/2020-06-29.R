#' ------------------------------------------------------------------#
#'  UNCANNY X-MEN
#'  - Data and README can be found at:
#'    https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-06-30/readme.md
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidytuesdayR)
library(tidyverse)
library(tidylog)

### >> b) Dataset ----

#all datasets
tuesdata <- tidytuesdayR::tt_load(2020, week = 27)

#individuaL datasets
comic_bechdel <- tuesdata$comic_bechdel
xmen_bechdel <- tuesdata$xmen_bechdel
character_visualization
characters <- tuesdata$characters
covers <- tuesdata$covers
issue_collaborators <- tuesdata$issue_collaborators
locations <- tuesdata$locations


#data from previous TT for Bechdel Test data
becheldata <- tidytuesdayR::tt_load(2018, week = 9)
comic_characters <- becheldata$week9_comic_characters

# End of script ----