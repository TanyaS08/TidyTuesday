#' ------------------------------------------------------------------#
#'  CARIBOU LOCATION TRACKING
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-06-23)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidytuesdayR)
library(tidyverse)

### >> d) Data import ----
tuesdata <- tidytuesdayR::tt_load(2020, week = 26)
individuals <- tuesdata$individuals
locations <- tuesdata$locations



# End of script ----