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
library(lubridate)

#todays prompts
options(prompt = "\U1F680",
        continue = "\U1F31F")

### >> d) Data import ----
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)

astronauts <- tuesdata$astronauts


# End of script ----