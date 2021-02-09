#' ------------------------------------------------------------------#
#' NCAA WOMENS BASKETBALL
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-10-06)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)

#todays prompts
options(prompt = c("\U1F3C0"),
        continue = "\U1F3C6")

### >> b) Data ----
tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')
