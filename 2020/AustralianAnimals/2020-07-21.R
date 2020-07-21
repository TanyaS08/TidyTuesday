#' ------------------------------------------------------------------#
#'  AUSTRALIAN PETS
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-21/readme.md)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
#devtools::install_github("coolbutuseless/ggblur")
library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(extrafont)
library(ggtext)
library(ggblur)

#todays prompts
options(prompt = "\U1F436",
        continue = "\U1F434")

### >> d) Data import ----
tuesdata <- tidytuesdayR::tt_load(2020, week = 30)

animal_outcomes <- tuesdata$animal_outcomes
