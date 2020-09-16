#' ------------------------------------------------------------------#
#' FRIENDS
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-09-08)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(tidylog)
library(ggforce)
library(patchwork)
library(ggtext)
library(broom)
library(here)
library(showtext)
library(cowplot)
library(stringr)

#todays prompts
options(prompt = c("\U1F44F"),
        continue = "\U1F6CB")

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

str_which(friends$text,
                "break")