#' ------------------------------------------------------------------#
#' BEER AWARDS
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-10-20)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidytext)
library(tidyverse)
library(tidylog)
library(geofacet) 
library(ggforce)
library(sp)

#todays prompts
options(prompt = c("\U1F37A"),
        continue = "\U1F968")

### >> b) Data ----

beer_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

summary(as.factor(beer_raw$category))

beer <- 
    beer_raw %>%
    mutate(state = str_to_upper(state),
           category = case_when(str_detect(category, "Ale") ~ "Ale",
                                str_detect(category, "Stout") ~ "Stout",
                                str_detect(category, "Pilsener") ~ "Pilsner",
                                str_detect(category, "Pilsner") ~ "Pilsner",
                                str_detect(category, "Lager") ~ "Lager",
                                str_detect(category, "Bitter") ~ "Bitter"))  %>% 
    na.omit()  %>% 
    group_by(state, category)  %>% 
    count()  %>% 
    top_n(1, n)  %>% 
    group_by(category)  %>% 
    mutate(count = ifelse(n/max(n) == 0,
                          NA,
                          n/max(n)))

ggplot(beer) +
    geom_col(aes(x = state,
                y = count)) +
    facet_wrap(vars(category))

beer_ale <-
    beer  %>% 
    filter(category == 'Ale') %>%
    mutate(id = row_number())

ale <-
    tibble(
        id = rep(1:nrow(beer_ale), each = 10),
        x = rep(c(30, 25, 24, 24.5, 23, 57, 55.5, 56, 55, 50), nrow(beer_ale)),
        y = rep(c(10, 80, 90, 100, 120, 120, 100, 90, 80, 10), nrow(beer_ale)),
    ) %>%
    left_join(.,
              beer_ale,
              by = c('id'))

ggplot() +
    geom_shape(aes(y = c(10, 80, 90, 100, 120, 120, 100, 90, 80, 10),
                     x = c(30, 25, 24, 24.5, 23, 57, 55.5, 56, 55, 50)),
                 radius = unit(0.6, 'cm')) +
    geom_shape(aes(y = c(100, 120, 120, 100),
                   x = c(24.5, 23, 57, 55.5)),
               radius = unit(0.6, 'cm'),
               fill = '#FEFAE4')

ggplot() +
    geom_shape(aes(y = c(10, 80, 90, 100, 120, 120, 100, 90, 80, 10),
                   x = c(30, 25, 24, 24.5, 23, 57, 55.5, 56, 55, 50)),
               radius = unit(0.6, 'cm'))

list(10, 80, 90, 100, 120, 120, 100, 90, 80, 10)

ggplot(data = ale) +
    geom_shape(aes(y = y,
                   x = x,
                   group = id,
                   fill = log(n)),
               radius = unit(0.6, 'cm')) +
    facet_geo(~ state) +
    scale_fill_gradient(low = '#A38950',
                        high = '#F5CE78')
    


    
    
    
summary(as.factor(beer$category))
