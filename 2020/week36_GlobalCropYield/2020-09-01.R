#' ------------------------------------------------------------------#
#'  GLOBAL CROP YIELD
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-09-01)
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
library(biscale)
library(sf)
library(spData)
library(cowplot)


#Font Choices
font_add_google("Roboto Mono", "Roboto Mono")

#todays prompts
options(prompt = c("\U1F69C"),
        continue = "\U1F33E")


### >> b) Import data and clean ----
key_crop_yields_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
fertilizer_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_crop_yield_vs_fertilizer_application.csv')
tractors_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')
land_use_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv')
arable_land_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/arable_land_pin.csv')


key_crop_yields_long <-
    key_crop_yields_raw  %>% 
        pivot_longer(cols = -c(Entity, Code, Year))  %>% 
        group_by(Entity, Code, name)  %>% 
        summarise(mean_prod = mean(value, na.rm = TRUE))

tractors_long <-
    tractors_raw %>%
    group_by(Entity, Code)  %>% 
    summarise(num_tract = mean(`Tractors per 100 sq km arable land`, na.rm = TRUE),
              cereal_yield = mean(`Cereal yield (kilograms per hectare) (kg per hectare)`, na.rm = TRUE)) %>%
    na.omit()

tractor_biclass <- 
    bi_class(tractors_long, 
             x = num_tract, 
             y = cereal_yield, 
             style = "quantile", 
             dim = 3) %>%
    mutate(Entity = case_when(Entity == "Brunei" ~ "Brunei Darussalam",
                              Entity == "Cote d'Ivoire" ~ "Côte d'Ivoire",
                              Entity == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
                              Entity == "Dominica" ~ "Dominican Republic",
                              Entity == "Russia" ~ "Russian Federation",
                              Entity == "Swaziland" ~ "eSwatini",
                              Entity == "Timor" ~ "Timor-Leste",
                              Entity == "Laos" ~ "Lao PDR",
                              Entity == "North Korea" ~ "Dem. Rep. Korea",
                              TRUE ~ Entity))

productivity <-
    full_join(world,
              tractor_biclass,
              by = c("name_long" = "Entity"))

legend <- bi_legend(pal = "DkCyan",
                    dim = 3,
                    xlab = "per 1 ha" ,
                    ylab = "Cerial Yield ",
                    size = 8)

map <-
    ggplot() +
    geom_sf(data = productivity, 
            aes(fill = bi_class), 
            color = "white", 
            size = 0.1, 
            show.legend = FALSE) +
    bi_scale_fill(pal = "DkCyan", 
                  dim = 3,
                  na.value = "grey90") +
    labs(
        title = "Race and Income in St. Louis, MO",
        subtitle = "Dark Blue (DkBlue) Palette"
    ) +
    bi_theme()

ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.05, .2, 0.2, 0.2)
