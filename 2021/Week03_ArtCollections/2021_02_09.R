#' ------------------------------------------------------------------#
#' ART COLLECTIONS
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidytext)
library(tidyverse)
library(tidylog)
library(geofacet) 
library(ggforce)
library(sp)
library(imager)
library(here)

#todays prompts
options(prompt = c("\U1F58C"),
        continue = "\U1F3A8")

### >> b) Data ----


tuesdata <- tidytuesdayR::tt_load(2021, week = 3)
artwork <- tuesdata$artwork

artwork %>% 
  filter(artist == "Warhol, Andy") %>%
  drop_na(thumbnailUrl) %>%
  pull(thumbnailUrl) 


boys <- "http://www.tate.org.uk/art/images/work/P/P07/P07125_8.jpg"
clusters <- imager::load.image(boys) %>%
  as.data.frame(wide = "c") %>%
  select(starts_with("c")) %>%
  kmeans(centers = 4)

get_dominant_hex_from_image <- function(url, colours_n = 4, ...) {
  imager::load.image(url) %>%
    as.data.frame(wide = "c") %>%
    select(starts_with("c")) %>%
    kmeans(centers = colours_n, ...) %>%
    {rgb(red = .$centers[,1],
         green = .$centers[,2],
         blue = .$centers[,3],
         alpha = 1)}
}

get_dominant_hex_from_image("http://www.tate.org.uk/art/images/work/P/P07/P07125_8.jpg") %>%
  plot(x = 1:4, y = 1:4, col = ., cex = 7, pch = 16)

system.time({ 
  artwork %>%
    filter(medium == "Oil paint on canvas") %>%
    drop_na(thumbnailUrl) %>%
    slice_sample(n = 25) %>%
    mutate(dom_colours = map(thumbnailUrl, possibly(get_dominant_hex_from_image, NA)))
})
beepr::beep()

## Post scriptum: all paintings and 16 dominant colours for a painting

art_full <- artwork %>%
  filter(artist == "Warhol, Andy") %>%
  drop_na(thumbnailUrl) %>%
  mutate(dom_colours = map(thumbnailUrl, possibly(get_dominant_hex_from_image, NA), 4))
beepr::beep()


dom_colours_by_year <-
  art_full %>%
  unnest(cols = c(dom_colours)) %>%
  select(year, id, dom_colours, width, height)

summary(dom_colours_by_year)


unique(dom_colours_by_year$id)


ggplot(dom_colours_by_year) +
  geom_dotplot(aes(x = year,
                   fill = id))


dom_colours_by_year <- art_full_unnested %>%
  select(year, id, dom_colours) %>%
  mutate(hue = map_dbl(dom_colours, hue_from_hex)) %>%
  arrange(year, hue) %>%
  group_by(year) %>%
  mutate(ncol = row_number()) %>%
  ungroup() %>% 
  group_by(dom_colours) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(hue) %>%
  mutate(n = row_number(),
         dom_colours = factor(n, labels = dom_colours))

poster <- dom_colours_by_year %>%
  filter(year > 1900 & year <= 2000) %>%
  ggplot(aes(x = year, fill = dom_colours)) +
  geom_bar(position = "fill") +
  scale_fill_identity() +
  coord_flip() +
  scale_x_reverse(minor_breaks = NULL) +
  labs(title = "Evolution of oil paints dominant colours",
       subtitle = "Tate Art Museum, 1901 - 2000 years",
       caption = "Воскресный скRинкаст \nData from Tidy Tuesday Art Collections dataset \nhttps://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-12/readme.md") +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("2021_01_17_tate_art_museum/poster3cols_XX.png", plot = poster)

