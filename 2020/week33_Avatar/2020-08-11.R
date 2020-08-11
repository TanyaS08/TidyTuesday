#' ------------------------------------------------------------------#
#'  AVATAR
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-08-11)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(lubridate)
library(ggforce)
library(sysfonts)
library(showtext)
library(tvthemes)
library(ggforce)
library(rvest)
library(ggalluvial)
library(ggtext)
library(ggstream)


#todays prompts
options(prompt = "\U1F30A",
        continue = "\U1F525")

### >> b) Import data and clean ----

#Get data for the week
tuesdata <- tidytuesdayR::tt_load(2020, week = 33)

avatar_raw <- tuesdata$avatar %>%
#make charaters a factor
mutate(character = as.factor(character)) %>%
#group for summarising
select(book, chapter, chapter_num, character) %>%
#tally number of times a character speaks per episode
#emove scene discription
filter(character != "Scene Description") %>%
#cod elevels for books/seasons
mutate(book = factor(book, levels = c("Water", 
                                      "Earth", 
                                      "Fire"))) %>%
#arrange chronologically
arrange(book, chapter_num)

### 1) Webscraping to get tribe data ----

EarthKingdom <-
    read_html("https://avatar.fandom.com/wiki/Category:Earth_Kingdom_characters") %>%
        #select nodes based on css style name
        html_nodes(".category-page__member-link") %>% 
        #convert to text
        html_text() %>% 
        as_tibble() %>%
        mutate(Nation = rep("Earth Kingdom",
                            times = nrow(.)))

WaterTribe <-
    read_html("https://avatar.fandom.com/wiki/Category:Water_Tribe_characters") %>%
        #select nodes based on css style name
        html_nodes(".category-page__member-link") %>% 
        #convert to text
        html_text() %>% 
        as_tibble() %>%
        mutate(Nation = rep("Water Tribe",
                            times = nrow(.)))

FireNation <-
    read_html("https://avatar.fandom.com/wiki/Category:Fire_Nation_characters") %>%
        #select nodes based on css style name
        html_nodes(".category-page__member-link") %>% 
        #convert to text
        html_text() %>% 
        as_tibble() %>%
        mutate(Nation = rep("Fire Nation",
                            times = nrow(.)))         

AirNomads <-
    read_html("https://avatar.fandom.com/wiki/Category:Air_Nomad_characters") %>%
        #select nodes based on css style name
        html_nodes(".category-page__member-link") %>% 
        #convert to text
        html_text() %>% 
        as_tibble() %>%
        mutate(Nation = rep("Air Nomad",
                            times = nrow(.)))                                                                                  

#Bind character lists
CharacterList <- 
    AirNomads %>%
    rbind(.,
           FireNation)  %>% 
    rbind(.,
          WaterTribe)  %>% 
    rbind(.,
          EarthKingdom)     

#Combine with Avatar df

avatar <- 
    avatar_raw  %>% 
        left_join(.,
                  CharacterList,
                  by = c("character" = "value"))  %>% 
        #Toph is a special case
        mutate(Nation = case_when(character == "Toph" ~ "Earth Kingdom",
                                  TRUE ~ Nation),
               #new charter group for plotting  
               character_group = ifelse(character %in% c("Aang",
                                                         "Sokka",
                                                         "Katara",
                                                         "Zuko",
                                                         "Iroh",
                                                         "Ozai",
                                                         "Azula",
                                                         "Toph"),
                                        character,
                                        Nation),
               chapter_num = case_when(book == "Water" ~ chapter_num + 100,
                                        book == "Earth" ~ chapter_num + 200,
                                        book == "Fire" ~ chapter_num + 300 ))  %>% 
        mutate_at(vars(character, Nation, character_group),
                  as.factor)  %>% 
        na.omit() %>%
        group_by(chapter, book, chapter_num, character_group, Nation)  %>% 
        arrange(book, chapter_num) %>% 
        tally()  %>% 
        ungroup()  %>% 
        mutate(episode = as.factor(as.integer(factor(chapter_num)))) %>%
        arrange(episode)  %>% 
        pivot_wider(id_cols = character_group, names_from = episode, values_from = n, values_fill = 0)  %>% 
        pivot_longer(cols = -character_group)


ggplot(data = avatar,
       aes(x = name, y = value, alluvium = character_group)) +
  geom_alluvium(aes(fill = character_group, colour = character_group),
                alpha = .5, decreasing = FALSE)  +
  geom_textbox(aes(
    label = 'Book 1 - "Water"',
    x = 1,
    y = 250,
    vjust = 0),
    fill = "#fffff0",
    box.colour = "#fffff0",
    width = 0.3)

#streamplot



# End of script ----