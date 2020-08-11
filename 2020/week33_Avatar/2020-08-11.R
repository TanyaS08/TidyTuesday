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
library(ggTimeSeries)
library(extrafont)
loadfonts() ## You need to do this at the beginning of a session.
pdfFonts()

#todays prompts
options(prompt = "\U1F30A",
        continue = "\U1F525")

# import fonts
import_avatar()

### >> b) Import data and clean ----

#Get data for the week
tuesdata <- tidytuesdayR::tt_load(2020, week = 33)

avatar_raw <- tuesdata$avatar %>%
    #make charaters a factor
    mutate(character = as.factor(character)) %>%
    #group for summarising
    select(book, chapter, chapter_num, character) %>%
    #tally number of times a character speaks per episode
    #remove scene discription
    filter(character != "Scene Description") %>%
    #cod elevels for books/seasons
    mutate(book = factor(book, levels = c("Water", 
                                          "Earth", 
                                          "Fire")),
           character = case_when(str_detect(character, "Toph")  ~ "Toph",
                                 str_detect(character, "Aang")  ~ "Aang",
                                 str_detect(character, "Sokka")  ~ "Sokka",
                                 str_detect(character, "Katara")  ~ "Katara",
                                 str_detect(character, "Iroh")  ~ "Iroh",
                                 str_detect(character, "Ozai")  ~ "Ozai",
                                 str_detect(character, "Azula")  ~ "Azula",
                                 str_detect(character, "Zuko")  ~ "Zuko",)                                      ) %>%
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
    pivot_longer(cols = -character_group) %>%
    mutate(n_jittered = jitter(value, factor= 0.3),
           name = as.numeric(name)) %>%
    filter(character_group %in% c("Aang",
                                  "Sokka",
                                  "Katara",
                                  "Zuko",
                                  "Iroh",
                                  "Ozai",
                                  "Azula",
                                  "Toph"))

#streamplot
#plot <- 
ggplot(data = avatar,
       aes(x = name,
           y = value,
           fill = character_group
       )) +
    geom_stream(
        geom = "contour",
        color = "white",
        size = 1.25,
        bw = .1
    ) +
    geom_stream(
        geom = "polygon",
        #n_grid = 12000,
        bw = .1,
        size = 0,
        alpha = 0.9) +
    geom_segment(aes(x = 1,
                     xend = 20,
                     y = 95,
                     yend = 95),
                 colour = "#0047AB") +
    geom_segment(aes(x = 21,
                     xend = 40,
                     y = 95,
                     yend = 95),
                 colour = "#7A5C12")+
    geom_segment(aes(x = 41,
                     xend = 61,
                     y = 95,
                     yend = 95),
                 colour = "#A10000") +
    annotate(
        geom = "text",
        x = 10,
        y = 100,
        label = 'Book 1 - "Water"',
        family = "Herculanum",
        size = 7,
        fontface = 2,
        colour = "#0047AB") +
    annotate(
        geom = "text",
        x = 30,
        y = 100,
        label = 'Book 2 - "Earth"',
        family = "Herculanum",
        size = 7,
        fontface = 2,
        colour = "#7A5C12") +
    annotate(
        geom = "text",
        x = 50,
        y = 100,
        label = 'Book 3 - "Fire"',
        family = "Herculanum",
        size = 7,
        fontface = 2,
        colour = "#A10000") +            
    theme_avatar(text.font = "Herculanum") +
    scale_fill_manual(name = "Main \nCharacter",
                      values = c("Aang" = "#87AFD1",
                                 "Sokka" = "#1DB4D3",
                                 "Katara" = "#174D79",
                                 "Zuko" = "#A10000",
                                 "Iroh" = "#572530",
                                 "Ozai" = "#FF4500",
                                 "Azula" = "#ECB100",
                                 "Toph" = "#015E05")) +
    labs(y = "",
         x = "Chapter",
         title = "Avatar: The Last Word",
         caption = "Data from {appa} (https://github.com/averyrobbins1/appa) and  https://avatar.fandom.com/wiki/Avatar_Wiki\n Visualisation and illustrations by @TanyaS_08",
         subtitle = "<p>Over the 61 episodes there were 9 992 spoken instances<br/>  spread across 363 different characters.</p><br/> 
        <p>Of the 8 main characters <b style='color:#87AFD1'>Aang</b> is the most chatty<br/>with 1 819 dialogue instances.<br/>
        <b style='color:#FF4500'>Ozai</b> says the least, having only 64 dialogue instances.</p><br/>
        <p>The chapters indicated on the horizontal axis indicate when a <br/>character had their most dialogue instances.</p>") +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_markdown(size = 12),
          axis.ticks = element_blank(),
          axis.title.x = element_text(size = 17),
          plot.title = element_text(size = 36),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.background = element_rect(fill = NA,
                                           color = NA),
          plot.subtitle = element_markdown(size = 14)) +
    scale_x_continuous(breaks = c(6, 12, 14, 45, 47, 51, 53, 54),
                       labels = c("Ch.6<br/><b style='color:#174D79'>Katara</b>",
                                  "<br/><br/>Ch.12<br/><b style='color:#572530'>Iroh</b>",
                                  "Ch.14<br/><b style='color:#87AFD1'>Aang</b>",
                                  "<br/><br/>Ch.45<br/><b style='color:#ECB100'>Azula</b>",
                                  "Ch.57<br/><b style='color:#015E05'>Toph</b>",
                                  "<br/><br/>Ch.51<br/><b style='color:#FF4500'>Ozai</b>",
                                  "Ch.53<br/><b style='color:#A10000'>Zuko</b>",
                                  "<br/><br/>Ch.54<br/><b style='color:#1DB4D3'>Sokka</b>"))


ggsave("2020/week33_Avatar/LastWord.png", 
       plot, 
       height = 8.3, width = 15, 
       units = "in", dpi = 600)

# End of script ----

avatar_raw %>%
    mutate(character = case_when(str_detect(character, "Toph")  ~ "Toph",
                                 str_detect(character, "Aang")  ~ "Aang",
                                 str_detect(character, "Sokka")  ~ "Sokka",
                                 str_detect(character, "Katara")  ~ "Katara",
                                 str_detect(character, "Iroh")  ~ "Iroh",
                                 str_detect(character, "Ozai")  ~ "Ozai",
                                 str_detect(character, "Azula")  ~ "Azula",
                                 str_detect(character, "Zuko")  ~ "Zuko",)) %>%
    na.omit()  %>% 
    filter(chapter == "The Fortuneteller")

x <- avatar_raw %>%
    mutate(character = case_when(str_detect(character, "Toph")  ~ "Toph",
                                 str_detect(character, "Aang")  ~ "Aang",
                                 str_detect(character, "Sokka")  ~ "Sokka",
                                 str_detect(character, "Katara")  ~ "Katara",
                                 str_detect(character, "Iroh")  ~ "Iroh",
                                 str_detect(character, "Ozai")  ~ "Ozai",
                                 str_detect(character, "Azula")  ~ "Azula",
                                 str_detect(character, "Zuko")  ~ "Zuko",)) %>%
    na.omit()  %>% 
    group_by(chapter, character) %>%
    tally()
    #most per episode by charcater
    #Aang      The Fortuneteller           73
    
    #most per episode
    #The Ember Island Players                  181
    
    #least per episode
    # Appa's Lost Days                              14
    
    