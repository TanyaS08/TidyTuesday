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
library(ggforce)
library(sysfonts)
library(showtext)
library(tvthemes) #for avatar theme
library(rvest) #for webscraping
library(cowplot) #for adding images
library(ggstream) #for streamplots
library(extrafont)
library(ggtext)
library(magick)
library(grid)
library(png)

#todays prompts
options(prompt = "\U1F30A",
        continue = "\U1F525")

# import fonts for theme
import_avatar()
font_add(family = "Herculanum", regular = "/Users/tanyastrydom/Documents/TidyTuesday/2020/week33_Avatar/Herculanum-Regular.ttf")
showtext_auto()

trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

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
        #account for variations in names of main characters
        character = case_when(str_detect(character, "Toph")  ~ "Toph",
                                 str_detect(character, "Aang")  ~ "Aang",
                                 str_detect(character, "Sokka")  ~ "Sokka",
                                 str_detect(character, "Katara")  ~ "Katara",
                                 str_detect(character, "Iroh")  ~ "Iroh",
                                 str_detect(character, "Ozai")  ~ "Ozai",
                                 str_detect(character, "Azula")  ~ "Azula",
                                 str_detect(character, "Zuko")  ~ "Zuko",
                                 TRUE ~ as.character(character))) %>%
        #arrange chronologically
        arrange(book, chapter_num)

### 1) Webscraping to get tribe/kingdom data ----

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

### 2) COncatinate datasets ----

#Combine with avatar_raw df
avatar_interim <- 
    avatar_raw  %>% 
    left_join(.,
              CharacterList,
              by = c("character" = "value"))  %>% 
    #Toph is a special case
    mutate(Nation = case_when(character == "Toph" ~ "Earth Kingdom",
                              TRUE ~ Nation),
           #new charater_group for plotting - pulls out the main characters and assigns rest to their tribe 
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
    #get a tally for each group i.e chapter by character_group
    tally()  %>% 
    ungroup()  %>% 
    #set levels for chapters
    mutate(episode = as.factor(as.integer(factor(chapter_num)))) %>%
    arrange(episode)  %>% 
    #pivot wider to fill in zeros
    pivot_wider(id_cols = character_group, names_from = episode, values_from = n, values_fill = 0)  %>% 
    #pivor back to long form
    pivot_longer(cols = -character_group)  %>% 
    #convert 'anme' i.e. chapter to numeric
    mutate(name = as.numeric(name))
           
avatar <-   
    avatar_interim %>%
    filter(character_group %in% c("Aang",
                                  "Sokka",
                                  "Katara",
                                  "Zuko",
                                  "Iroh",
                                  "Ozai",
                                  "Azula",
                                  "Toph"))

#streamplot
plot <- 
ggplot(data = avatar,
       aes(x = name, #chapter number
           y = value, #tally of dialogue
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
        <p>Unsurprisingly <b style='color:#1DB4D3'>Sokka</b> has the last word the most often - closing out 15 chapters<br/>
        Although, <b style='color:#87AFD1'>Aang</b> has the most to say - having the most dialogue instances.<br/>
        <b style='color:#015E05'>Toph</b> has the final say the least - closing out only 2 chapters<br/>
        <b style='color:#FF4500'>Ozai</b> has the least to say over the three books</p><br/>
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
          plot.subtitle = element_markdown(size = 13)) +
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
       height = 10, width = 15, 
       units = "in", dpi = 600)

# End of script ----

#Bonus Stream plot with all kingdoms

avatar_nations <-
  avatar_interim %>% 
  group_by(character_group) %>% 
  slice(1:4) %>% 
  #add 'tails' for smoother start/finish
  mutate(
    name = c(
      min(avatar_interim$name) - 6, 
      min(avatar_interim$name) - 3,
      max(avatar_interim$name) + 3,
      max(avatar_interim$name) + 6
    ), 
    value = c(0, .001, .001, 0)
  )  %>% 
  #combine with dataset
  rbind(.,
        avatar_interim)

ggplot(data = avatar_nations,
       aes(x = name, #chapter number
           y = value, #tally of dialogue
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
    theme_avatar(text.font = "Herculanum")
    