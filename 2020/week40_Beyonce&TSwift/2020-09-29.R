#' ------------------------------------------------------------------#
#' HIMALAYAN CLIMBING EXPEDITIONS
#'  - Data and README can be found at:
#'  (https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-09-29)
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
library(tidytext)
library(tidyverse)
library(tidylog)
library(stm)
library(quanteda)
library(wordcloud)
library(reshape2)
library(ggplot2)
library(geometry)
library(Rtsne)
library(rsvd)
library(syuzhet)
library(xml2)
library(rvest) #for webscraping


#todays prompts
options(prompt = c("\U1F399"),
        continue = "\U1F3C6")

### >> b) Data ----
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

charts  %>% 
    filter(artist == "Beyoncé",
    chart == "US")

sales  %>% 
    filter(artist == "Beyoncé",
    country == "US")  %>% 
    select(formats)

data(stop_words)

top_words <- beyonce_lyrics %>%
   unnest_tokens(word, line) %>% 
   anti_join(stop_words, by=c("word"="word")) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>% 
    filter(word != "whoa") %>% 
    filter(word != "ready") %>%
    group_by(sentiment) %>%
    top_n(3) %>%
    ungroup()


beyonce_lyrics %>%
    unnest_tokens(word, line) %>%
    right_join(top_words, by=c("word"="word"))

sentiments <- beyonce_lyrics %>%
    unnest_tokens(word, line) %>% 
    anti_join(stop_words, by=c("word"="word")) %>%
    left_join(get_sentiments("bing"))


Tracklist <- read_html("https://beyonce.fandom.com/wiki/Dangerously_In_Love_(Album)") %>%
    #select nodes based on css style name
    html_nodes(c("#mw-content-text li a")) %>% 
    #convert to text
    html_text() %>% 
    as_tibble() %>%
    mutate(TrackNum = row_number(),
           Album = rep("Dangerously in Love", nrow(.))) %>%
    add_row(read_html("https://beyonce.fandom.com/wiki/I_Am..._Sasha_Fierce") %>%
    #select nodes based on css style name
    html_nodes(c("h2+ ol a")) %>% 
    #convert to text
    html_text() %>% 
    as_tibble() %>%
    mutate(TrackNum = row_number(),
           Album = rep("I Am... Sasha Fierce", nrow(.)))) %>%
    add_row(read_html("https://beyonce.fandom.com/wiki/B%27Day") %>%
    #select nodes based on css style name
    html_nodes(c("h2+ ol a:nth-child(1)")) %>% 
    #convert to text
    html_text() %>% 
    as_tibble() %>%
    mutate(TrackNum = row_number(),
           Album = rep("B'Day", nrow(.)))) %>%
    add_row(read_html("https://beyonce.fandom.com/wiki/Beyonc%C3%A9_(album)") %>%
    #select nodes based on css style name
    html_nodes(c("h2+ ol a:nth-child(1)")) %>% 
    #convert to text
    html_text() %>% 
    as_tibble() %>%
    mutate(TrackNum = row_number(),
           Album = rep("Beyoncé", nrow(.)))) %>%
    add_row(read_html("https://beyonce.fandom.com/wiki/4") %>%
    #select nodes based on css style name
    html_nodes(c("h2+ ol a")) %>% 
    #convert to text
    html_text() %>% 
    as_tibble() %>%
    mutate(TrackNum = row_number(),
           Album = rep("4", nrow(.)))) %>%
    add_row(read_html("https://beyonce.fandom.com/wiki/Lemonade") %>%
    #select nodes based on css style name
    html_nodes(c("h2+ ol a")) %>% 
    #convert to text
    html_text() %>% 
    as_tibble() %>%
    mutate(TrackNum = row_number(),
           Album = rep("Lemonade", nrow(.))))

data <- sentiments %>%
    left_join(Tracklist,
          by = c("song_name"="value")) %>%
    arrange(Album)  %>%
    mutate(id = row_number()) %>%
    filter(Album != is.na(Album)) %>%
    group_by(Album) %>%
    mutate(time = scale(id)) %>%
    na.omit()

ggplot(data) +
    geom_point(aes(y = Album,
                   x = time,
                   colour = sentiment),
               size = 0.1)





###End of script###