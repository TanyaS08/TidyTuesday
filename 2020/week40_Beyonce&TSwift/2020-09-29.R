#' ------------------------------------------------------------------#
#' BEYONCE AND TAYLOR SWIFT LYRICS
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
library(reshape2)
library(ggplot2)
library(geometry)
library(Rtsne)
library(rsvd)
library(syuzhet)
library(xml2)
library(rvest) #for webscraping
library(sysfonts)
library(here)


#todays prompts
options(prompt = c("\U1F399"),
        continue = "\U1F3C6")

#fonts

font_add_google("JetBrains Mono",
                "JetBrains",
                regular.wt = 700)
font_add_google("Big Shoulders Display",
                "BigShoulders")
font_paths()  
font_files()
font_families()
trace(grDevices::png, exit = quote({
  showtext::showtext_begin()
}), print = FALSE)

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

lyrics <- 
    beyonce_lyrics %>%
    unnest_tokens(word, line)%>% 
    #anti_join(stop_words, by=c("word"="word")) %>%
    right_join(Tracklist,
              by = c("song_name"="value")) %>%
    pull(word)

emotions <- get_nrc_sentiment((lyrics)) %>%
    select(-c(negative, positive)) %>%
    mutate(neutral = ifelse(rowSums(.) == 0,
                            1, 0))

lyrics_nrc <- cbind(beyonce_lyrics %>%
                        unnest_tokens(word, line) %>% 
                        #anti_join(stop_words, by=c("word"="word")) %>%
                        right_join(Tracklist,
                                   by = c("song_name"="value")) , emotions) %>%
    pivot_longer(cols = c(9:17),
                 names_to = "sentiment",
                 values_to = "value") %>%
    filter(value > 0) %>%
    arrange(Album)  %>%
    mutate(id = row_number()) %>%
    group_by(Album) %>%
    mutate(time = scale(id)) %>%
    mutate(time = scale(id),
           sentiment = ifelse(sentiment == "neutral",
                              NA, sentiment)) %>%
    na.omit()

levels(lyrics_nrc$Album) <- c("low", "high")

data <- sentiments %>%
    left_join(Tracklist,
          by = c("song_name"="value")) %>%
    arrange(Album)  %>%
    mutate(id = row_number()) %>%
    filter(Album != is.na(Album)) %>%
    group_by(Album) %>%
    na.omit()

unique(lyrics_nrc$Album)

summary(as.factor(lyrics_nrc$Album))
lyrics_nrc$Album <- factor(lyrics_nrc$Album, 
                           levels = c("Lemonade", "Beyoncé", 
                                      "4", "I Am... Sasha Fierce",
                                      "B'Day", "Dangerously in Love"))


### >> c) Plotting ----

sentiment_plot = 
ggplot(lyrics_nrc) +
  geom_linerange(aes(ymin = as.numeric(as.factor(Album)) - 0.2,
                     ymax = as.numeric(as.factor(Album)) + 0.2,
                     x = time,
                     colour = sentiment),
                 size = 0.1) +
  labs(x = NULL,
       caption = "visualisation by @TanyaS08",
       title = "BEYONCÉ") +
  scale_y_continuous(
    breaks = c(1, 2, 3, 4, 5, 6),
    label = c("Lemonade (2016)", "Beyoncé (2013)", 
              "4 (2011)", "I Am... Sasha Fierce (2008)",
              "B'Day (2006)", "Dangerously in Love (2003)")) +
  scale_colour_manual(values = c("#F74A5F", 
                               "#F59357",
                               "#02A0A8",
                               "#80699C",
                               "#F5D84D",
                               "#83D2F6",
                               "#DA73DE",
                               "#67FFB2")) +
  guides(colour = guide_legend(override.aes = list(size = 10),
                               nrow = 1)) +
  theme(panel.background = element_rect(fill = "#02010b"),
        plot.background = element_rect(fill = "#02010b"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "#02010b"),
        legend.position = "bottom",
        legend.key = element_rect(fill = "#02010b"),
        legend.text = element_text(size = 12,
                                   colour = "#d8aec8",
                                   family = "BigShoulders"),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 12,
                                   colour = "#d8aec8",
                                   family = "BigShoulders"),
        plot.title = element_text(size = 40,
                                  colour = "#d8aec8",
                                  family = "BigShoulders",
                                  hjust = 0.5,
                                  face = 'bold'),
        plot.caption = element_text(size = 10,
                                    colour = "#d8aec8",
                                    family = "BigShoulders"))

ggsave(here("2020/week40_Beyonce&TSwift/BeyonceSentiment.png"), 
       sentiment_plot, 
       height = 8.3, width = 15, 
       units = "in", dpi = 600)

ggplot(data) + 
  geom_bar(aes(y = Album,
               group = id,
               fill = sentiment),
           alpha = 0.9,
           width = 0.3) +
  scale_fill_discrete(na.value="transparent") +
  theme(panel.background = element_rect(fill = "#02010b"),
        plot.background = element_rect(fill = "#02010b"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "#02010b"),
        legend.position = "bottom",
        legend.key = element_rect(fill = "#02010b"),
        legend.text = element_text(size = 12,
                                   colour = "grey50"),
        axis.text.y = element_text(size = 12,
                                   colour = "grey50"),
        plot.title = element_text(size = 20,
                                  colour = "grey50"),
        plot.caption = element_text(size = 10,
                                    colour = "grey50"))
  

###End of script###