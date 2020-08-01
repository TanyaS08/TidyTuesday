#' ------------------------------------------------------------------#
#'  UNCANNY X-MEN
#'  - Data and README can be found at:
#'    https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-06-30/readme.md
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidytuesdayR)
library(tidyverse)
library(tidylog)

#todays prompts
options(prompt = "\U1F5EF",
        continue = "\U1F4D7")

### >> b) Dataset ----

#all datasets
tuesdata <- tidytuesdayR::tt_load(2020, week = 27)

#individuaL datasets
comic_bechdel <- tuesdata$comic_bechdel %>%
  #turn into factor
  mutate(pass_bechdel = as.factor(pass_bechdel))

xmen_bechdel <- tuesdata$xmen_bechdel %>%
  #turn into factor
  mutate(pass_bechdel = as.factor(pass_bechdel),
         #add writer = Chris Claremont
         writer = rep("Chris Claremont", nrow(.)),
         #add series = X-Men
         series = rep("X-Men", nrow(.)))

character_visualization <- tuesdata$character_visualization

characters <- tuesdata$characters %>%
  #select only count (numeric) categories
  select(character, rendered_unconcious, captured, declared_dead, redressed, depowered, clothing_torn,
         subject_to_torture, quits_team, surrenders, number_of_kills_humans, number_of_kills_non_humans,
         expresses_reluctance_to_fight, shower_number_of_panels_shower_lasts, bath_number_of_panels_bath_lasts,
         depicted_eating_food, visible_tears_number_of_panels, visible_tears_number_of_intances) %>%
  group_by(character) %>%
  summarise_all(sum)
  
covers <- tuesdata$covers
issue_collaborators <- tuesdata$issue_collaborators
locations <- tuesdata$locations


#data from previous TT for character info
becheldata <- tidytuesdayR::tt_load(2018, week = 9)
comic_characters <- becheldata$week9_comic_characters

### 1) Comic Bechdel ----

bechdel <-
union_all(xmen_bechdel,
      comic_bechdel) %>%
  mutate_at(vars(writer, series),
            as.factor) %>%
  arrange(pass_bechdel) %>%
  mutate(
    index = row_number()
         ) %>%
  select(pass_bechdel, writer, series, index) %>%
  na.omit()

ggplot() +
  geom_dotplot(data = bechdel,
               aes(y = pass_bechdel,
                   x = writer))

ggplot(bechdel, 
       aes(series, 
           1, 
           group = index, 
           fill = pass_bechdel)) +
  geom_bar(stat = 'identity',
           color = "#1b1f2b",
           size = 0.1) +
  scale_fill_brewer(palette = "Dark2",
                    guide = guide_legend(direction = "vertical", nrow = 2),
                    labels = c("Fail", "Pass")) +
  theme(panel.background = element_rect("#1b1f2b"),
        panel.grid.major = element_blank(),
        axis.title.y = element_blank(),
        #legend.position = c(0.90, 0.70),
        legend.text = element_text(size = 10),
        rect = element_rect("#1b1f2b"),
        text = element_text(colour = "#888c97"),
        plot.title = element_text(size= 22,
                                  hjust= 0.5,
                                  margin = margin(b = -0.1, t = 1, l = 2, unit = "cm")),
        plot.caption = element_text(hjust= 1,
                                    size= 6))

# End of script ----