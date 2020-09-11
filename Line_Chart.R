# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(tidytext)
library(gt)
library(paletteer)
library(ggtext)

windowsFonts(`Roboto Condensed` = windowsFont("Roboto Condensed"))


# Loading Data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-09-08')

friends <- tuesdata$friends
friends_info <- tuesdata$friends_info
friends_emotions <- tuesdata$friends_emotions


full_data <- friends_emotions %>% left_join(friends)


graph_data <- full_data %>% filter(speaker %in% c("Ross Geller","Joey Tribbiani","Chandler Bing",
                                                          "Rachel Green","Phoebe Buffay", "Monica Geller")) %>% 
  count(season, speaker,emotion,na.rm=T) %>% group_by(season, speaker) %>% mutate(total=sum(n)) %>% ungroup() %>% 
  mutate(percent=n/total*100) %>% 
  separate(col = "speaker", sep = " ", into=c("speaker","lastname")) %>% select(-lastname)

ggplot(graph_data,aes(x=season,y=percent)) +
  geom_line(aes(color=speaker),lwd=1.25) +
  facet_wrap(~emotion) +
  labs(title="Through the seasons, the emotions of characters evolved along similar lines",
       subtitle = "Share of lines in classified with a given emotion by season",
       x="Season") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title = element_markdown(face = "bold", size = rel(1.5)),
        plot.subtitle = element_text(face = "plain", size = rel(1.1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        strip.text = element_text(size = rel(1), face="bold"),
        plot.title.position = "plot", 
        axis.title.y = element_blank(),
        axis.text = element_text(siz=rel(1)),
        legend.position = "bottom")
