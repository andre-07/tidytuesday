##'*Tidy Tuesday 2019/07/30 - Video Games dataset*
## Data Source : Liza Wood via Steampy

## prerequisites
library(tidyverse)
library(showtext)
library(showtextdb)
library(ggtext)
library(glue)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

## explore
skimr::skim(video_games)

## DATA PROCESS ------------------------------------------------------------

## highest metascores 2004-2018
vid_games_data <- 
  video_games %>% 
  mutate(
    release_date = as.Date(release_date, format='%B %d, %Y'),
    release_year = lubridate::year(release_date)
  ) %>% 
  drop_na() %>% 
  select(-number, -release_date, -price) %>% 
  filter(
    median_playtime &
    average_playtime > 0
  ) %>% 
  group_by(release_year) %>% 
  slice(which.max(metascore)) %>%
  mutate(
    game = fct_reorder(game, release_year)
  ) %>% 
  arrange(desc(release_year))

# PLOT/TABLE --------------------------------------------------------------

## get Montserrat font
font_add_google("Montserrat", "Montserrat")
showtext_auto()
font_families()

## ggplot2 "table"
ggplot(vid_games_data) +
  ## bars
  geom_tile(aes(-18, game, height = 0.9, width = 60), fill = "white") +
  
  ## text rows
  geom_richtext(aes(-35, game, label = glue("**{game}**<br>*{developer}*"), family = "Montserrat"), size = 3.5, fill = NA, label.color = NA) +
  geom_richtext(aes(-15, game, label = owners, family = "Montserrat"), size = 3.5, fill = NA, label.color = NA) +
  geom_label(aes(-5, game, label = metascore, family = "Montserrat"), color = "white", fill = "#32CD32", fontface = "bold") +
  geom_point(aes(log(average_playtime), game), size = 3) +
  geom_segment(aes(x = 0, xend = log(average_playtime), y = game, yend = game)) +
  geom_richtext(aes(12.5, game, label = release_year, family = "Montserrat"), size = 3.5, fill = NA, label.color = NA) +
  
  ## column labels
  annotate("text", -35, 16, label = "GAME\nDeveloper", family = "Montserrat", color = "grey60") +
  annotate("text", -15, 16, label = "No. of Owners\n(Estimated)", family = "Montserrat", color = "grey60") +
  annotate("text", -5, 16, label = "Metascore", family = "Montserrat", color = "grey60") +
  annotate("text", 4.3, 16, label = "Avg. 2-Week\nPlaytime (Hrs)", family = "Montserrat", color = "grey60") +
  annotate("text", 12.5, 16, label = "Release\nYear", family = "Montserrat", color = "grey60") +
  
  ## lollipop labels
  annotate("text", 7.85, 14, label = "717", family = "Montserrat", color = "grey60", size = 2.5) +
  annotate("text", 7.80, 12, label = "697", family = "Montserrat", color = "grey60", size = 2.5) +
  annotate("text", 3.5, 3, label = "12", family = "Montserrat", color = "grey60", size = 2.5) +
  
  ## stars
  geom_text(data = subset(vid_games_data, metascore == max(vid_games_data$metascore)), 
            aes(-6.5, game, label = "★"), colour = "darkgoldenrod1", vjust = 0) +
  
  ## table lines
  geom_hline(
    yintercept = seq(0.5, 16.5, by = 1),
    size = 0.25, colour = "grey70"
  ) +
  
  ## theme
  coord_fixed(ratio = 3, clip = "off") +
  theme_void() +
  scale_x_continuous(limits = c(-50,17.5), expand = c(0, 0)) +
  labs(
    title = "Highest rated games from 2004 to 2018",
    caption = "#TidyTuesday 2019-07-30 | Graphic: Andre De Vera | Data Source: Liza Wood via Steampy (https://steamspy.com/year/)"
  ) +
  theme(
    legend.position = "none",
    plot.margin = margin(30,10,20,10),
    plot.title.position = "panel",
    plot.title = element_text(family = "Montserrat", hjust = 0.5, vjust = 6.8, face = "bold", margin = margin(0, 0, 5, 0)),
    plot.caption = element_text(family = "mono", hjust = 0.5, colour = "grey70")
  )

ggsave("vidgames.png", width = 18, height = 10, dpi = 300)
