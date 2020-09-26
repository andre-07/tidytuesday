##'*Tidy Tuesday Week 39 - Himalayan Climbing Expeditions*
## Data Source : The Himalayan Database

gc()
rm(list = ls())

## prerequisites
library(tidyverse)
library(ggbillboard)
library(palettetown)
library(cowplot)
library(ghibli)
library(grid)
library(png)

## week 39 data
tuesdata <- tidytuesdayR::tt_load(2020, week = 39)

members <- tuesdata$members
expeditions <- tuesdata$expeditions

# DATA --------------------------------------------------------------------

## Top 5 peaks by number of injured cases
top5_injured_ <- 
  members %>% 
  select(peak_id, injured) %>% 
  filter(!is.na(peak_id) &
           injured == TRUE) %>% 
  count(peak_id, sort = TRUE) %>% 
  top_n(n = 5, n)

top5_injured <-
  members %>% 
  select(peak_id, peak_name, injury_type) %>% 
  filter(peak_id %in% c(top5_injured_$peak_id),
         !is.na(injury_type)) %>% 
  group_by(peak_name, injury_type) %>% 
  summarise(Cases = n())

## Top 5 peaks by number of death cases
## pivot the data 
exp_pivot <- 
  expeditions %>% 
  select(year, expedition_id, peak_name, termination_reason, members, 
         member_deaths, hired_staff, hired_staff_deaths) %>% 
  pivot_longer(cols = 5:last_col(),
               names_to = "Category", 
               values_to = "Count")

top5 <- 
  exp_pivot %>% 
  filter(Category %in% c("member_deaths", "hired_staff_deaths"),
         !is.na(peak_name)) %>% 
  group_by(peak_name) %>%
  summarise(Deaths = sum(Count)) %>%
  top_n(n = 5, Deaths) %>% 
  arrange(desc(Deaths))

deaths <- 
  exp_pivot %>% 
  filter(Category %in% c("member_deaths", "hired_staff_deaths") &
           peak_name %in% c(top5$peak_name)) %>% 
  group_by(year, peak_name) %>%
  summarise(Deaths = sum(Count))

# number of climbers per peak
climbers <- 
  exp_pivot %>% 
  filter(Category %in% c("members", "hired_staff") &
           peak_name %in% c(top5$peak_name)) %>% 
  group_by(year, peak_name) %>%
  summarise(Climbers = sum(Count))

## join tables
death_climbers <- 
  climbers %>% 
  left_join(select(deaths, Deaths, peak_name), 
            by = c("year" = "year", 
                   "peak_name" = "peak_name"))

# PLOT --------------------------------------------------------------------

## Setting the plot theme
background <- "#3D3638"
text_colour <- "white"
plot_colour <- "black"
theme_style <- theme(rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(face = 'bold', size = 20, colour = text_colour),
                     plot.subtitle = element_text(size = 12, colour = text_colour),
                     plot.caption = element_text(size = 10, colour = text_colour),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.text.y = element_text(colour = text_colour),
                     axis.text.x = element_text(colour = text_colour),
                     axis.title.x = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     axis.ticks = element_blank(),
                     axis.line = element_blank(),
                     legend.text = element_text(colour = text_colour),
                     strip.text = element_text(size = 12))

theme_set(theme_classic() + theme_style)

p1 <- 
  ggplot(top5_injured, 
         aes(fct_reorder(peak_name, Cases), Cases, 
           fill = fct_reorder(injury_type, Cases))) +
  geom_col(position = "dodge") +
  scale_fill_poke(pokemon = "Beedrill", spread = 11) +
  coord_flip() +
  labs(title = "Top 5 Peaks by Number of Injuries",
       x = "Peak",
       y = "Number of Cases",
       fill = "Type of Injury") +
  theme(plot.title = element_text(size = 15, colour = "#F4A460", hjust = .06),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 11))

p2 <- 
  ggplot(death_climbers,
         aes(year, Deaths, 
             color = fct_reorder(peak_name, Deaths))) +
  geom_line() +
  scale_color_ghibli_d("MononokeMedium", direction = -1) +
  facet_wrap(~peak_name) +
  theme(plot.title = element_text(size = 15, colour = "#F4A460", hjust = .06),
        legend.position = "none") +
  labs(title = "Number of Deaths by Peaks",
       x = "Year",
       y = "Number of Deaths",
       subtitle = "Top 5 Peaks",
       caption = "Graphics: @andre-07 | Source: The Himalayan Database")

## For tidy tuesday logo
img <- png::readPNG("ttues_logo.png")
g1 <- grid::rasterGrob(img, interpolate=TRUE)

p3 <- billboard(p2, g1)

## Assemble
plot_grid(p1, p3, ncol = 1, nrow = 2)

ggsave("Himalayan.png", width = 18, height = 10, dpi = 300)

##'*End*
