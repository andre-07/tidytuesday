##'*Tidy Tuesday Week 40 - Beyoncé and Taylor Swift Lyrics*
## Data Source : Rosie Baillie & Dr. Sara Stoudt

gc()
rm(list = ls())

## prerequisites
library(tidyverse)
library(cowplot)
library(ggtext)

## week 40 data
tuesdata <- tidytuesdayR::tt_load(2020, week = 40)

sales <- tuesdata$sales

# DATA PROCESSING ---------------------------------------------------------
sales_tswift <-
  sales %>% 
  filter(!is.na(sales),
         artist == "Taylor Swift",
         country == "US") %>% 
  mutate(
    title = fct_reorder(title, sales),
    sales = round(sales/1e6, 2)
  )

sales_beyonce <-
  sales %>% 
  filter(!is.na(sales),
         artist == "Beyoncé",
         country == "US") %>% 
  mutate(
    title = fct_reorder(title, sales),
    sales = round(sales/1e6, 2)
  )

## convert `released` to date format
sales2 <- sales %>%
  filter(
    !is.na(sales),
    country == "US"
  ) %>% 
  mutate(
    released = str_replace_all(string = released,
                               pattern = c('\\(UK\\)' = "",
                                           '\\(US\\)' = "", 
                                           "\\[39\\]" = "", 
                                           "\\[51\\]" = "")) %>% 
      trimws() %>% 
      as.Date(format='%B %d, %Y'),
    sales = round(sales/1e6, 2)
  )

# PLOT --------------------------------------------------------------------
## Setting the default plot theme
## You can always modify the default plot theme by adding
## theme() on your ggplot2 object
background <- "#FFDAB9"
text_colour <- "#D2691E"
plot_colour <- "black"
family <- "mono"
theme_style <- theme(rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(face = 'bold', size = 20, colour = text_colour, family = family),
                     plot.subtitle = element_text(size = 12, colour = text_colour, family = family),
                     plot.caption = element_text(size = 10, colour = text_colour),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.text.y = element_text(colour = text_colour, family = family),
                     axis.text.x = element_text(colour = text_colour),
                     axis.title.x = element_text(size = 12, family = family),
                     axis.title.y = element_text(size = 12, family = family),
                     axis.ticks = element_blank(),
                     axis.line = element_blank(),
                     legend.text = element_text(colour = text_colour, family = family),
                     strip.text = element_text(size = 12))

theme_set(theme_classic() + theme_style)

## Sales per Album - T-Swift
p1 <- 
  ggplot(sales_tswift, 
         aes(x = title, 
             y = sales, 
             label = sales)) + 
  geom_point(size = 7) + 
  geom_segment(aes(x = title, 
                   xend = title, 
                   y = 0, 
                   yend = sales), size = .5) +
  geom_text(color = "white", size = 2.5) + 
  labs(title = "Taylor Swift", 
       subtitle = "Sales per Album in the US",
       y = "Sales (in Million USD)") + 
  coord_flip() +
  theme(axis.title.y = element_blank())

## Sales per Album - Bey
p2 <- 
  ggplot(sales_beyonce, 
         aes(x = title, 
             y = sales,
             label = sales)) + 
  geom_point(size = 7) + 
  geom_segment(aes(x = title, 
                   xend = title, 
                   y = 0, 
                   yend = sales), size = .5) +
  geom_text(color = "white", size = 2.5) +
  labs(title="Beyoncé", 
       subtitle="Sales per Album in the US",
       y = "Sales (in Million USD)") +
  coord_flip() +
  theme(axis.title.y = element_blank())

## Sales through the years - T-Swift
p3 <- 
  ggplot(data = filter(sales2,
                       artist == "Taylor Swift"), 
         aes(released, sales)) +
  geom_line(color = "#BC8F8F") +
  geom_point(color = "#BC8F8F", size = 1.7) +
  xlim(as.Date("2005-06-01"),as.Date("2020-06-01")) +
  ylim(-1, 8) +
  annotate(
    geom = "text", x = as.Date("2007-06-24"), 
    y = 5.4, label = "Taylor Swift", hjust = "right",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2009-09-01"), 
    y = 7.8, label = "Fearless", hjust = "right",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2010-10-25"), 
    y = 4.3, label = "Speak Now", hjust = "right",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2012-12-22"), 
    y = 4.1, label = "Red", hjust = "right",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2015-01-30"), 
    y = 6.7, label = "1989", hjust = "right",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2017-11-10"), 
    y = 2.7, label = "Reputation", hjust = "left",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2019-04-23"), 
    y = 0.8, label = "Lover", hjust = "left",
    size = 3, family = family
  ) +
  labs(title = "Through the Years",
       subtitle = "T-Swift's Albums",
       x = "Date Released",
       y = "Sales (in Million USD)",
       caption = "T-Swift's latest album *Folklore* is excluded<br>Country : US") +
  theme(plot.caption = element_markdown(lineheight = 1.2, size = 8.5))

## Sales through the years - Bey
p4 <- 
  ggplot(data = filter(sales2,
                       artist == "Beyoncé"), 
         aes(released, sales)) +
  geom_line(color = "#BC8F8F") +
  geom_point(color = "#BC8F8F", size = 1.7) +
  xlim(as.Date("2002-06-23"),as.Date("2017-04-23")) +
  ylim(0.5, 6) +
  annotate(
    geom = "text", x = as.Date("2002-06-23"), 
    y = 5.5, label = "Dangerously in Love", hjust = "left",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2006-09-01"), 
    y = 3.47, label = "B'Day", hjust = "right",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2008-05-14"), 
    y = 3.69, label = "I Am... Sasha Fierce", hjust = "left",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2011-07-24"), 
    y = 1.3, label = "4", hjust = "right",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2013-05-13"), 
    y = 2.77, label = "Beyoncé", hjust = "left",
    size = 3, family = family
  ) +
  annotate(
    geom = "text", x = as.Date("2015-08-23"), 
    y = 1.4, label = "Lemonade", hjust = "left",
    size = 3, family = family
  ) +
  labs(title = "Through the Years",
       subtitle = "Bey's Albums",
       x = "Date Released",
       y = "Sales (in Million USD)",
       caption = "Country : US<br><br><br>Viz: @andredeveraa | Source: Rosie Baillie & Dr. Sara Stoudt") +
  theme(plot.caption = element_markdown(lineheight = 1.2, size = 8.5))

## Assemble
plot_grid(p1, p2,
          p3, p4,
          ncol = 2, nrow = 2)

ggsave("TSwift_Bey.png", width = 18, height = 10, dpi = 300)

##'*End*

































