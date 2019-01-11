# Investigate Primary Countries of Origin for Tourists to Indonesia -------

# Load libraries ----------------------------------------------------------
library(scales)
library(stringr)
library(lubridate)
library(tidyquant)
library(glue)
library(janitor)
library(tidyverse)
library(rvest)
library(gganimate)

# Scrape data from Wikipedia using rvest
wiki_page <- "https://en.wikipedia.org/wiki/Tourism_in_Indonesia"

# 2000-2010
arrivals_1 <- wiki_page %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE)

# 2011-2017
arrivals_2 <- wiki_page %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table(fill = TRUE)

# Join data for complete data
arrivals <- left_join(arrivals_1, arrivals_2, by = "Country")

# Wrangle Data ------------------------------------------------------------

arrivals_clean <- arrivals_1 %>%
  left_join(arrivals_2, by = "Country") %>% 
  mutate(Rank = row_number()) %>% 
  filter(Rank <= 10) %>%
  gather(key = year, -c(Country, Rank), value = arrivals) %>% 
  clean_names() %>% 
  mutate(country = as_factor(country),
         arrivals = parse_number(arrivals),
         arrivals_trunc = paste(format(round(arrivals / 1e6, 2), trim = TRUE), 'M'),
         year = paste(year, "-01-01") %>% as_date() %>% year()) %>% 
  filter(rank <= 5) 

# Clean up environment
remove(arrivals_1, arrivals_2)

# Plot Arrivals by Country of Origin --------------------------------------
arrivals_clean %>% 
  ggplot(aes(x = year, y = arrivals, colour = country, group = country)) + 
  geom_line(size = .8) + 
  geom_segment(aes(xend = 2017, yend = arrivals),
               linetype = "dashed", colour = 'grey') +
  geom_point(size = 2) +
  scale_color_tq() +
  scale_y_continuous(labels = scales::comma) +
  geom_text(data = arrivals_clean,
            show.legend = FALSE,
            x = 2019, 
            aes(label = country)) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank()) +
  labs(title = "Indonesian Inbound Tourism",
       subtitle = "Top 5 countries by arrivals, 2000 - 2017", 
       caption = glue("Source: {wiki_page}"),
       x = "", 
       y = "Arrivals") +
  expand_limits(x = 2020) +
  transition_reveal(year) +
  coord_cartesian(clip = 'off') 

ggsave("00_Output/plot_tourist-arrivals-Indonesia.png", dpi = 320)
