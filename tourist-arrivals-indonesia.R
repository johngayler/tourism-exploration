# Investigate Primary Countries of Origin for Tourists to Indonesia -------

# Load libraries ----------------------------------------------------------
source("00_Scripts/helpers.R")
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

arrivals <- left_join(arrivals_1, arrivals_2, by = "Country")


# Wrangle Data ------------------------------------------------------------

arrivals_clean <- arrivals_1 %>%
  left_join(arrivals_2, by = "Country") %>% 
  mutate(Rank = row_number()) %>% 
  filter(Rank <= 10) %>%
  select(-Rank) %>%
  gather(key = year, -Country, value = arrivals) %>% 
  clean_names() %>% 
  mutate(country = as_factor(country),
         arrivals = parse_number(arrivals),
         arrivals_trunc = paste(format(round(arrivals / 1e6, 2), trim = TRUE), 'M'),
         year = paste(year, "-01-01") %>% as_date() %>% year())

# Clean up environment
remove(arrivals_1, arrivals_2)

# Plot Arrivals by Country of Origin --------------------------------------
countries <- c("China", "Singapore", "Malaysia", "Australia", "Japan", "India")

arrivals_clean %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(x = year, y = arrivals, colour = country, group = country)) + 
  geom_line(size = .8) + 
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  scale_color_tq() +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  labs(title = "Tourist Arrivals to Indonesia",
       subtitle = "Top 10 countries - total visitation", 
       caption = glue("Source: {wiki_page}")) +
  expand_limits(x = 2011) +
  # geom_segment(aes(xend = 2011, yend = arrivals), linetype = "dashed", colour = 'grey') +
  geom_text(label = country)
  # coord_cartesian(clip = 'off') +


  geom_text(aes(x = as.Date("2019-03-31"), label = country), hjust = 0) +
  theme_minimal() +

  
  geom_text(data = arrivals_clean[arrivals_clean$year %in% c("2017"), ],
                  show.legend = FALSE,
                  colour = "black",
                  # direction = "y",
                  # segment.alpha = 0,
                  nudge_x = .3, 
                  size = 3.3,
                  aes(label = arrivals_trunc))



ggsave("00_Output/plot_tourist-arrivals-Indonesia.png", dpi = 320)
