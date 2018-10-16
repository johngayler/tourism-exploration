# Investigate Primary Countries of Origin for Tourists to Indonesia -------

# Load libraries ----------------------------------------------------------
source("00_Scripts/helpers.R")

# Scrape data from Wikipedia using rvest
wiki_page <- "https://en.wikipedia.org/wiki/Tourism_in_Indonesia"

arrivals <- wiki_page %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill = TRUE)

# Wrangle Data ------------------------------------------------------------

arrivals_clean <- arrivals %>% 
  filter(Rank <= 10) %>% 
  select(-Rank) %>% 
  gather(key = year, -Country, value = arrivals) %>% 
  clean_names() %>% 
  mutate(country = as_factor(country),
         arrivals = parse_number(arrivals),
         arrivals_trunc = paste(format(round(arrivals / 1e6, 2), trim = TRUE), 'M'))

# Plot Arrivals by Country of Origin --------------------------------------
arrivals_clean %>% 
  ggplot(aes(x = year, y = arrivals, colour = country, group = country)) + 
  geom_line(size = .8) + 
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  labs(title = "Tourist Arrivals to Indonesia",
       subtitle = "Top 10 countries - total visitation", 
       caption = glue("Source: {wiki_page}")) +
  geom_text_repel(data = arrivals_clean[arrivals_clean$year %in% c("2017"), ],
                  show.legend = FALSE,
                  colour = "black",
                  direction = "y",
                  segment.alpha = 0,
                  nudge_x = .3, 
                  size = 3.3,
                  aes(label = arrivals_trunc))


ggsave("00_Output/plot_tourist-arrivals-Indonesia.png", dpi = 320)
