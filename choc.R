library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2022-01-18')

chocolate <- tuesdata$chocolate

# most memorable characteristic

chocolate %>% 
  separate_rows(most_memorable_characteristics, sep = ",") %>% 
  mutate(cocoa_percent = str_remove(cocoa_percent, "\\%"),
         cocoa_percent = as.numeric(cocoa_percent),
         most_memorable_characteristics = str_trim(most_memorable_characteristics)) %>% 
  group_by(most_memorable_characteristics) %>% 
  summarise(n = n(),
            mean_rating = mean(rating),
            mean_cocoa = mean(cocoa_percent),
            corr = cor(rating, cocoa_percent)) %>% 
  filter(n > 40) %>% 
  mutate(most_memorable_characteristics = glue::glue("{most_memorable_characteristics} ({n})")) %>% 
  ggplot(aes(y = fct_reorder(most_memorable_characteristics, corr), x = corr)) +
  geom_point() +
  scale_x_continuous(limits = c(-0.6, 0.2)) +
  geom_vline(xintercept = 0, color = "#bb5f62") +
  labs(y = "",
       x = "",
       title = "Pearson correlation between rating and cocoa percent",
       subtitle = "n > 40")

scat_plot <- chocolate %>% 
  mutate(cocoa_percent = str_remove(cocoa_percent, "\\%"),
         cocoa_percent = as.numeric(cocoa_percent)) %>% 
  ggplot(aes(y = rating, x = cocoa_percent)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE)

ggExtra::ggMarginal(scat_plot, type = "histogram")
