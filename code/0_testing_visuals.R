# Testing the visuals
# https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/
# https://stackoverflow.com/questions/29272550/geom-bar-colorize-bars-depending-on-column-in-data-frame

# packages ----------------------------------------------------------------

library(tidyverse)
library(here)

library(ggthemes)
library(scales)


# data load ---------------------------------------------------------------

temp <- list.files(path = here("responses"), pattern="*.csv", full.names = TRUE) # Worry about "full.names" when deployed to shinyapps
myfiles <- lapply(temp, read_csv)

dat <- myfiles %>% bind_rows()


test_4 <- dat %>% select(comfort_grocery, timestamp)

ggplot(test_4, aes(x = comfort_grocery)) +
  geom_histogram()

ggplot(test_4, aes(x = comfort_grocery, fill = test_4$comfort_grocery[which(test_4$timestamp == max(test_4$timestamp))])) +
  geom_bar(aes(y = (..count..)/sum(..count..))) 
  # geom_bar(data = test_4 %>% filter(timestamp == max(timestamp)), aes(x = comfort_grocery, y = (..count..)/sum(..count..), fill = "red"))

ggplot(test_4, aes(x = comfort_grocery, fill = as.factor(1))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) 


test <- tabyl(test_4, comfort_grocery) %>% 
  rename(choice = comfort_grocery) %>% 
  mutate(bar_color = case_when(choice == test_4 %>% filter(timestamp == max(timestamp)) %>% select(comfort_grocery) %>% as.numeric() ~ "#e88f00",
                               TRUE ~ "#124d47")) %>% 
  mutate(your_selection = case_when(choice == test_4 %>% filter(timestamp == max(timestamp)) %>% select(comfort_grocery) %>% as.numeric() ~ paste0(percent(percent, accuracy = 1), " (Your pick)"),
                                    TRUE ~ percent(percent, accuracy = 1)))


test_4 %>% filter(timestamp == max(timestamp)) %>% select(comfort_grocery) %>% as.numeric()
ggplot(data = test,
       aes(x = choice, 
           y = percent,
           fill = bar_color,
           color = bar_color,
           label = your_selection)) +
  geom_bar(stat = "identity") +
  geom_text(vjust = -1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(label = percent) +
  theme_fivethirtyeight() +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("Test")



# behavior ----------------------------------------------------------------
dat
test_b <- dat %>% select(timestamp, behavior_personal)
test_b

test_b_pct <- tabyl(test_b, behavior_personal) %>% 
  rename(choice = behavior_personal) %>% 
  mutate(bar_color = case_when(choice == test_b %>% filter(timestamp == max(timestamp)) %>% select(behavior_personal) %>% as.character() ~ "#e88f00",
                               TRUE ~ "#124d47")) %>% 
  mutate(your_selection = case_when(choice == test_b %>% filter(timestamp == max(timestamp)) %>% select(behavior_personal) %>% as.character() ~ paste0(percent(percent, accuracy = 1), " (Your pick)"),
                                    TRUE ~ percent(percent, accuracy = 1)))


ggplot(data = test_b_pct,
       aes(x = choice, 
           y = percent,
           fill = bar_color,
           color = bar_color,
           label = your_selection)) +
  geom_bar(stat = "identity") +
  geom_text(vjust = -1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(label = percent) +
  theme_fivethirtyeight() +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("Test")

