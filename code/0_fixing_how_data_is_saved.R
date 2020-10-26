test <- read_csv(here("responses", "20201025-214908_661b12cd2f1b9a948d2e85d882d1cb6f.csv"))
test                 
t(test)

test
test %>% 
  mutate(newid = row_number()) %>%
  mutate_all(as.character) %>% 
  pivot_longer(names_to = "names", values_to = "value", cols = behavior_personal:timestamp) %>% 
  distinct() %>% 
  pivot_wider(names_from = "names", values_from = "value")



test %>% 
  pivot_longer(names_to = "names", values_to = "value", cols = -timestamp) %>% 
  distinct() %>%
  mutate(new_value = case_when(names == "behavior_mask" ~ "1", TRUE ~ value)) %>% 
  mutate(names = case_when(names == "behavior_mask" ~ paste0("Mask Behavior: ", value),
                           TRUE ~ names)) %>% 
  select(-value) %>% 
  rename(value = new_value) %>% 
  pivot_wider(names_from = "names", values_from = "value") %>% 
  clean_names() %>% 
  mutate_at(vars(matches("mask_behavior")), as.numeric)



test %>% 
  select(-timestamp) %>% 
  pivot_longer(names_to = "names", values_to = "value", cols = everything()) %>% 
  distinct() %>%
  mutate(new_value = case_when(names == "behavior_mask" ~ "1", TRUE ~ value)) %>% 
  mutate(names = case_when(names == "behavior_mask" ~ paste0("Mask Behavior: ", value),
                           TRUE ~ names)) %>% 
  select(-value) %>% 
  rename(value = new_value) %>% 
  pivot_wider(names_from = "names", values_from = "value") %>% 
  clean_names() %>% 
  mutate_at(vars(matches("mask_behavior")), as.numeric) %>% 
  bind_cols(tibble(timestamp = "test"))
  
