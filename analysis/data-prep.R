library(tidyverse)

raw_data <- read_csv("../data/annotations_20211202.csv")
participants <- read_csv("../data/participants.csv")
durations <- read_csv("../data/timestamps.csv") %>%
  arrange(sub_num, TimestampHMS) %>%
  group_by(sub_num) %>%
  mutate(first = first(Image), 
         last = last(Image)) %>%
  ungroup() %>%
  mutate(next_timestamp = lead(TimestampHMS, 1), 
         duration = ifelse(Image == first | Image == last, NA, 
                           as.numeric(next_timestamp - TimestampHMS))) %>%
  select(sub_num, Image, duration)

# excluded counts
raw_data %>%
  filter(Exclude == 1 | None == 1 | StudyRelated == 1 | Unsure == 1) %>%
  select(Exclude, None, StudyRelated, Unsure) %>%
  pivot_longer(everything(), names_to = "reason", values_to = "exclusion") %>%
  filter(exclusion == 1) %>%
  group_by(reason) %>%
  summarize(n = n())

# data cleaning
data <- raw_data %>%
  left_join(participants, by = "sub_num") %>%
  left_join(durations, by = c("sub_num", "Image")) %>%
  filter(Exclude != 1 & None != 1 & StudyRelated != 1 & Unsure != 1) %>%
  pivot_longer(StudyRelated:OtherNatural, names_to = "category", 
               values_to = "category_value") %>%
  filter(category_value == 1) %>%
  mutate(object_type = case_when(
    category == "Food" ~ "Food", 
    category %in% c("Toy", "Book") ~ "Toy", 
    category %in% c("Plant", "Animal", "OtherNatural") ~ "Other Natural", 
    category %in% c("ToolMealtime", "ToolWork") ~ "Tool", 
    category %in% c("OtherSynthetic", "Electronic", "Clothing") ~ "Other Synthetic Object",  
    category == "OtherLargeImmovable" ~ "Other Large Immovable Object")) %>%
  mutate(n_objects = str_count(Object, ",") + 1) %>%
  separate(Object, c("object1", "object2", "object3"), ",") %>%
  mutate(across(starts_with("object"), ~ trimws(.))) %>%
  pivot_longer(starts_with("object"), names_to = "object_num", values_to = "object") %>%
  filter(!is.na(object)) %>%
  rename(image = Image) %>%
  select(site, sub_num, age, sex, image, category, object_type, object, duration) 

write_csv(data, "../data/usable.data_20211202.csv")

# coded counts 
raw_data %>%
  group_by(sub_num) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(mean = mean(n),
            min = min(n), 
            max = max(n), 
            n = length(unique(raw_data$sub_num)))

# included counts 
data %>%
  select(sub_num, image) %>%
  distinct() %>%
  group_by(sub_num) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  summarize(mean = mean(n),
            min = min(n), 
            max = max(n), 
            n = length(unique(raw_data$sub_num)))