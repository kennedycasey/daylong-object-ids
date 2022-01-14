library(tidyverse)

annotations <- read_csv("data/annotations_20220106.csv")
participants <- read_csv("data/metadata/participants.csv")
durations <- read_csv("data/metadata/timestamps.csv") %>%
  arrange(sub_num, TimestampHMS) %>%
  group_by(sub_num) %>%
  mutate(first = first(Image), 
         last = last(Image)) %>%
  ungroup() %>%
  mutate(next_timestamp = lead(TimestampHMS, 1), 
         duration = ifelse(Image == first | Image == last, NA, 
                           as.numeric(next_timestamp - TimestampHMS))) %>%
  select(sub_num, Image, TimestampHMS, duration)

raw.data <- annotations %>%
  pivot_longer(c(None:Unsure), names_to = "exclusion", values_to = "exclusion.val") %>%
  mutate(exclusion = ifelse(exclusion.val == 1, exclusion, NA)) %>%
  pivot_longer(StudyRelated:OtherNatural, names_to = "coded.category", 
               values_to = "category.val") %>%
  filter(category.val != 0) %>%
  select(sub_num, Image, Object, exclusion, coded.category) %>%
  distinct()
         
# replace unsure objects --------------------------------------------------
unsure <- read_csv("data/manual-checks/unsure-objects.csv") %>%
  pivot_longer(c(Exclude:Unsure), names_to = "exclusion", values_to = "exclusion.val") %>%
  mutate(exclusion.corrected = ifelse(exclusion.val == 1, exclusion, NA)) %>%
  rename(object.corrected = Object, 
         coded.category.corrected = Category) %>%
  # TO DO: decide whether we're keeping categories when exact objects aren't identifiable
  select(sub_num, Image, object.corrected, exclusion.corrected, coded.category.corrected) 

data.w.unsure <- raw.data %>%
  left_join(unsure, by = c("sub_num", "Image")) %>%
  mutate(Object = ifelse(!is.na(object.corrected), object.corrected, Object), 
         exclusion = ifelse(!is.na(exclusion.corrected), exclusion.corrected, exclusion), 
         coded.category = ifelse(!is.na(coded.category.corrected), coded.category.corrected, coded.category)) %>%
  select(-ends_with("corrected"))


# replace originally excluded images --------------------------------------


# add regularized labels --------------------------------------------------


# add regularized categories ----------------------------------------------


# export clean data -------------------------------------------------------
data <- data.w.unsure %>%
  left_join(participants, by = "sub_num") %>%
  left_join(durations, by = c("sub_num", "Image")) %>%
  # TO DO: move above to regularized labels section
  mutate(n_objects = str_count(Object, ",") + 1) %>%
  separate(Object, c("object1", "object2", "object3"), ",") %>%
  mutate(across(starts_with("object"), ~ trimws(.))) %>%
  pivot_longer(starts_with("object"), names_to = "object_num", values_to = "object") %>%
  filter(!is.na(object)) %>%
  # TO DO: move to categories section
  mutate(object_type = case_when(
    category == "Food" ~ "Food", 
    category %in% c("Toy", "Book") ~ "Toy", 
    category %in% c("Plant", "Animal", "OtherNatural") ~ "Other Natural Object", 
    category %in% c("ToolMealtime", "ToolWork") ~ "Tool", 
    category == "OtherLargeImmovable" ~ "Large Immovable", 
    category %in% c("OtherSynthetic", "Electronic", "Clothing") ~ "Other Synthetic Object")) %>%
  rename(image = Image, 
         timestamp = TimestampHMS) %>%
  select(site, sub_num, age, sex, image, category, object_type, object, timestamp, duration) %>%
  distinct()

#write_csv(data, "../data/usable.data_20220106.csv")