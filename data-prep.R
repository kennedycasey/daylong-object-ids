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
none <- read_csv("data/manual-checks/no-objects.csv") %>%
  mutate(object.corrected = Object, 
         exclusion.corrected = ifelse(Exclude == 1, "Exclude", ifelse(None == 1, "None", NA)), 
         from.none = 1) %>%
  select(sub_num, Image, object.corrected, exclusion.corrected, from.none) 

data.w.none <- data.w.unsure %>%
  full_join(none, by = c("sub_num", "Image")) %>%
  mutate(Object = ifelse(!is.na(object.corrected) &  !is.na(from.none), object.corrected, Object), 
         exclusion = ifelse(!is.na(from.none), exclusion.corrected, exclusion)) %>%
  select(-ends_with("corrected"), -from.none)

# add regularized labels --------------------------------------------------
labels <- read_csv("data/manual-checks/labels.csv")

data.w.labels <- data.w.none %>%
  mutate(n.objects = str_count(Object, ",") + 1) %>%
  separate(Object, c("Object1", "Object2", "Object3"), ",") %>%
  mutate(across(starts_with("Object"), ~ trimws(.))) %>%
  pivot_longer(starts_with("Object"), names_to = "object.num", values_to = "Object") %>%
  filter(!is.na(Object)) %>%
  left_join(labels, by = "Object") %>%
  mutate(object = ifelse(is.na(object.corrected), Object, object.corrected)) %>%
  mutate(n.objects = str_count(object, ",") + 1) %>%
  separate(object, c("object1", "object2"), ",") %>%
  mutate(across(ends_with("1|2"), ~ trimws(.))) %>%
  pivot_longer(c("object1", "object2"), names_to = "object.num2", values_to = "object2") %>%
  filter(!is.na(object2)) %>%
  rename(object = object2) %>%
  select(sub_num, Image, exclusion, coded.category, object)

# add regularized categories ----------------------------------------------

# export clean data -------------------------------------------------------
data <- data.w.labels %>%
  left_join(participants, by = "sub_num") %>%
  left_join(durations, by = c("sub_num", "Image")) %>%
  # TO DO: move to categories section
  mutate(object_type = case_when(
    coded.category == "Food" ~ "Food", 
    coded.category %in% c("Toy", "Book") ~ "Toy", 
    coded.category %in% c("Plant", "Animal", "OtherNatural") ~ "Other Natural Object", 
    coded.category %in% c("ToolMealtime", "ToolWork") ~ "Tool", 
    coded.category == "OtherLargeImmovable" ~ "Large Immovable", 
    coded.category %in% c("OtherSynthetic", "Electronic", "Clothing") ~ "Other Synthetic Object")) %>%
  rename(image = Image, 
         timestamp = TimestampHMS) %>%
  select(site, sub_num, age, sex, image, exclusion, object_type, object, timestamp, duration) %>%
  distinct()

write_csv(data, "data/usable.data_20220114.csv")