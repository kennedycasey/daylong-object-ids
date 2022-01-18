library(tidyverse)

# read in data
annotations <- read_csv("data/annotations_20220117.csv")
participants <- read_csv("data/metadata/participants.csv")

# calculate duration between images
durations <- read_csv("data/metadata/timestamps.csv") %>%
  arrange(sub_num, TimestampHMS) %>%
  group_by(sub_num) %>%
  mutate(first = first(Image), 
         last = last(Image)) %>%
  ungroup() %>%
  # store NA if it's the first or last image in a dir
  mutate(next_timestamp = lead(TimestampHMS, 1), 
         duration = ifelse(Image == first | Image == last, NA, 
                           as.numeric(next_timestamp - TimestampHMS))) %>%
  select(sub_num, Image, TimestampHMS, duration)

# create one column that tells us the reason for exclusion
raw.data <- annotations %>%
  pivot_longer(c(None:Unsure), names_to = "exclusion", values_to = "exclusion.val") %>%
  mutate(exclusion = ifelse(exclusion.val == 1, exclusion, NA)) %>%
  select(sub_num, Image, Object, exclusion) %>%
  distinct()
         
# replace unsure objects --------------------------------------------------
# read in manually checked unsure objects
unsure <- read_csv("data/manual-checks/unsure-objects.csv") %>%
  pivot_longer(c(Exclude:Unsure), names_to = "exclusion", values_to = "exclusion.val") %>%
  mutate(exclusion.corrected = ifelse(exclusion.val == 1, exclusion, NA), 
         from.unsure = 1) %>%
  # TO DO: decide whether we're keeping categories when exact objects aren't identifiable
  rename(object.corrected = Object) %>%
  select(sub_num, Image, object.corrected, exclusion.corrected, from.unsure) 

# merge with main annotations df and replace old values for corrected items
data.w.unsure <- raw.data %>%
  left_join(unsure, by = c("sub_num", "Image")) %>%
  mutate(Object = ifelse(!is.na(object.corrected) & !is.na(from.unsure), object.corrected, Object), 
         exclusion = ifelse(!is.na(from.unsure), exclusion.corrected, exclusion)) %>%
  select(-ends_with("corrected"))

# replace originally excluded images --------------------------------------
# read in manually checked images with no objects
none <- read_csv("data/manual-checks/no-objects.csv") %>%
  mutate(object.corrected = Object, 
         exclusion.corrected = ifelse(Exclude == 1, "Exclude", ifelse(None == 1, "None", NA)), 
         from.none = 1) %>%
  select(sub_num, Image, object.corrected, exclusion.corrected, from.none) 

# merge with main annotations df and replace if there was a held object
data.w.none <- data.w.unsure %>%
  full_join(none, by = c("sub_num", "Image")) %>%
  mutate(Object = ifelse(!is.na(object.corrected) & !is.na(from.none), object.corrected, Object), 
         exclusion = ifelse(!is.na(from.none), exclusion.corrected, exclusion)) %>%
  select(-ends_with("corrected"), -from.none)

# add regularized labels --------------------------------------------------
# read in manually checked object labels
labels <- read_csv("data/manual-checks/labels.csv")

# merge with main annotations df and replace raw label with regularized version
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
  mutate(object = trimws(object)) %>%
  select(sub_num, Image, exclusion, object)

# temporary code for pulling out new labels that need categorization
old_categories <- read_csv("data/manual-checks/categories.csv") %>%
  pull(object) %>%
  unique()

`%notin%` <- Negate(`%in%`)

#data.w.labels %>%
  #select(object) %>%
  #distinct() %>%
  #filter(object %notin% old_categories) %>%
  #write_csv("data/manual-checks/new-objects-to-categorize.csv")

# add regularized categories ----------------------------------------------
# read in manually checked categories for each object
categories <- read_csv("data/manual-checks/categories.csv")

# merge with main annotations df and store new category
data.w.categories <- data.w.labels %>%
  full_join(categories, by = "object") %>%
  select(sub_num, Image, exclusion, category, object) %>%
  distinct() 

# export clean data -------------------------------------------------------
# randomly select rec for kids with 2 recs -> hard code until we have all annotations
p1.exclude <- 7959 #output of sample(c(7959, 4856), 1)
p2.exclude <- 5499 #output of sample(c(4590, 5499), 1)

data.to.export <- data.w.categories %>%
  left_join(participants, by = "sub_num") %>%
  left_join(durations, by = c("sub_num", "Image")) %>%
  filter(sub_num != p1.exclude & sub_num != p2.exclude) %>%
  rename(image = Image, 
         timestamp = TimestampHMS) %>%
  select(site, sub_num, age, sex, image, exclusion, category, object, timestamp, duration) %>%
  distinct()

write_csv(data.to.export, "data/usable.data_20220117.csv")