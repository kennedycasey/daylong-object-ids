library(tidyverse)

# read in data
annotations <- read_csv("data/object-annotations.csv")
participants <- read_csv("data/metadata/participants.csv")
timestamps <- read_csv("data/metadata/timestamps.csv")

# create not in operator
`%notin%` <- Negate(`%in%`)

# create one column that tells us the reason for exclusion
raw.data <- annotations %>%
  mutate(Experimenter = ifelse(str_detect(tolower(Object), "exclude"), 1, 0)) %>%
  pivot_longer(c(None, Exclude, Unsure, Experimenter), 
               names_to = "exclusion", 
               values_to = "exclusion.val") %>%
  mutate(exclusion = ifelse(exclusion.val == 1, exclusion, NA)) %>%
  select(sub_num, Image, Object, exclusion) %>%
  distinct()

# get knife categories ----------------------------------------------------
knife.categories <- annotations %>%
  filter(str_detect(Object, "knife"))

# check to make sure there aren't any images with both categories
nrow(filter(knife.categories, ToolMealtime == 1 & ToolWork == 1))

m.knife.images <- knife.categories %>%
  filter(ToolMealtime == 1) %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  pull(exact.image)

w.knife.images <- knife.categories %>%
  filter(ToolWork == 1) %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  pull(exact.image)

# replace unsure objects --------------------------------------------------
# read in manually checked unsure objects
unsure <- read_csv("data/manual-checks/unsure-objects.csv") %>%
  pivot_longer(c(Exclude:Unsure), names_to = "exclusion", values_to = "exclusion.val") %>%
  mutate(exclusion.corrected = ifelse(exclusion.val == 1, exclusion, NA), 
         from.unsure = 1) %>%
  rename(object.corrected = Object) %>%
  select(sub_num, Image, object.corrected, exclusion.corrected, from.unsure) 

# merge with main annotations df and replace old values for corrected items
data.w.unsure <- raw.data %>%
  left_join(unsure, by = c("sub_num", "Image")) %>%
  mutate(Object = ifelse(!is.na(from.unsure), object.corrected, Object), 
         exclusion = ifelse(!is.na(from.unsure), exclusion.corrected, exclusion)) %>%
  select(-ends_with("corrected"), -from.unsure) %>%
  mutate(Object = ifelse(!is.na(exclusion), NA, Object)) %>%
  distinct()

checked.unsure <- read_csv("data/manual-checks/unsure-objects.csv") %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  pull(exact.image)

not.checked.unsure <- data.w.unsure %>%
  filter(exclusion == "Unsure") %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  filter(exact.image %notin% checked.unsure)
  
if (nrow(not.checked.unsure > 0)) {
  write_csv(not.checked.unsure, "data/manual-checks/new-unsure-objects.csv")
}

# replace immovable objects -----------------------------------------------
# read in manually checked immovable objects
immovable <- read_csv("data/manual-checks/immovable.csv") %>%
  mutate(from.immovable = 1) %>%
  select(sub_num, Image, object.corrected, exclusion.corrected, from.immovable)

# merge with main annotations df and replace old values for corrected items
data.w.immovable <- data.w.unsure %>%
  left_join(immovable, by = c("sub_num", "Image")) %>%
  mutate(Object = ifelse(!is.na(from.immovable), 
                         object.corrected, Object), 
         exclusion = ifelse(!is.na(from.immovable), exclusion.corrected, exclusion)) %>%
  select(-ends_with("corrected"), -from.immovable) %>%
  mutate(Object = ifelse(!is.na(exclusion), NA, Object)) %>%
  distinct()

# replace originally excluded images --------------------------------------
# read in manually checked images with no objects
none <- read_csv("data/manual-checks/no-objects.csv") %>%
  mutate(object.corrected = Object, 
         exclusion.corrected = ifelse(Exclude == 1, "Exclude", ifelse(None == 1, "None", NA)), 
         from.none = 1) %>%
  select(sub_num, Image, object.corrected, exclusion.corrected, from.none) 

# merge with main annotations df and replace if there was a held object
data.w.none <- data.w.immovable %>%
  full_join(none, by = c("sub_num", "Image")) %>%
  mutate(Object = ifelse(!is.na(from.none), object.corrected, Object), 
         exclusion = ifelse(!is.na(from.none), exclusion.corrected, exclusion)) %>%
  select(-ends_with("corrected"), -from.none) %>%
  mutate(Object = ifelse(!is.na(exclusion), NA, Object)) %>%
  distinct()

checked.none <- read_csv("data/manual-checks/no-objects.csv") %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  pull(exact.image)

not.checked.none <- data.w.none %>%
  filter(exclusion == "None") %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  filter(exact.image %notin% checked.none)

if (nrow(not.checked.none > 0)) {
  write_csv(not.checked.none, "data/manual-checks/new-no-objects.csv")
}

# add regularized labels --------------------------------------------------
# determine if there are any objects that need regularized labels
# if there are, write csv file with only these objects
data.w.objects <- data.w.none %>%
  mutate(n.objects = str_count(Object, ",") + 1) %>%
  separate(Object, c("Object1", "Object2", "Object3"), ",") %>%
  mutate(across(starts_with("Object"), ~ trimws(.))) %>%
  pivot_longer(starts_with("Object"), names_to = "object.num", values_to = "Object") %>%
  filter(!is.na(Object) | !is.na(exclusion)) %>%
  mutate(Object = ifelse(!is.na(exclusion), NA, Object)) %>%
  distinct()

regularized <- read_csv("data/manual-checks/labels.csv") %>%
  pull(Object) %>%
  unique()

not.regularized <- data.w.objects %>%
  select(Object) %>%
  distinct() %>%
  filter(Object %notin% regularized)

if (nrow(not.regularized > 0)) {
  write_csv(not.regularized, "data/manual-checks/new-objects-to-label.csv")
}

# read in manually checked object labels
labels <- read_csv("data/manual-checks/labels.csv")

# merge with main annotations df and replace raw label with regularized version
data.w.labels <- data.w.objects %>%
  left_join(labels, by = "Object") %>%
  mutate(object = ifelse(is.na(object.corrected), Object, object.corrected)) %>%
  mutate(n.objects = str_count(object, ",") + 1) %>%
  separate(object, c("object1", "object2"), ",") %>%
  mutate(across(ends_with("1|2"), ~ trimws(.))) %>%
  pivot_longer(c("object1", "object2"), names_to = "object.num2", values_to = "object2") %>%
  filter(!is.na(object2) | !is.na(exclusion)) %>%
  rename(object = object2) %>%
  mutate(object = trimws(object)) %>%
  select(sub_num, Image, exclusion, object) %>%
  mutate(object = ifelse(!is.na(exclusion), NA, object)) %>%
  distinct()

# add corrections ---------------------------------------------------------
corrections <- read_csv("data/manual-checks/corrections.csv") %>%
  mutate(from.corrections = 1) %>%
  select(sub_num, Image, object.corrected, exclusion.corrected, from.corrections)

corrected.images <- corrections %>%
  mutate(corrected.image = paste0(sub_num, "/", Image)) %>%
  pull() %>%
  unique()

data.pre.correction <- data.w.labels %>%
  mutate(exact.image = paste0(sub_num, "/", Image), 
         exclusion = ifelse(exact.image %in% corrected.images, NA, exclusion), 
         object = ifelse(exact.image %in% corrected.images, NA, object)) %>%
  distinct() %>%
  select(-exact.image)
         
data.w.corrections <- data.pre.correction %>%
  left_join(corrections, by = c("sub_num", "Image")) %>%
  mutate(object = ifelse(!is.na(from.corrections), object.corrected, object), 
         exclusion = ifelse(!is.na(from.corrections), exclusion.corrected, exclusion)) %>%
  select(-ends_with("corrected")) %>%
  mutate(n.objects = str_count(object, ",") + 1)  %>%
  separate(object, c("object1", "object2", "object3"), ",") %>%
  mutate(across(ends_with("1|2|3"), ~ trimws(.))) %>%
  pivot_longer(c("object1", "object2", "object3"), names_to = "object.num2", values_to = "object2") %>%
  filter(!is.na(object2) | !is.na(exclusion)) %>%
  rename(object = object2) %>%
  mutate(object = trimws(object)) %>%
  select(sub_num, Image, exclusion, object) %>%
  mutate(object = ifelse(!is.na(exclusion), NA, object)) %>%
  distinct()

# add labels to distinguish mealtime knifes vs. working tools
data.w.corrections <- data.w.corrections %>%
  mutate(exact.image = paste0(sub_num, "/", Image), 
         object = ifelse(exact.image %in% m.knife.images, "knife-M", 
                             ifelse(exact.image %in% w.knife.images, "knife-W", 
                             object)))
  
# determine if there are any objects that need categories
# if there are, write csv file with only these objects
categorized <- read_csv("data/manual-checks/categories.csv") %>%
  pull(object) %>%
  unique()

not.categorized <- data.w.corrections %>%
  filter(is.na(exclusion)) %>%
  select(object) %>%
  distinct() %>%
  filter(object %notin% categorized)

if (nrow(not.categorized > 0)) {
  write_csv(not.categorized, "data/manual-checks/new-objects-to-categorize.csv")
}

# add regularized categories ----------------------------------------------
# read in manually checked categories for each object
categories <- read_csv("data/manual-checks/categories.csv")

# merge with main annotations df and store new category
data.w.categories <- data.w.corrections %>%
  left_join(categories, by = "object") %>%
  select(sub_num, Image, exclusion, category, object) %>%
  mutate(object = ifelse(!is.na(exclusion), NA, object), 
         category = ifelse(!is.na(exclusion), NA, category)) %>%
  distinct() 

# correct for multiple exclusion reasons ----------------------------------
multiple.exclusion.images <- data.w.categories %>%
  filter(!is.na(exclusion)) %>%
  select(sub_num, Image, exclusion) %>%
  distinct() %>%
  group_by(sub_num, Image) %>%
  filter(length(unique(exclusion)) > 1) %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  pull(exact.image) %>%
  unique()

data.w.exclusions <- data.w.categories %>%
  mutate(exact.image = paste0(sub_num, "/", Image), 
         exclusion = ifelse(exact.image %in% multiple.exclusion.images, 
                            "None", exclusion)) %>%
  distinct()

# export clean data -------------------------------------------------------
# randomly select rec for kids with 2 recs
p1.exclude <- sample(c(7959, 4856), 1)
p2.exclude <- sample(c(4590, 5499), 1)

data.to.export <- data.w.exclusions %>%
  left_join(participants, by = "sub_num") %>%
  left_join(timestamps, by = c("sub_num", "Image")) %>%
  filter(sub_num != p1.exclude & sub_num != p2.exclude) %>%
  rename(image = Image, 
         timestamp = TimestampHMS) %>%
  select(site, sub_num, age, sex, child_worn_camera, image, exclusion, 
         category, object, timestamp) %>%
  mutate(object = ifelse(!is.na(exclusion), NA, object), 
         category = ifelse(!is.na(exclusion), NA, category)) %>%
  distinct()

write_csv(data.to.export, "data/object-data.csv")