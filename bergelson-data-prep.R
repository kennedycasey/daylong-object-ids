library(tidyverse)

# read in data
annotations <- read_csv("data/bergelson/all-annotations.csv")
participants <- read_csv("data/bergelson/metadata/participants.csv")

# create not in operator
`%notin%` <- Negate(`%in%`)

# create one column that tells us the reason for exclusion
raw.data <- annotations %>%
  pivot_longer(c(None, Exclude, Unsure), 
               names_to = "exclusion", 
               values_to = "exclusion.val") %>%
  mutate(exclusion = ifelse(exclusion.val == 1, exclusion, NA)) %>%
  pivot_longer(c(StudyRelated:OtherNatural), 
               names_to = "category", 
               values_to = "category.val") %>%
  mutate(category = ifelse(category.val == 1, category, NA)) %>%
  select(sub_num, Image, exclusion, ObjectCount, Object, 
         category, OutOfFrame, Reaching) %>%
  distinct()

# replace unsure objects --------------------------------------------------

# read in manually checked unsure objects
unsure <- read_csv("data/bergelson/manual-checks/unsure-objects.csv") %>%
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

checked.unsure <- read_csv("data/bergelson/manual-checks/unsure-objects.csv") %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  pull(exact.image)

not.checked.unsure <- data.w.unsure %>%
  filter(exclusion == "Unsure") %>%
  mutate(exact.image = paste0(sub_num, "/", Image)) %>%
  filter(exact.image %notin% checked.unsure)

if (nrow(not.checked.unsure > 0)) {
  write_csv(not.checked.unsure, "data/bergelson/manual-checks/new-unsure-objects.csv")
}


# add regularized labels --------------------------------------------------
# determine if there are any objects that need regularized labels
# if there are, write csv file with only these objects
data.w.objects <- data.w.unsure %>%
  mutate(n.objects = ObjectCount) %>%
  filter((!is.na(Object) & !is.na(category)) | (!is.na(exclusion))) %>%
  mutate(Object = ifelse(!is.na(exclusion), NA, Object), 
         Image = str_remove(Image, "_DUPLICATE[0-9]")) %>%
  distinct()

regularized <- read_csv("data/bergelson/manual-checks/labels.csv") %>%
  pull(Object) %>%
  unique()

not.regularized <- data.w.objects %>%
  filter(!is.na(Object)) %>%
  select(Object, category) %>%
  distinct() %>%
  filter(Object %notin% regularized)

if (nrow(not.regularized > 0)) {
  write_csv(not.regularized, "data/bergelson/manual-checks/new-objects-to-label.csv")
}

# read in manually checked object labels
labels <- read_csv("data/bergelson/manual-checks/labels.csv")

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
  select(sub_num, Image, exclusion, object, object.basic, category) %>%
  mutate(object = ifelse(!is.na(exclusion), NA, object)) %>%
  distinct()
  
# add corrections ---------------------------------------------------------
# corrections <- read_csv("data/bergelson/manual-checks/corrections.csv") %>%
#   mutate(from.corrections = 1) %>%
#   select(sub_num, Image, object.corrected, exclusion.corrected, from.corrections)
# 
# corrected.images <- corrections %>%
#   mutate(corrected.image = paste0(sub_num, "/", Image)) %>%
#   pull() %>%
#   unique()
# 
# data.pre.correction <- data.w.labels %>%
#   mutate(exact.image = paste0(sub_num, "/", Image), 
#          exclusion = ifelse(exact.image %in% corrected.images, NA, exclusion), 
#          object = ifelse(exact.image %in% corrected.images, NA, object)) %>%
#   distinct() %>%
#   select(-exact.image)
#          
# data.w.corrections <- data.pre.correction %>%
#   left_join(corrections, by = c("sub_num", "Image")) %>%
#   mutate(object = ifelse(!is.na(from.corrections), object.corrected, object), 
#          exclusion = ifelse(!is.na(from.corrections), exclusion.corrected, exclusion)) %>%
#   select(-ends_with("corrected")) %>%
#   mutate(n.objects = str_count(object, ",") + 1)  %>%
#   separate(object, c("object1", "object2", "object3"), ",") %>%
#   mutate(across(ends_with("1|2|3"), ~ trimws(.))) %>%
#   pivot_longer(c("object1", "object2", "object3"), names_to = "object.num2", values_to = "object2") %>%
#   filter(!is.na(object2) | !is.na(exclusion)) %>%
#   rename(object = object2) %>%
#   mutate(object = trimws(object)) %>%
#   select(sub_num, Image, exclusion, object) %>%
#   mutate(object = ifelse(!is.na(exclusion), NA, object)) %>%
#   distinct()
# 
# # correct for multiple exclusion reasons ----------------------------------
# multiple.exclusion.images <- data.w.categories %>%
#   filter(!is.na(exclusion)) %>%
#   select(sub_num, Image, exclusion) %>%
#   distinct() %>%
#   group_by(sub_num, Image) %>%
#   filter(length(unique(exclusion)) > 1) %>%
#   mutate(exact.image = paste0(sub_num, "/", Image)) %>%
#   pull(exact.image) %>%
#   unique()
# 
# data.w.exclusions <- data.w.categories %>%
#   mutate(exact.image = paste0(sub_num, "/", Image), 
#          exclusion = ifelse(exact.image %in% multiple.exclusion.images, 
#                             "None", exclusion)) %>%
#   distinct()

# export clean data -------------------------------------------------------
data.to.export <- data.w.labels %>%
  left_join(participants, by = "sub_num") %>%
  rename(image = Image) %>%
  select(site, sub_num, age, sex, image, exclusion, category, object, object.basic) %>%
  mutate(object = ifelse(!is.na(exclusion), NA, object), 
         object.basic = ifelse(!is.na(exclusion), NA, object.basic), 
         category = ifelse(!is.na(exclusion), NA, category), 
         image_no = str_remove(image, ".gif"), 
         hr = as.numeric(as.character(substr(image_no, 1, 2))), 
         min = as.numeric(as.character(substr(image_no, 3, 4))), 
         sec = as.numeric(as.character(substr(image_no, 5, 6))), 
         msec = as.numeric(as.character(ifelse(str_detect(image_no, "."), 
                       sub(".*\\.", "", image_no), "0"))),
         timestamp = (hr*3600 + min*60 + sec)*1000 + msec) %>%
  distinct()

write_csv(data.to.export, "data/bergelson/all-data.csv")