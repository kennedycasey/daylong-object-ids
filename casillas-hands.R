library(tidyverse)

# read in data
imgs <- read_csv("data/casillas/all-img-data.csv") %>%
  mutate(path = paste0(sub_num, "/", Image))
hands <- read_csv("data/casillas/hands-annotations.csv") %>%
  mutate(path = paste0(sub_num, "/", Image))
objects <- read_csv("data/casillas/object-data.csv") %>%
  mutate(path = paste0(sub_num, "/", image))

# TEMP LINE WHILE HANDS CODING STILL IN PROGRESS
coded.ptcps <- unique(hands$sub_num)

# create not in operator
`%notin%` <- Negate(`%in%`)

# check overlap in hands and objects coding
overlap <- intersect(hands$path, objects$path) %>%
  unique()

# remove from the objects tibble any images with no handling
rm.objects <- objects %>%
  filter(path %in% overlap & exclusion == "None") %>%
  pull(path) %>%
  unique()

objects <- filter(objects, path %notin% rm.objects)

# remove from the hands tibble any images with object handling
rm.hands <- objects %>%
  filter(path %in% overlap) %>%
  filter(is.na(exclusion) | exclusion != "None") %>%
  pull(path) %>%
  unique()

hands <- filter(hands, path %notin% rm.hands)

# rm all coded imgs from all imgs tibble
imgs <- imgs %>%
  filter(sub_num %in% coded.ptcps) %>%
  mutate(image = Image) %>%
  select(path, sub_num, image) %>%
  distinct() %>%
  filter(path %notin% c(hands$path, objects$path)) %>%
  mutate(hands = 0, 
         hands_count = 0)

# clean up objects tibble and add hands var
objects <- objects %>%
  filter(sub_num %in% coded.ptcps) %>%
  select(path, sub_num, image) %>%
  distinct() %>%
  mutate(hands = 1, 
         hands_count = NA)


# merge all data
data <- hands %>%
  filter(sub_num %in% coded.ptcps) %>%
  mutate(hands_count = Hands, 
         hands = case_when(
           Hands %in% c(1, 2) ~ 1, 
           Hands == 0 ~ 0), 
         image = Image) %>%
  select(path, sub_num, image, hands, hands_count) %>%
  bind_rows(objects, imgs) %>%
  select(-path)





