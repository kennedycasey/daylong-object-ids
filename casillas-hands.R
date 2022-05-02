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

metadata <- imgs %>%
  select(path, Site, Age, Mobility) %>%
  distinct()

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
  left_join(metadata, by = "path") %>%
  select(-path)

site.colors <- c("Tseltal" = "sandybrown", 
                 "Rossel" = "brown4")

data %>%
  group_by(sub_num) %>%
  mutate(total = length(unique(image)), 
         Site = ifelse(Site == "Mayan", "Tseltal", Site),
         Site = factor(Site, levels = c("Tseltal", "Rossel")), 
         Mobility = ifelse(Mobility == "Y", "Y", "N")) %>%
  group_by(sub_num, total, Site, Age, Mobility, hands) %>%
  summarize(n = length(unique(image))) %>%
  filter(hands == 1) %>%
  summarize(hands_prop = n/total*100) %>%
  ggplot(aes(x = Age, y = hands_prop, color = Site, fill = Site)) + 
  facet_grid(. ~ Site) + 
  geom_point(aes(shape = Mobility), size = 3, alpha = 0.7) + 
  geom_smooth(method = "loess") +
  scale_color_manual(values = site.colors) + 
  scale_fill_manual(values = site.colors) + 
  scale_shape_manual(values = c(1, 19)) + 
  scale_x_continuous(breaks = c(0, 12, 24, 36, 48)) + 
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) + 
  labs(x = "Age (months)", y = "% Photos with Hands") + 
  theme_test(base_size = 20)
