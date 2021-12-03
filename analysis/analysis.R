library(tidyverse)

data <- read_csv("../data/usable.data_20211202.csv")
site_colors <- c("Rossel" = "#3C5388", 
                 "Tseltal" = "#02A087")

# categories --------------------------------------------------------------
category_counts <- data %>%
  group_by(age) %>%
  mutate(n_images = n()) %>%
  group_by(age, object_type) %>%
  summarize(n_category = n(), 
            n_images = n_images, 
            prop = n_category/n_images, 
            site = site) %>%
  distinct() %>%
  ungroup() %>%
  mutate(object_type = factor(object_type, 
                              levels = c("Food", "Tool", "Toy", 
                                         "Other Synthetic Object", 
                                         "Other Natural Object", 
                                         "Other Large Immovable Object"), 
                              labels = c("Food", "Tool", "Toy", 
                                         "Other\nSynthetic", 
                                         "Other\nNatural", 
                                         "Large\nImmovable")))

category_means <- category_counts %>%
  group_by(object_type, site) %>%
  summarize(mean = mean(prop), 
            sd = sd(prop), 
            n = n(), 
            se = sd/sqrt(n))
  
ggplot() +
  geom_bar(data = category_means, 
           mapping  = aes(x = object_type, y = mean, color = site, fill = site),
           stat = "identity", 
           position = "dodge", 
           alpha = 0.7) + 
  geom_point(data = category_counts,
             mapping = aes(x = object_type, y = prop, color = site, fill = site),
             position = position_dodge(width = 0.9), 
             size = 2) +
  geom_errorbar(data = category_means,
                mapping = aes(x = object_type, ymin = mean - se, ymax = mean + se, 
                              group = site),
                position = position_dodge(width = 0.9), 
                width = 0.25, size = 0.8, color = "black") +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  labs(x = "Object Categories", y = "Mean Prop. of Images", color = "Site", fill = "Site") +
  theme_classic(base_size = 15)
ggsave("../figs/category-props.jpg", height = 5, width = 7)
  
unique_categories <- data %>%
  group_by(sub_num) %>%
  summarize(n = length(unique(object_type)), 
            age = age, 
            site = site) %>%
  distinct()

ggplot(unique_categories, aes(x = age, y = n, color = site, fill = site)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  labs(x = "Age (months)", y = "Number of Object Categories", 
       color = "Site", fill = "Site") + 
  theme_classic(base_size = 15)
ggsave("../figs/unique-categories.jpg", height = 5, width = 6)