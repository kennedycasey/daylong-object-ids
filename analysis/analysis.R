library(tidyverse)
library(ggpubr)
library(lubridate)
library(viridis)

data <- read_csv("../data/usable.data_20220106.csv") %>%
  mutate(object = ifelse(object %in% c("croc shoe", "flip flop", "sneaker", "flat shoe"), 
                         "shoe", object), 
    hour = hour(timestamp), 
    tod = case_when(
      hour < 11 ~ "morning", 
      hour >= 11 & hour <= 13 ~ "midday", 
      hour > 13 ~ "afternoon"), 
    tod = factor(tod, levels = c("morning", "midday", "afternoon")))
site_colors <- c("Rossel" = "#3C5388", 
                 "Tseltal" = "#02A087")

# categories --------------------------------------------------------------
category_counts <- data %>%
  group_by(age, object_type, site) %>%
  summarize(n_category = n()) %>%
  distinct() %>%
  ungroup() %>% 
  group_by(age, site) %>%
  summarize(n_images = sum(n_category), 
            n_category = n_category, 
            prop = n_category/n_images, 
            object_type = object_type) %>%
  distinct() %>%
  mutate(object_type = factor(object_type, 
                              levels = c("Food", "Tool", "Toy", 
                                         "Large Immovable",
                                         "Other Natural Object",
                                        "Other Synthetic Object"),
                              labels = c("Food", "Tool", "Toy", 
                                         "Large\nImmovable",
                                         "Other\nNatural", 
                                         "Other\nSynthetic")))

category_means <- category_counts %>%
  group_by(object_type, site) %>%
  summarize(mean = mean(prop), 
            sd = sd(prop), 
            n = n(), 
            se = sd/sqrt(n))
  
plot1 <- ggplot() +
  geom_bar(data = category_means, 
           mapping  = aes(x = object_type, y = mean*100, color = site, fill = site),
           stat = "identity", 
           position = "dodge", 
           alpha = 0.7) + 
  geom_point(data = category_counts,
             mapping = aes(x = object_type, y = prop*100, color = site, fill = site),
             position = position_dodge(width = 0.9), 
             size = 1.5) +
  geom_errorbar(data = category_means,
                mapping = aes(x = object_type, ymin = (mean - se)*100, ymax = (mean + se)*100, 
                              group = site),
                position = position_dodge(width = 0.9), 
                width = 0.25, size = 0.8, color = "black") +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  labs(x = "Object Categories", y = "% of Images", 
       color = "Site", fill = "Site", tag = "A") +
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

# objects -----------------------------------------------------------------
## proportion of unique objects handled per recording
unique_objects <- data %>%
  group_by(sub_num) %>%
  summarize(n_total = n(),
            n_unique = length(unique(object)), 
            prop = n_unique/n_total,
            age = age, 
            site = site) %>%
  distinct() %>%
  ungroup()

m <- glm(n_unique ~ age + site, data = unique_objects)
summary(m)

unique_objects %>%
  summarize(mean = mean(n_unique), 
            min = min(n_unique), 
            max = max(n_unique))

plot2 <- ggplot(unique_objects, aes(x = age, y = n_unique, color = site, fill = site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  labs(x = "Age (months)", y = "Unique Object Types", 
       color = "Site", fill = "Site", tag = "B") + 
  scale_x_continuous(limits = c(0, 49), breaks = c(0, 12, 24, 36, 48)) +
  theme_classic(base_size = 15)
ggsave("../figs/unique-objects.jpg", height = 5, width = 6)

# top objects based overlap across kids
top_objects_list <- list()
for (i in unique(data$site)) {
  top_objects <- data %>%
    filter(site == i) %>%
    mutate(total_kids = length(unique(sub_num))) %>%
    group_by(object) %>%
    summarize(n_kids = length(unique(sub_num)), 
              total_kids = total_kids, 
              prop = n_kids/total_kids) %>%
    ungroup() %>%
    distinct() %>%
    arrange(desc(prop)) %>%
    mutate(rank = row_number(), 
           site = i) 
  top_objects_list[[i]] <- top_objects
}
top_objects <- do.call(rbind, top_objects_list)

rossel_objects <- top_objects %>%
  filter(site == "Rossel") %>%
  pull(object)

tseltal_objects <- top_objects %>%
  filter(site == "Tseltal") %>%
  pull(object)

shared_objects <- intersect(rossel_objects, tseltal_objects)

length(shared_objects)/(length(rossel_objects) + length(tseltal_objects) - length(shared_objects))

data %>%
  filter(site == "Rossel") %>%
  group_by(object) %>%
  summarize(n_children = length(unique(sub_num))) 
  
test <- data %>%
  filter(site == "Tseltal") %>%
  mutate(total_kids = length(unique(sub_num))) %>%
  group_by(object) %>%
  summarize(n_kids = length(unique(sub_num)), 
            total_kids = total_kids, 
            prop = n_kids/total_kids) %>%
  ungroup() %>%
  distinct() %>%
  arrange(prop) %>%
  summarize(mean = mean(prop),
            min = min(prop), 
            max = max(prop))

data %>%
  filter(object %in% shared_objects & site == "Rossel") %>%
  group_by(site, object) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

rossel_objects_top25 <- top_objects %>%
  filter(rank <= 25 & site == "Rossel") %>% 
  select(object) %>%
  distinct() %>%
  pull()

tseltal_objects_top25 <- top_objects %>%
  filter(rank <= 25 & site == "Tseltal") %>% 
  select(object) %>%
  distinct() %>%
  pull()

top_category_labels <- data %>%
  filter(object %in% rossel_objects_top25 | object %in% tseltal_objects_top25) %>%
  group_by(object, object_type) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(object) %>%
  summarize(max = max(n), 
            n = n,
            object_type = object_type) %>%
  filter(n == max) %>%
  select(object, object_type)
  
top_objects <- top_objects %>%
  left_join(top_category_labels, by = "object") %>%
  mutate(both = ifelse(object %in% rossel_objects_top25 & object %in% tseltal_objects_top25, 1, 0), 
         label = paste0(object, " (", tolower(trimws(str_remove(object_type, "Object"))), ")"))

ggplot(filter(top_objects, rank <= 25), 
       aes(x = rank, y = prop*100, color = site, fill = site)) +
  facet_grid(. ~ site) +
  geom_bar(aes(alpha = as.factor(both)), stat = "identity") +
  geom_text(aes(y = prop*100/2, label = label), color = "black", srt = 90, size = 3.8) +
  scale_alpha_manual(values = c(0.2, 0.7)) +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  labs(x = "Top 25 Objects", y = "% of Children") +
  scale_y_continuous(limits = c(0, 62), breaks = c(0, 25, 50)) + 
  theme_test(base_size = 20) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        legend.position = "none")
ggsave("../figs/top-objects.jpg", height = 8, width = 12)

# time of day -------------------------------------------------------------
unique_objects_by_time <- data %>%
  group_by(sub_num, tod) %>%
  summarize(n_total = n(),
            n_unique = length(unique(object)), 
            prop = n_unique/n_total,
            age = age, 
            site = site) %>%
  distinct() %>%
  ungroup()

ggplot(unique_objects_by_time, aes(x = tod, y = n_unique, color = site, fill = site)) +
  geom_boxplot(alpha = 0.5, color = "black") +
  geom_point(position = position_dodge(width = 0.75)) +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  labs(x = "Time of Day", y = "Unique Object Types", color = "Site", fill = "Site") + 
  theme_classic(base_size = 15)


category_counts_by_time <- data %>%
  group_by(age, object_type, site, tod) %>%
  summarize(n_category = n()) %>%
  distinct() %>%
  ungroup() %>% 
  group_by(age, site, tod) %>%
  summarize(n_images = sum(n_category), 
            n_category = n_category, 
            prop = n_category/n_images, 
            object_type = object_type) %>%
  distinct() %>%
  mutate(object_type = factor(object_type, 
                              levels = c("Food", "Tool", "Toy", 
                                         "Large Immovable",
                                         "Other Natural Object",
                                         "Other Synthetic Object"),
                              labels = c("Food", "Tool", "Toy", 
                                         "Large\nImmovable",
                                         "Other\nNatural", 
                                         "Other\nSynthetic")))

category_means_by_time <- category_counts_by_time %>%
  group_by(object_type, site, tod) %>%
  summarize(mean = mean(prop), 
            sd = sd(prop), 
            n = n(), 
            se = sd/sqrt(n))

ggplot() +
  facet_grid(. ~ tod) +
  geom_bar(data = category_means_by_time, 
           mapping  = aes(x = object_type, y = mean*100, color = site, fill = site),
           stat = "identity", 
           position = "dodge", 
           alpha = 0.7) + 
  geom_point(data = category_counts_by_time,
             mapping = aes(x = object_type, y = prop*100, color = site, fill = site),
             position = position_dodge(width = 0.9), 
             size = 1.5) +
  geom_errorbar(data = category_means_by_time,
                mapping = aes(x = object_type, ymin = (mean - se)*100, ymax = (mean + se)*100, 
                              group = site),
                position = position_dodge(width = 0.9), 
                width = 0.25, size = 0.8, color = "black") +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  labs(x = "Object Categories", y = "% of Images", 
       color = "Site", fill = "Site") +
  theme_classic(base_size = 12)

ordered_subs <- data %>%
  select(site, age, sub_num) %>%
  distinct() %>%
  arrange(site, age) %>%
  group_by(site) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  select(sub_num, order)

data %>%
  left_join(ordered_subs, by = c("sub_num")) %>%
  mutate(object_type = ifelse(object_type %in% c("Food", "Other Natural Object"), 
                              str_remove(object_type, "Other "), "Synthetic Object")) %>%
  ggplot(aes(x = timestamp, y = as.factor(order), 
             color = object_type)) +
  facet_wrap(. ~ site) +
  geom_point(shape = "|", size = 5) + 
  labs(x = "Time of Day", y = "Participants (youngest to oldest)", 
       color = "Object Type") +
  theme_test() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) 
ggsave("figs/category-time-distribution.jpg", width = 10, height = 8)


category_colors <- c("Food" = "#440154FF",
                     "Toy" = "#414487FF",
                     "Tool" = "#2A788EFF", 
                     "Natural" = "#22A884FF", 
                     "Synthetic" = "#7AD151FF", 
                     "Immovable" = "#FDE725FF")

for (i in 1:29) {
  for (j in c("Rossel", "Tseltal")) {
    data %>%
      left_join(ordered_subs, by = c("sub_num")) %>%
      filter(order == i & site == j) %>%
      mutate(object_type = trimws(str_remove_all(object_type, "Object|Other|Large")), 
             object_type = factor(object_type, 
                                  levels = c("Food", "Toy", "Tool", 
                                             "Natural", "Synthetic", "Immovable"))) %>%
      ggplot(aes(x = timestamp, y = NA, color = object_type)) +
      facet_grid(object_type ~ ., switch = "y", drop = FALSE) +
      geom_point(shape = "|", size = 10) +
      labs(x = "Time of Day") +
      scale_x_time(limits = hms("08:00:00", "20:00:00"),
                   breaks = hms("08:00:00", "12:00:00", "16:00:00", "20:00:00")) +
      scale_color_manual(values = category_colors) +
      theme_test() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
            axis.title.y = element_blank(), legend.position = "none", 
            aspect.ratio = 0.1, strip.text = element_text(size = 5))
    
    sub_num <- data %>%
      left_join(ordered_subs, by = c("sub_num")) %>%
      filter(order == i & site == j) %>%
      pull(sub_num) %>%
      unique()
    
    age <- data %>%
      left_join(ordered_subs, by = c("sub_num")) %>%
      filter(order == i & site == j) %>%
      pull(age) %>%
      unique()  
    
    ggsave(paste0("../figs/TOD-cat-dist/", j, "-", age, "mo-", sub_num, ".jpg"), width = 4, height = 4)
  }
}
