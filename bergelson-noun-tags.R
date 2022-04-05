library(tidyverse)

ptcps <- read_csv("~/Desktop/secure/seedlings/bergelson-secure-metadata.csv") %>%
  rename(sub_num = aclew_id)
  
nouns <- read_csv("~/Desktop/secure/seedlings/all_basiclevel_NA.csv") %>%
  filter(SubjectNumber %in% unique(ptcps$lab_internal_subject_id)) %>%
  left_join(ptcps, by = c("SubjectNumber" = "lab_internal_subject_id")) %>%
  mutate(onset = onset/1000, 
         offset = offset/1000) %>%
  select(sub_num, onset, offset, object, basic_level, utterance_type, 
         object_present, speaker)

objects <- read_csv("data/bergelson/all-data.csv") %>%
  filter(is.na(exclusion)) %>%
  mutate(labels_30 = NA, 
         labels_60 = NA, 
         labels_180 = NA, 
         labels_300 = NA,
         labels_600 = NA)

for (i in 1:nrow(objects)) {
  ptcp <- objects[i,]$sub_num
  label <- objects[i,]$object.basic
  timestamp <- objects[i,]$timestamp
  
  for (j in c(30, 60, 180, 300, 600)) {
    min <- timestamp - j*1000
    max <- timestamp + j*1000
    
    nearby_nouns <- nouns %>%
      filter(sub_num == ptcp &
               onset >= min & 
               offset <= max & 
               basic_level == label) %>%
      nrow()
    
    objects[i, paste0("labels_", j)] <- nearby_nouns
  }
}

objects <- objects %>%
  pivot_longer(starts_with("labels"), 
               names_to = "time_window", 
               values_to = "label_count") %>% 
  mutate(time_window = factor(str_remove(time_window, "labels_"), 
                              levels = c("30", "60", "180", "300", "600"), 
                              labels = c("+/- 30sec", "+/- 1min", "+/- 3min", "+/- 5min", "+/- 10min")))

write_csv(objects, "data/bergelson/basic-label-counts.csv")

ggplot(objects, aes(x = time_window, y = label_count)) + 
  geom_violin() +
  geom_jitter(filter(objects, label_count > 0),
              mapping = aes(x = time_window, y = label_count), 
              alpha = 0.1) +
  labs(x = "Time window surrouding child object handling", 
       y = "Count of basic-level labels produced by adults") +
  theme_classic()

objects %>%
  group_by(sub_num, time_window) %>%
  summarize(label_count = mean(label_count)) %>%
  group_by(time_window) %>%
  summarize(mean = mean(label_count), 
            median = median(label_count), 
            sd = sd(label_count), 
            min = min(label_count), 
            max = max(label_count))
