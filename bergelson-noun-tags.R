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

write_csv(objects, "data/bergelson/basic-label-counts.csv")

