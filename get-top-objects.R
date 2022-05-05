library(tidyverse)

study.related <- c("camera", "vest", "camera cover", "headcam", "headband")
`%notin%` <- Negate(`%in%`)

all.casillas <- read_csv("data/casillas/object-data.csv") %>%
  filter(is.na(exclusion) & object %notin% study.related) %>%
  left_join(read_csv("data/casillas/manual-checks/basic.csv"), by = "object")
  
casillas <- all.casillas %>%
  select(site, sub_num, basic) 

all.bergelson <- read_csv("data/bergelson/all-data.csv") %>%
  filter(is.na(exclusion) & object %notin% study.related)

bergelson <- all.bergelson %>%
  select(site, sub_num, basic)

data <- bind_rows(casillas, bergelson) %>%
  distinct()

top.objects <- data %>%
  select(site, sub_num, basic) %>%
  distinct() %>%
  group_by(site) %>%
  summarize(total = length(unique(sub_num)), 
            basic = basic,
            sub_num = sub_num) %>%
  group_by(site, basic, total) %>%
  summarize(n.children = length(unique(sub_num))) %>%
  ungroup() %>%
  complete(site, basic, fill = list(n.children = 0)) %>%
  mutate(prop = ifelse(n.children == 0, 0, n.children/total)) %>%
  group_by(basic) %>%
  summarize(mean.prop = mean(prop)) %>%
  arrange(desc(mean.prop)) %>%
  slice_head(n = 100)

n.images <- all.casillas %>%
  select(site, sub_num, object, category, basic, image) %>%
  bind_rows(select(all.bergelson, site, sub_num, object, category, basic, image)) %>%
  filter(basic %in% top.objects$basic) %>%
  group_by(site, sub_num, basic) %>%
  summarize(n = length(unique(image))) %>%
  ungroup() %>%
  complete(sub_num, basic, fill = list(n = 0)) %>%
  group_by(site, basic) %>%
  summarize(mean = mean(n),
            median = median(n),
            min = min(n), 
            max = max(n)) %>%
  filter(!is.na(site))

all.casillas %>%
  select(site, sub_num, object, category, basic, image) %>%
  bind_rows(select(all.bergelson, site, sub_num, object, category, basic, image)) %>%
  filter(basic %in% top.objects$basic) %>%
  mutate(basic = factor(basic, levels = top.objects$basic)) %>%
  group_by(site, sub_num, basic) %>%
  summarize(n = length(unique(image))) %>%
  ungroup() %>%
  complete(sub_num, basic, fill = list(n = 0)) %>%
  filter(!is.na(site)) %>%
  ggplot(aes(x = basic, y = n, color = site)) +
  geom_boxplot() +
  labs(x = "Object", y = "Number of Images", color = "Site") + 
  theme_test() +
  theme(axis.text.x = element_text(angle = 45)) 
ggsave("top-objects.jpg", width = 8, height = 6)