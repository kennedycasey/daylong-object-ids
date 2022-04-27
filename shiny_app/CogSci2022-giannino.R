## ----global_options, include = FALSE------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 3, fig.height = 3, fig.crop  =  FALSE, 
                      fig.pos  =  "tb", fig.path = 'figs/',
                      echo = FALSE, warning = FALSE, cache = FALSE, 
                      message = FALSE, sanitize = TRUE)


## ----libraries----------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(broom)
library(broom.mixed)
library(lme4)
library(lmerTest)
library(gghalves)
library(cowplot)
library(ggpubr)
library(ggeffects)
library(xtable)
library(kableExtra)


## ----data-prep----------------------------------------------------------------------------------
run.from.raw = FALSE
if (run.from.raw) {
  # this R script creates the "all-data.csv"
  # set run.from.raw equal to TRUE if you want to regenerate it
  source("../casillas-data-prep.R")
} 

# read in data
raw.data <- read_csv("../data/casillas/all-data.csv")
participants <- read_csv("../data/casillas/metadata/participants.csv") %>%
  select(site, sub_num, age)

included.subs <- unique(raw.data$sub_num)

# read in C&E data for all codeable hours per ptcp
codeable.hrs.pp <- read_csv(
  "../data/casillas/metadata/all-hours-with-codeable-photos-per-ptcp.csv") %>%
  filter(sub_num %in% included.subs) %>%
  # add participant metadata
  full_join(participants)

# pre-set colors and category labels
sites <- c("Tseltal", "Rossel")

site.colors <- c("Tseltal" = "sandybrown", 
                 "Rossel" = "brown4")

categories <- c("Food", "Synthetic", "Natural", "Toy",
                "Mealtime Tool", "Clothing", "Immovable", 
                "Work Tool")

category.labels <- c("Food", "Synthetic", "Natural", "Toy",
                "Tool-M", "Clothing", "Immovable", 
                "Tool-W")

# rm excluded photos + set other variable info
data <- raw.data %>%
  filter(is.na(exclusion) & !is.na(category)) %>%
  mutate(hour = hour(timestamp),
         category = factor(category, levels = categories), 
         site = factor(site, levels = sites))

# set rm.study.related to TRUE if you want to check results w/o these
study.related <- c("camera", "vest", "camera cover")
# define operator that selects all but the things in a list
`%notin%` <- Negate(`%in%`)

rm.study.related = FALSE
if (rm.study.related) {
  data <- data %>%
      filter(object %notin% study.related)
}


## ----exclusions---------------------------------------------------------------------------------
# check for errors in exclusion counts 
# some images were getting counted as both included and excluded
# excluding for now since it's only a handful but return to these in later checks
included.image.paths <- data %>%
  mutate(path = paste0(sub_num, "/", image)) %>%
  pull(path) %>%
  unique()

excluded.image.paths <- raw.data %>%
  filter(!is.na(exclusion)) %>%
  mutate(path = paste0(sub_num, "/", image)) %>%
  pull(path) %>%
  unique()
# intersect(included.image.paths, excluded.image.paths)

# get total number of images before exclusion
total.coded <- raw.data %>%
  select(sub_num, image) %>%
  distinct() %>%
  nrow()

# get total number of images after exclusion
total.included <- data %>%
  select(sub_num, image) %>%
  distinct() %>%
  mutate(path = paste0(sub_num, "/", image)) %>%
  filter(path %notin% excluded.image.paths) %>%
  nrow()

ros.included <- data %>%
  filter(site == "Rossel") %>%
  select(sub_num, image) %>%
  distinct() %>%
  mutate(path = paste0(sub_num, "/", image)) %>%
  filter(path %notin% excluded.image.paths) %>%
  nrow()

tse.included <- data %>%
  filter(site == "Tseltal") %>%
  select(sub_num, image) %>%
  distinct() %>%
  mutate(path = paste0(sub_num, "/", image)) %>%
  filter(path %notin% excluded.image.paths) %>%
  nrow()

# get total subs before and after exclusion
total.subs <- unique(raw.data$sub_num)
included.subs <- unique(raw.data$sub_num)

no.usable.images.subs <- raw.data %>%
  group_by(sub_num) %>%
  mutate(n.images = length(unique(image)), 
         path = paste0(sub_num, "/", image)) %>%
  group_by(sub_num, n.images) %>%
  filter(!is.na(exclusion) | path %in% excluded.image.paths) %>%
  summarize(n.excluded = n()) %>%
  filter(n.excluded == n.images) %>%
  nrow()

# get exclusion counts and percentages
exclusions <- raw.data %>%
  mutate(path = paste0(sub_num, "/", image)) %>%
  filter(!is.na(exclusion) | path %in% excluded.image.paths) %>%
  select(sub_num, image) %>%
  distinct() %>%
  nrow()

# get number of directories where the target child wasn't wearing the camera
caregiver.worn <- raw.data %>%
  mutate(camera.wearer = ifelse(child_worn_camera == 1, "child", "caregiver")) %>%
  group_by(site, camera.wearer) %>%
  summarize(n.kids = length(unique(sub_num))) %>%
  pivot_wider(names_from = "camera.wearer", values_from = "n.kids") %>%
  mutate(prop = caregiver / (caregiver + child)) %>%
  rename(n = caregiver)


## ----demo---------------------------------------------------------------------------------------
# create tibbles with basic descriptive stats of kid demographics and # images
demo.bysite <- raw.data %>%
  group_by(site, sub_num, age) %>%
  summarize(n.images = length(unique(image))) %>%
  group_by(site) %>%
  summarize(n.kids = length(unique(sub_num)), 
            m.images = mean(n.images),
            m.age = mean(age)) 

demo <- raw.data %>%
  group_by(sub_num, age) %>%
  summarize(n.images = n()) %>%
  ungroup() %>%
  summarize(min.images = min(n.images), 
            max.images = max(n.images), 
            mean.images = mean(n.images),
            median.images = median(n.images),
            min.age = min(age), 
            max.age = max(age),
            n.kids = length(unique(sub_num)))


## ----examples-fig, fig.env = "figure", fig.pos = "h", fig.align = "center", fig.width = 2.9, fig.height = 2, fig.cap = "Example images with object and category labels."----

examples <- png::readPNG("../figs/example-objects.png")
grid::grid.raster(examples)


## ----reliability--------------------------------------------------------------------------------
# read in output of "casillas-reliability.R"
reliability <- read_csv("../data/casillas/reliability-stats.csv")

# get total number of images that were double coded
n.reliability <- sum(reliability$n.images)

# calculate % agreement at the category and object level
# ...overall
reliability.stats <- reliability %>%
    summarize(category = mean(category.agreement)*100,
              object = mean(object.agreement)*100)
  
# ...by site
bysite.reliability.stats <- reliability %>%
  group_by(site) %>%
  summarize(category = mean(category.agreement)*100, 
            object = mean(object.agreement)*100)



## ----top-objects, results = "asis"--------------------------------------------------------------
# compute the objects that most often appear 1+ times across children w/i sites
# use this to find the top 3 items per category per site
top.objects.bykids <- data %>%
  filter(object %notin% study.related) %>%
  select(site, category, object, sub_num) %>%
  distinct() %>%
  group_by(site) %>%
  summarize(total = length(unique(sub_num)), 
            category = category, 
            object = object, 
            sub_num = sub_num) %>%
  group_by(site, category, object, total) %>%
  summarize(n.children = length(unique(sub_num))) %>%
  mutate(prop.children = n.children/total) %>%
  ungroup() %>%
  arrange(site, category, desc(prop.children)) %>%
  group_by(site, category) %>%
  mutate(rank = row_number()) %>%
  filter(rank %in% 1:3) %>%
  select(-n.children, -prop.children) %>%
  pivot_wider(names_from = "rank", values_from = "object") %>%
  mutate(objects = paste0(`1`, ", ", `2`, ", ", `3`), 
         objects = str_remove(objects, "-M|-W")) %>%
  select(category, objects, site) %>%
  group_by(category) %>%
  pivot_wider(names_from = "site", values_from = "objects")

# get list of all objects found in each site
ros.objects <- data %>% 
  filter(site == "Rossel" & object %notin% study.related) %>%
  pull(object) %>%
  unique()

tse.objects <- data %>% 
  filter(site == "Tseltal" & object %notin% study.related) %>%
  pull(object) %>%
  unique()

# compute the objects that appear in the most images across children w/i sites
# use this to find the top 3 items per category per site
tse.top.objects.bytime <- data %>%
  filter(site == "Tseltal" & object %notin% study.related) %>%
  group_by(site, sub_num) %>%
  mutate(n.images = length(unique(image)), 
         object = as_factor(object) %>% fct_expand(tse.objects)) %>%
  group_by(site, sub_num, n.images, category) %>%
  count(object, .drop = FALSE) %>%
  mutate(prop = n/n.images) %>%
  ungroup() %>%
  group_by(category, object) %>%
  summarize(m.prop = mean(prop)) %>%
  arrange(category, desc(m.prop)) %>%
  group_by(category) %>%
  mutate(rank = row_number()) %>%
  filter(rank %in% 1:3) %>%
  select(-m.prop) %>%
  pivot_wider(names_from = "rank", values_from = "object") %>%
  mutate(objects = paste0(`1`, ", ", `2`, ", ", `3`), 
         objects = str_remove(objects, "-M|-W"), 
         site = "Tseltal") %>%
  select(category, objects, site) %>%
  group_by(category) %>%
  pivot_wider(names_from = "site", values_from = "objects")

top.objects.bytime <- data %>%
  filter(site == "Rossel" & object %notin% study.related) %>%
  group_by(site, sub_num) %>%
  mutate(n.images = length(unique(image)), 
         object = as_factor(object) %>% fct_expand(ros.objects)) %>%
  group_by(site, sub_num, n.images, category) %>%
  count(object, .drop = FALSE) %>%
  mutate(prop = n/n.images) %>%
  ungroup() %>%
  group_by(category, object) %>%
  summarize(m.prop = mean(prop)) %>%
  arrange(category, desc(m.prop)) %>%
  group_by(category) %>%
  mutate(rank = row_number()) %>%
  filter(rank %in% 1:3) %>%
  select(-m.prop) %>%
  pivot_wider(names_from = "rank", values_from = "object") %>%
  mutate(objects = paste0(`1`, ", ", `2`, ", ", `3`), 
         objects = str_remove(objects, "-M|-W"), 
         site = "Rossel") %>%
  select(category, objects, site) %>%
  group_by(category) %>%
  pivot_wider(names_from = "site", values_from = "objects") %>%
  full_join(tse.top.objects.bytime, by = "category")
  
# compute number of objects in each category across sites
category.counts <- data %>%
  group_by(category, site) %>%
  summarize(n.objects = length(unique(object))) %>%
  pivot_wider(names_from = "site", values_from = "n.objects")

# combine and do some formatting to create table
tbl.input <- bind_cols(category.counts, top.objects.bykids) %>%
  select(category...1, Tseltal...2, Tseltal...5, Rossel...3, Rossel...6)

colnames(tbl.input) <- c("Object Category", "N", "Top Objects", "N", "Top Objects")

kable(tbl.input, format = "latex", 
      table.envir = "table", 
      row.names = FALSE, 
      booktabs = TRUE,
      position = "!ht",
      linesep = "",
      caption = "Number of unique objects (N) and objects handled by the most children, for each category, across sites.") %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" " = 1, "Tseltal" = 2, "Rossel" = 2), align = "c", bold = TRUE) %>%
  row_spec(0, bold = TRUE)


## ----daily-object-stats, results = "hide"-------------------------------------------------------
# compute descriptive stats of # unique objects held by individual kids

# ... overall
daily.objects <- data %>%
  group_by(sub_num) %>%
  summarize(n.objects = length(unique(object))) %>%
  ungroup() %>%
  summarize(mean = mean(n.objects), 
            median = median(n.objects), 
            sd = sd(n.objects), 
            min = min(n.objects),
            max = max(n.objects)) %>%
  mutate(across(mean:sd), round(., 2))

# ... by kid (incl. site info)
daily.objects.bysite <- data %>%
  group_by(sub_num, site) %>%
  summarize(n.objects = length(unique(object))) 

# t-test assumptions not met -> use wilcox
shapiro.test(subset(daily.objects.bysite,
                    daily.objects.bysite$site == "Rossel")$n.objects)
shapiro.test(subset(daily.objects.bysite,
                    daily.objects.bysite$site == "Tseltal")$n.objects)
var.test(n.objects ~ site, daily.objects.bysite)
comp.daily.objects <- tidy(wilcox.test(n.objects ~ site, daily.objects.bysite))

# rank objects according to how many kids handle them at least once
# w/in sites
ranked.objects.list <- list()
for (i in sites) {
  ranked.objects <- data %>%
    filter(site == i) %>%
    mutate(total.kids = length(unique(sub_num))) %>%
    group_by(object, total.kids) %>%
    summarize(n.kids = length(unique(sub_num)), 
              category = category) %>%
    distinct() %>%
    mutate(prop = n.kids/total.kids) %>%
    ungroup() %>%
    arrange(desc(prop)) %>%
    mutate(rank = row_number(), 
           site = i) 
  ranked.objects.list[[i]] <- ranked.objects
}
all.ranked.objects <- do.call(rbind, ranked.objects.list)

ros.objects <- all.ranked.objects %>%
  filter(site == "Rossel") %>%
  pull(object)

tse.objects <- all.ranked.objects %>%
  filter(site == "Tseltal") %>%
  pull(object)

# get top 25 objects for each site
ros.top.objects <- all.ranked.objects %>%
  filter(site == "Rossel" & rank <= 25) %>%
  pull(object)

tse.top.objects <- all.ranked.objects %>%
  filter(site == "Tseltal" & rank <= 25) %>%
  pull(object)

# compute percentage of kids that handled the camera at least once
camera <- all.ranked.objects %>%
  filter(object == "camera")

# compute percentage of images that include camera handling
camera.images <- data %>%
  group_by(sub_num, site) %>%
  mutate(n.images = length(unique(image)), 
         object = as_factor(object)) %>%
  group_by(sub_num, site, n.images) %>%
  count(object, .drop = FALSE) %>%
  filter(object == "camera") %>%
  summarize(prop = n/n.images) %>%
  distinct() %>%
  group_by(site) %>%
  summarize(min = min(prop), 
            max = max(prop), 
            mean = mean(prop), 
            median = median(prop))
  
# compute percentage of objects handled by only 1-2 kids in each site
uncommon.objects <- all.ranked.objects %>%
  group_by(site) %>%
  mutate(total = length(unique(object))) %>%
  filter(n.kids %in% 1:2) %>%
  group_by(site, total) %>%
  summarize(n = length(unique(object)), 
            prop = round(n/total*100, 2)) %>%
  distinct()


## ----zipfian-objects-fig, fig.env = "figure", fig.pos = "h", fig.align = "center", fig.width = 3.4, fig.height = 2, fig.cap = "Zipfian distribution of objects. Points reflect log-transformed proportion estimates for individual children."----

skew <- data %>%
  group_by(site, sub_num, object) %>%
  summarize(n.images = n()) %>%
  ungroup() %>%
  group_by(site, sub_num) %>%
  summarize(total = sum(n.images), 
            n.images = n.images,
            object = object) %>%
  distinct() %>%
  mutate(prop.images = log(n.images/total*100)) %>%
  arrange(sub_num, desc(prop.images)) %>%
  mutate(order = row_number()) %>%
  ungroup()

ggplot(skew, aes(x = order, y = prop.images, 
             fill = site, color = site)) +
    facet_wrap(. ~ site) +
    geom_point(aes(group = sub_num), alpha = 0.25, size = 0.5) +
    geom_smooth(se = FALSE, method = "loess", size = 0.5, color = "black") +
    scale_color_manual(values = site.colors) +
    scale_fill_manual(values = site.colors) +
    labs(x = "Objects Ranked by Frequency", y = "Log-scaled % Images", 
         color = "Site", fill = "Site") +
    theme_test(base_size = 8) +
    theme(legend.position = "none")


## ----top-objects-fig, fig.env = "figure*", fig.pos = "!ht", fig.align = "center", fig.width = 6.7, fig.height = 2.5, set.cap.width = TRUE, num.cols.cap = 2, fig.cap = "Non-study-related objects handled at least once by the most children in each site. Filled bars represent objects that were among the top 25 for both sites."----

all.ranked.objects %>%
  # remove study-related and recalculate ranks
  filter(object %notin% study.related) %>%
  group_by(site) %>%
  arrange(desc(prop)) %>%
  mutate(rank = row_number(), 
         # add var to indicate whether object appears in both sites
         both = ifelse(object %in% ros.top.objects &
                         object %in% tse.top.objects, 1, 0), 
         category.label = factor(category,
                                 levels = categories, labels = category.labels),
         label = paste0(str_to_sentence(str_remove(
           object, "-M|-W|empty drink ")), " (", category.label, ")"), 
         site = factor(site, levels = sites)) %>%
  filter(rank <= 25) %>%
  ggplot(aes(x = rank, y = prop*100, color = site, fill = site)) +
  facet_grid(. ~ site) +
  geom_bar(aes(alpha = as.factor(both)), stat = "identity") +
  geom_text(aes(y = prop*100/2, label = label),
            color = "black", srt = 90, size = 1.9) +
  scale_alpha_manual(values = c(0.2, 0.7)) +
  scale_color_manual(values = site.colors) +
  scale_fill_manual(values = site.colors) +
  labs(x = "Top 25 Objects", y = "% Children") +
  theme_test(base_size = 10) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        legend.position = "none")


## ----overall-category-stats---------------------------------------------------------------------
# compute the prevalence of category-level objects within kids 
# (i.e., proportion of holding by category for each kid)
category.props <- data %>%
  group_by(sub_num, site) %>%
  mutate(n.images = length(unique(image))) %>%
  group_by(sub_num, site, n.images) %>%
  count(category, .drop = FALSE) %>%
  summarize(prop = n/n.images, 
            category = category) %>%
  distinct()
  
# test for differences between sites for each category
wilcox.outputs <- list()
for (i in categories) {
  ros.input <- category.props %>%
    filter(category == i & site == "Rossel")
  
  tse.input <- category.props %>%
    filter(category == i & site == "Tseltal")
  
  wilcox.output <- tidy(wilcox.test(ros.input$prop, tse.input$prop))
  
  wilcox.outputs[[i]] <- wilcox.output
}

comp.category.bysite <- do.call(rbind, wilcox.outputs) %>%
  rownames_to_column(var = "category") %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm"))

# compute descriptive statistics of category prevalence by site and category
category.means <- category.props %>%
  group_by(category, site) %>%
  summarize(mean = mean(prop), 
            sd = sd(prop), 
            n = n(), 
            se = sd/sqrt(n))

# compute # kids with synthetic or food as their top category
top.category.bychild <- category.props %>%
  group_by(sub_num) %>%
  filter(prop == max(prop) & category %in% c("Synthetic", "Food")) %>%
  nrow()

# compute mean # of categories per hour overall
# (first over subs, then over sample)
hourly.categories <- data %>%
  group_by(sub_num, hour) %>%
  summarize(n.categories = length(unique(category))) %>%
  ungroup() %>%
  summarize(mean = round(mean(n.categories), 2))

hourly.categories.incl.zeroes <- data %>%
  group_by(site, sub_num, hour) %>%
  summarize(n.categories = length(unique(category))) %>%
  ungroup() %>%
  full_join(codeable.hrs.pp) %>%
  filter(sub_num %in% unique(data$sub_num)) %>%
  replace_na(list(n.categories = 0)) %>%
  summarize(mean = round(mean(n.categories), 2))


## ----hourly-object-stats------------------------------------------------------------------------
# compute mean # objects per hour overall
# (first over subs, then over sample)
hourly.objects <- data %>%
  group_by(sub_num, hour) %>%
  summarize(n.objects = length(unique(object))) %>%
  ungroup() %>%
  summarize(mean = mean(n.objects), 
            median = median(n.objects), 
            sd = sd(n.objects), 
            min = min(n.objects),
            max = max(n.objects)) %>%
  mutate(across(mean:sd), round(., 2))

hourly.objects.incl.zeroes <- data %>%
  group_by(sub_num, hour) %>%
  summarize(n.objects = length(unique(object))) %>%
  ungroup() %>%
  full_join(codeable.hrs.pp) %>%
  filter(sub_num %in% unique(data$sub_num)) %>%
  replace_na(list(n.objects = 0)) %>%
  summarize(mean = mean(n.objects), 
            median = median(n.objects), 
            sd = sd(n.objects), 
            min = min(n.objects),
            max = max(n.objects)) %>%
  mutate(across(mean:sd), round(., 2))

# prep tibble for models w/ dummy coded category cols
model.data <- data %>%
  group_by(site, sub_num) %>%
  mutate(n.images = length(unique(image))) %>%
  group_by(site, sub_num, n.images, hour, category) %>%
  summarize(n.objects = length(unique(object))) %>%
  ungroup() %>%
  mutate(synthetic = ifelse(category == "Synthetic", 1, 0), 
         natural = ifelse(category == "Natural", 1, 0), 
         food = ifelse(category == "Food", 1, 0), 
         mealtime = ifelse(category == "Mealtime Tool", 1, 0),
         work = ifelse(category == "Work Tool", 1, 0), 
         clothing = ifelse(category == "Clothing", 1, 0),
         toy = ifelse(category == "Toy", 1, 0), 
         immovable = ifelse(category == "Immovable", 1, 0))

codeable.hrs.pp.bycategory <- expand_grid(
  codeable.hrs.pp, tibble(category = categories))

model.data.incl.zeroes <- data %>%
  group_by(site, sub_num, hour) %>%
  # add number of handking images for each hour by participant
  mutate(n.images = length(unique(image))) %>%
  full_join(codeable.hrs.pp.bycategory) %>%
  # ensure that we didn't add any new ptcps not in data
  filter(sub_num %in% unique(data$sub_num)) %>%
  # add zeroes for hours with no handling images
  replace_na(list(n.images = 0)) %>%
  group_by(site, sub_num, n.images, hour, category) %>%
  # compute unique objects per hour
  mutate(n.objects.raw = ifelse(n.images == 0, 0, length(unique(object))), 
         # correct for the fact that NAs are getting counted as objects
         n.objects = ifelse(is.na(object), 0, n.objects.raw)) %>%
  ungroup() %>%
  select(site, sub_num, category, hour, n.images, n.objects) %>%
  distinct() %>%
  mutate(synthetic = ifelse(category == "Synthetic", 1, 0), 
         natural = ifelse(category == "Natural", 1, 0), 
         food = ifelse(category == "Food", 1, 0), 
         mealtime = ifelse(category == "Mealtime Tool", 1, 0),
         work = ifelse(category == "Work Tool", 1, 0), 
         clothing = ifelse(category == "Clothing", 1, 0),
         toy = ifelse(category == "Toy", 1, 0), 
         immovable = ifelse(category == "Immovable", 1, 0))

zero.spike <- model.data.incl.zeroes %>%
  ggplot(aes(x = n.objects)) + 
  geom_histogram(binwidth = 1)

# run lmers
# include only non-zero cases for now (see above zero.spike histogram for justification)
lmer.outputs <- list()
for (i in c(categories)) {
  
  lmer.output <- lmer(n.objects ~ site * eval(as.symbol(tolower(trimws(str_remove(i, "Tool"))))) + (1|sub_num), model.data) %>%
  tidy() %>% 
  filter(effect == "fixed" & term != "(Intercept)")
  
  lmer.outputs[[i]] <- lmer.output
}

hourly.objects.model <- do.call(rbind, lmer.outputs) %>%
  rownames_to_column(var = "category") %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm"), 
         category = str_remove(category, ".1|.2|.3")) %>%
  group_by(category) %>%
  mutate(row = row_number(), 
         term = case_when(
           row == 1 ~ "site", 
           row == 2 ~ tolower(category),
           row == 3 ~ paste0("site:", tolower(category)))) %>%
  filter(p.adjusted < 0.10) %>%
  mutate(p = ifelse(p.adjusted < 0.001, 
                    paste0("< 0.001"), paste0("= ", round(p.adjusted, 3))))

# compute mean rate of objects/hr per category per kid
hourly.objects.bycategory <- model.data %>%
  group_by(site, sub_num, category) %>%
  summarize(n.objects.bysub = mean(n.objects)) %>%
  ungroup() %>%
  mutate(category = factor(category, levels = categories))


## ----overall-stats-fig, fig.env = "figure", fig.pos = "!h", fig.align = "left", fig.width = 3.3, fig.height = 2.2, fig.cap = "Rate of unique objects handled per hour across object categories. Points reflect means for individual children. Asterisks indicate significant site-by-category interactions after correcting for multiple comparisons."----

# create fig showing unique objects per hr across categories and sites
ggplot(hourly.objects.bycategory,
             aes(x = category, y = n.objects.bysub, 
                 fill = site, color = site)) +
  geom_point(position = position_jitterdodge(
    dodge.width = 0.75, jitter.width = 0.15), size = 0.5, alpha = 0.5) +
  geom_boxplot(width = 0.75, outlier.shape = NA,
               alpha = 0.5, color = "black") +
  annotate(geom = "text", label = "*", x = "Immovable", y = 4, size = 6) +
  scale_color_manual(values = site.colors) + 
  scale_fill_manual(values = site.colors) +
  scale_x_discrete(labels = category.labels) + 
  labs(x = "Object Categories\n(ordered by overall handling frequency)", y = "Unique Objects/Hour",
       color = "Site", fill = "Site") +
  theme_classic(base_size = 8) +
  theme(plot.tag = element_text(face = "bold"), 
        legend.justification = c(1, 1),
        legend.position = c(1, 1), 
        legend.direction = "horizontal", 
        axis.text.x = element_text(size = 5))


## ----age-effects-bycategory---------------------------------------------------------------------
# compute prop of images in each category for each age
category.props.byage <- data %>%
  group_by(site, sub_num, age) %>%
  mutate(n.images = length(unique(image))) %>%
  group_by(sub_num, n.images, site, age, category) %>%
  summarize(n.category = n(), 
            prop = n.category/n.images) %>%
  distinct()

# model age effects w/ indiv models for each category
age.bycategory.effects.list <- list()
age.bycategory.effects.toplot.list <- list()
for (i in categories) {
  # no random intercepts for kids because n.images already captures variance b/t kids
  model <- lm(prop ~ site * age + n.images,
                filter(category.props.byage, category == i))
  
  # check collinearity -> low
  # (making sure age and n.images aren't too strongly correlated)
  performance::check_collinearity(model)
  car::vif(model)
  
  effects <- tidy(model) %>%
    mutate(category = i, 
           p.adjusted = p.adjust(p.value, method = "holm")) %>%
    filter(term != "(Intercept)" & p.value < 0.10)
  
  # get predicted values from the model for plotting
  effects.toplot <- ggpredict(model,
                              terms = c("site", "age [all]"),
                              type = "fixed", back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(category = i)
  
  age.bycategory.effects.list[[i]] <- effects
  age.bycategory.effects.toplot.list[[i]] <- effects.toplot
}
age.bycategory.effects <- do.call(rbind, age.bycategory.effects.list) %>%
  mutate(estimate = ifelse(estimate < 0.001, 
                           paste0("< 0.001"), paste0("= ", round(estimate, 3))))

age.bycategory.effects.toplot <- do.call(rbind, age.bycategory.effects.toplot.list) %>%
  mutate(category = factor(category, levels = categories), 
         age = as.numeric(as.character(age)))

# check correlation between age and number of images 
cor.data <- data %>%
  group_by(sub_num, age) %>%
  summarize(n.images = length(unique(image))) %>%
  mutate(age = as.numeric(as.character(age)))

cor <- cor.test(cor.data$age, cor.data$n.images) %>%
  tidy()


## ----age-objects--------------------------------------------------------------------------------
# calculate total # unique objects per kid
daily.objects.byage <- data %>%
  group_by(site, sub_num, age) %>%
  summarize(n.objects = length(unique(object))) %>%
  ungroup()

# model total # unique objects handled by site and age
model <- lm(n.objects ~ site * age, daily.objects.byage)

daily.objects.byage.effects <- tidy(model) %>%
    filter(term != "(Intercept)" & p.value < 0.10)

# calculate # unique objects per hour per kid
hourly.objects.byage <- data %>%
  group_by(site, sub_num, age, hour) %>%
  summarize(n.objects = length(unique(object))) %>%
  ungroup()

# model total # unique objects handled per hour by site and age
model <- lmer(n.objects ~ site * age + (1|sub_num), 
              hourly.objects.byage)

# increase in unique objects per hour with age
hourly.objects.byage.effects <- tidy(model) %>%
    filter(effect == "fixed" & term != "(Intercept)" & p.value < 0.10)

# get model predicted values for plotting
hourly.objects.byage.effects.toplot <- ggpredict(
  model, terms = c("site", "age [all]"), type = "fixed",
  back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(age = as.numeric(as.character(age)))

# compute # of transitions between objects per hour per kid
# and compare that to the total number of objects in each hour
# to get the relative number of object transitions ("rel.transitions")
hourly.object.transitions <- data %>%
  group_by(sub_num, image) %>%
  # first, calculate # categories per image
  mutate(n.objects = length(unique(object)), 
         all.objects = paste(object, collapse = ", ")) %>%
  ungroup() %>%
  select(site, sub_num, image, age, hour, n.objects, all.objects) %>%
  distinct() %>%
  # second, add cols w/ prev category and check if there's been a change
  # in held object---if so, mark it as a transition == 1
  mutate(prev.objects = lag(all.objects, 1), 
         same.child = ifelse(sub_num == lag(sub_num, 1), 1, 0), 
         transition = ifelse(all.objects != prev.objects &
                               same.child == 1, 1, 0)) %>%
  group_by(site, sub_num, age, hour) %>%
  summarize(n.transitions = sum(transition, na.rm = TRUE)) %>%
  left_join(hourly.objects.byage, by = c("site", "sub_num", "age", "hour")) %>%
  mutate(rel.transitions = n.transitions/n.objects) %>%
  filter(rel.transitions > 0)

# model number of hourly transitions between by site and age
model <- lmer(rel.transitions ~ site * age + (1|sub_num),
              hourly.object.transitions)

# marginal increase in object transitions per hour with age
hourly.object.transitions.byage.effects <- tidy(model) %>%
  filter(effect == "fixed" & term != "(Intercept)") %>%
  mutate(term = str_remove(term, "Rossel"))

# get model predicted values for plotting
hourly.object.transitions.byage.effects.toplot <- ggpredict(
  model, terms = c("site", "age [all]"), type = "fixed",
  back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(age = as.numeric(as.character(age)))


## ----age-categories-----------------------------------------------------------------------------
# calculate total # objects per category and prop objects per category, per kid
category.props.byage <- data %>%
  group_by(sub_num, site, age) %>%
  mutate(n.images = length(unique(image))) %>%
  group_by(sub_num, site, age, n.images, category) %>%
  summarize(n.category = n(), 
            prop = n.category/n.images) %>%
  ungroup() %>%
  distinct() %>%
  mutate(category = factor(category, levels = categories))
  
# model proportion of object handling by site and age
age.effects.list <- list()
age.effects.toplot.list <- list()
for (i in categories) {
  model <- lm(prop ~ site * age + n.images,
              filter(category.props.byage, category == i))
  
  effects <- tidy(model) %>%
    mutate(category = i, 
           p.adjusted = p.adjust(p.value, method = "holm")) %>%
    filter(term != "(Intercept)" & p.value < 0.10)
  
  # get model predicted values for plotting
  effects.toplot <- ggpredict(model, terms = c("site", "age [all]"),
                              type = "fixed", back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(category = i)
  
  age.effects.list[[i]] <- effects
  age.effects.toplot.list[[i]] <- effects.toplot
}

age.effects <- do.call(rbind, age.effects.list)
age.effects.toplot <- do.call(rbind, age.effects.toplot.list) %>%
  mutate(category = factor(category,
                              levels = categories))

# compute # unique categories per hour per kid
hourly.categories.byage <- data %>%
  group_by(site, sub_num, age, hour) %>%
  summarize(n.categories = length(unique(category))) %>%
  ungroup()

# model # categories per hour by site and age
model <- lmer(n.categories ~ site * age + (1|sub_num), hourly.categories.byage)

# increase in unique objects per hour with age
hourly.categories.byage.effects <- tidy(model) %>%
  filter(effect == "fixed" & term != "(Intercept)" & p.value < 0.10) %>%
  mutate(p = ifelse(p.value < 0.001, 
                    paste0("< 0.001"), paste0("= ", round(p.value, 3))))

# get model predicted values for plotting
hourly.categories.byage.effects.toplot <- ggpredict(
  model, terms = c("site", "age [all]"), type = "fixed",
  back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(age = as.numeric(as.character(age)))

# compute # unique categories per hour per kid
hourly.total.categories <- data %>%
  group_by(sub_num, hour) %>%
  summarize(n.categories = length(unique(category)))

# compute # of transitions between categories per hour per kid
# and compare that to the total number of categories in each hour
# to get the relative number of category transitions ("rel.transitions")
hourly.category.transitions <- data %>%
  group_by(sub_num, image) %>%
  # first, calculate # categories per image
  mutate(n.categories = length(unique(category)), 
         all.categories = paste(category, collapse = ", ")) %>%
  ungroup() %>%
  select(site, sub_num, image, age, hour, n.categories, all.categories) %>%
  distinct() %>%
  # second, add cols w/ prev category and check if there's been a change
  # in held object---if so, mark it as a transition == 1
  mutate(prev.categories = lag(all.categories, 1), 
         same.child = ifelse(sub_num == lag(sub_num, 1), 1, 0), 
         transition = ifelse(all.categories != prev.categories &
                               same.child == 1, 1, 0)) %>%
  group_by(site, sub_num, age, hour) %>%
  summarize(n.transitions = sum(transition, na.rm = TRUE)) %>%
  left_join(hourly.total.categories, by = c("sub_num", "hour")) %>%
  mutate(rel.transitions = n.transitions/n.categories) %>%
  filter(rel.transitions > 0)

# model number of hourly transitions between by site and age
model <- lmer(rel.transitions ~ site * age + (1|sub_num),
              hourly.category.transitions)

# increase in category transitions per hour with age
hourly.category.transitions.byage.effects <- tidy(model) %>%
    filter(effect == "fixed" & term != "(Intercept)") %>%
    mutate(term = str_remove(term, "Rossel"))

# get model predicted values for plotting
hourly.category.transitions.byage.effects.toplot <- ggpredict(
  model, terms = c("site", "age [all]"), type = "fixed",
  back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(age = as.numeric(as.character(age)))


## ----age-effects-fig, fig.env = "figure", fig.pos = "!ht", fig.align = "center", fig.width = 3.2, fig.height = 3.5, fig.cap = "(A) Unique objects and (B) object categories handled per hour as a function of age. (C) Relative number of transitions between objects and (D) object categories per hour as a function of age. Points reflect raw hourly counts for each child, and lines reflect model predictions with shaded standard error regions."----
# create fig showing increase in unique objects per hr with age
p1 <- ggplot() +
  # add trend line from lmer output
  geom_line(hourly.objects.byage.effects.toplot,
            mapping = aes(x = age, y = predicted, color = site), size = 1) +
  # add SE from lmer output
  geom_ribbon(hourly.objects.byage.effects.toplot,
              mapping = aes(x = age , ymin = predicted - conf.low,
                            ymax = predicted + conf.low, fill = site),
              alpha = 0.1) +
  # add points for individual kids
  geom_jitter(hourly.objects.byage,
              mapping = aes(x = age, y = n.objects,
                            color = site, fill = site), 
              alpha = 0.25, size = 0.5) +
  scale_color_manual(values = site.colors) +
  scale_fill_manual(values = site.colors) +
  scale_x_continuous(breaks = c(0, 12, 24, 36, 48)) +
  scale_y_continuous(limits = c(0, 30),breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  labs(x = "Age (months)", y = "Unique Objects/Hour",
       color = "Site", fill = "Site", tag = "A") +
  theme_test(base_size = 7)
# format so that only 1 legend appears at the bottom of combined fig
p1a <- p1 + theme(legend.position = "none", legend.direction = "horizontal", 
                  plot.tag = element_text(face = "bold"))
legend <- get_legend(p1 + theme(legend.position = "bottom"))
# create fig showing increase in number of categories per hr with age
p2 <- ggplot() +
  # add trend line from lmer output
  geom_line(hourly.categories.byage.effects.toplot,
            mapping = aes(x = age, y = predicted, color = site), size = 1) +
  # add SE from lmer output
  geom_ribbon(hourly.categories.byage.effects.toplot,
              mapping = aes(x = age , ymin = predicted - conf.low,
                            ymax = predicted + conf.low, fill = site),
              alpha = 0.1) +
  # add points for individual kids
  geom_jitter(hourly.categories.byage,
              mapping = aes(x = age, y = n.categories,
                            color = site, fill = site),
              alpha = 0.25, size = 0.5) +
  scale_color_manual(values = site.colors) +
  scale_fill_manual(values = site.colors) +
  scale_x_continuous(breaks = c(0, 12, 24, 36, 48)) +
  scale_y_continuous(limits = c(0, 10),breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(x = "Age (months)", y = "Unique Categories/Hour",
       color = "Site", fill = "Site", tag = "B") +
  theme_test(base_size = 7) +
  theme(legend.position = "none", plot.tag = element_text(face = "bold"))

p3 <- ggplot() +
  # add trend line from lmer output
  geom_line(hourly.object.transitions.byage.effects.toplot,
            mapping = aes(x = age, y = predicted, color = site), size = 1) +
  # add SE from lmer output
  geom_ribbon(hourly.object.transitions.byage.effects.toplot,
              mapping = aes(x = age , ymin = predicted - conf.low,
                            ymax = predicted + conf.low, fill = site),
              alpha = 0.1) +
  # add points for individual kids
  geom_jitter(hourly.object.transitions,
              mapping = aes(x = age, y = rel.transitions,
                            color = site, fill = site), 
              alpha = 0.25, size = 0.5) +
  scale_color_manual(values = site.colors) +
  scale_fill_manual(values = site.colors) +
  scale_x_continuous(breaks = c(0, 12, 24, 36, 48)) +
  labs(x = "Age (months)", y = "Object Transitions/Hour",
       color = "Site", fill = "Site", tag = "C") +
  theme_test(base_size = 7) +
  theme(legend.position = "none", plot.tag = element_text(face = "bold"))

p4 <- ggplot() +
  # add trend line from lmer output
  geom_line(hourly.category.transitions.byage.effects.toplot,
            mapping = aes(x = age, y = predicted, color = site), size = 1) +
  # add SE from lmer output
  geom_ribbon(hourly.category.transitions.byage.effects.toplot,
              mapping = aes(x = age , ymin = predicted - conf.low,
                            ymax = predicted + conf.low, fill = site),
              alpha = 0.1) +
  # add points for individual kids
  geom_jitter(hourly.category.transitions,
              mapping = aes(x = age, y = rel.transitions,
                            color = site, fill = site), 
              alpha = 0.25, size = 0.5) +
  scale_color_manual(values = site.colors) +
  scale_fill_manual(values = site.colors) +
  scale_x_continuous(breaks = c(0, 12, 24, 36, 48)) +
  scale_y_continuous(limits = c(0, 8),breaks = c(2, 4, 6, 8)) +
  labs(x = "Age (months)", y = "Category Transitions/Hour",
       color = "Site", fill = "Site", tag = "D") +
  theme_test(base_size = 7) +
  theme(legend.position = "none", plot.tag = element_text(face = "bold"))
# combine plots and do some formatting
# (i.e., align axes and shrink space between panels)
fig5 <- align_plots(p1a, p2, p3, p4, align = "hv", axis = "l")
panels <- plot_grid(ggdraw(fig5[[1]]), ggdraw(fig5[[2]]), 
                    ggdraw(fig5[[3]]), ggdraw(fig5[[4]]), 
                    nrow = 2, rel_heights = c(1, 1, 1, 1))
plot_grid(panels, legend, nrow = 2, rel_heights = c(1, .1))


## -----------------------------------------------------------------------------------------------
# References will be generated automatically by Pandoc and included here.
# The following code is some latex to format the bibliography. Do not remove it.

