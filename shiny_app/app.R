library(shiny)
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

raw.data <- read_csv("../all-data.csv")

sites <- c("Rossel", "Tseltal")

site.colors <- c("Rossel" = "sandybrown",
                 "Tseltal" = "brown4")

categories <- c("Food", "Synthetic", "Natural", "Toy",
                "Mealtime Tool", "Clothing", "Immovable", 
                "Work Tool")

category.labels <- c("Food", "Synthetic", "Natural", "Toy",
                     "Tool-M", "Clothing", "Immovable", 
                     "Tool-W")

# rm excluded photos + set other variable info
data <- raw.data %>%
  filter(is.na(exclusion)) %>%
  mutate(hour = hour(timestamp),
         category = factor(category, levels = categories))

# set rm.study.related to TRUE if you want to check results w/o these
study.related <- c("camera", "vest", "camera cover")
# define operator that selects all but the things in a list
`%notin%` <- Negate(`%in%`)

rm.study.related = FALSE
if (rm.study.related) {
  data <- data %>%
    filter(object %notin% study.related)
}

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

ros.top.objects <- all.ranked.objects %>%
  filter(site == "Rossel" & rank <= 25) %>%
  pull(object)

tse.top.objects <- all.ranked.objects %>%
  filter(site == "Tseltal" & rank <= 25) %>%
  pull(object)

frame <- all.ranked.objects %>%
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
           object, "-M|-W|empty drink ")), " (", category.label, ")")) %>%
  filter(rank <= 25) 

hourly.total.categories <- data %>%
  group_by(sub_num, hour) %>%
  summarize(n.categories = length(unique(category)))

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

hourly.objects.bycategory <- model.data %>%
  group_by(site, sub_num, category) %>%
  summarize(n.objects.bysub = mean(n.objects)) %>%
  ungroup() %>%
  mutate(category = factor(category, levels = categories))

hourly.categories.byage <- data %>%
  group_by(site, sub_num, age, hour) %>%
  summarize(n.categories = length(unique(category))) %>%
  ungroup()


hourly.objects.byage <- data %>%
  group_by(site, sub_num, age, hour) %>%
  summarize(n.objects = length(unique(object))) %>%
  ungroup()

model <- lmer(n.objects ~ site * age + (1|sub_num), 
              hourly.objects.byage)

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

model <- lmer(rel.transitions ~ site * age + (1|sub_num),
              hourly.category.transitions)

hourly.category.transitions.byage.effects.toplot <- ggpredict(
  model, terms = c("site", "age [all]"), type = "fixed",
  back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(age = as.numeric(as.character(age)))

model <- lmer(n.objects ~ site * age + (1|sub_num), 
              hourly.objects.byage)

hourly.objects.byage.effects.toplot <- ggpredict(
  model, terms = c("site", "age [all]"), type = "fixed",
  back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(age = as.numeric(as.character(age)))

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

model <- lmer(rel.transitions ~ site * age + (1|sub_num),
              hourly.object.transitions)

hourly.object.transitions.byage.effects.toplot <- ggpredict(
  model, terms = c("site", "age [all]"), type = "fixed",
  back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(age = as.numeric(as.character(age)))

model <- lmer(n.categories ~ site * age + (1|sub_num), hourly.categories.byage)

hourly.categories.byage.effects.toplot <- ggpredict(
  model, terms = c("site", "age [all]"), type = "fixed",
  back.transform = TRUE) %>%
  rename(age = group, site = x) %>%
  mutate(age = as.numeric(as.character(age)))
  
figures <- c("Plot1", "Plot2", "Plot3", "Plot4")  


ui <- fluidPage(
  
  titlePanel("ImCo Shiny"),
  
    
    # Main panel for displaying outputs ----
    mainPanel(
      selectInput("plot", 'select plot', figures),
      plotOutput(outputId = 'selectedGraph')
      
   )
  )

# Define server logic required to draw a histogram ----
server <- shinyServer(function(input, output) {
  graph <- reactive({
    switch(input$plot,
           "Plot1"= plot1(),
           "Plot2"= plot2(),
           "Plot3"= plot3(),
           "Plot4"= plot4())
  })
  plot1 <- reactive({ggplot(skew, aes(x = order, y = prop.images, 
                                      fill = site, color = site)) +
      facet_wrap(. ~ site) +
      geom_point(aes(group = sub_num), alpha = 0.25, size = 0.5) +
      geom_smooth(se = FALSE, method = "loess", size = 0.5, color = "black") +
      scale_color_manual(values = site.colors) +
      scale_fill_manual(values = site.colors) +
      labs(x = "Objects Ranked by Frequency", y = "Log-scaled % Images", 
           color = "Site", fill = "Site") +
      theme_test(base_size = 8) +
      theme(legend.position = "none")})
  plot2 <- reactive({ggplot(frame, aes(x = rank, y = prop*100, color = site, fill = site)) +
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
  })
  plot3 <- reactive({ggplot(hourly.objects.bycategory,
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
            axis.text.x = element_text(size = 5))})
  
  
  plot4 <- reactive({p1 <- ggplot() +
    geom_line(hourly.objects.byage.effects.toplot,
              mapping = aes(x = age, y = predicted, color = site), size = 1) +
    geom_ribbon(hourly.objects.byage.effects.toplot,
                mapping = aes(x = age , ymin = predicted - conf.low,
                              ymax = predicted + conf.low, fill = site),
                alpha = 0.1) +
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
  p1a <- p1 + theme(legend.position = "none", legend.direction = "horizontal", 
                    plot.tag = element_text(face = "bold"))
  legend <- get_legend(p1 + theme(legend.position = "bottom"))
  p2 <- ggplot() +
    geom_line(hourly.categories.byage.effects.toplot,
              mapping = aes(x = age, y = predicted, color = site), size = 1) +
    geom_ribbon(hourly.categories.byage.effects.toplot,
                mapping = aes(x = age , ymin = predicted - conf.low,
                              ymax = predicted + conf.low, fill = site),
                alpha = 0.1) +
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
    geom_line(hourly.object.transitions.byage.effects.toplot,
              mapping = aes(x = age, y = predicted, color = site), size = 1) +
    geom_ribbon(hourly.object.transitions.byage.effects.toplot,
                mapping = aes(x = age , ymin = predicted - conf.low,
                              ymax = predicted + conf.low, fill = site),
                alpha = 0.1) +
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
    geom_line(hourly.category.transitions.byage.effects.toplot,
              mapping = aes(x = age, y = predicted, color = site), size = 1) +
    geom_ribbon(hourly.category.transitions.byage.effects.toplot,
                mapping = aes(x = age , ymin = predicted - conf.low,
                              ymax = predicted + conf.low, fill = site),
                alpha = 0.1) +
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
  fig5 <- align_plots(p1a, p2, p3, p4, align = "hv", axis = "l")
  panels <- plot_grid(ggdraw(fig5[[1]]), ggdraw(fig5[[2]]), 
                      ggdraw(fig5[[3]]), ggdraw(fig5[[4]]), 
                      nrow = 2, rel_heights = c(1, 1, 1, 1))
  plot_grid(panels, legend, nrow = 2, rel_heights = c(1, .1))})
  
  output$selectedGraph <- renderPlot({
    graph()
  })
  
})

# Create Shiny app ----
shinyApp(ui = ui, server = server)

