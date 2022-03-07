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

figures <- c("Plot1", "Plot2")

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

Plot1 <- ggplot(skew, aes(x = order, y = prop.images, 
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
  
  Plot2 <- ggplot(frame, aes(x = rank, y = prop*100, color = site, fill = site)) +
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


ui <- fluidPage(
  
  titlePanel("ImCo Shiny"),
  
    
    # Main panel for displaying outputs ----
    mainPanel(
      selectInput("plot", 'select plot', figures),
      plotOutput(outputId = input$name)
      
   )
  )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  graph <- eventReactive(input$plot,{
    switch(input$plot,
           "Plot1"= Plot1,
           "Plot2"= Plot2)
  })
  output$plot <- renderPlot({
    graph()
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

