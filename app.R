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

raw.data <- read_csv("all-data.csv")

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


ui <- fluidPage(
  
  titlePanel("ImCo Shiny"),
  
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput(outputId = "Plot1")
      
   )
  )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    
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
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

