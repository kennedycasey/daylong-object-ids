library(shiny)
library(shinythemes)

# WHILE STILL IN DEVELOPMENT:
# check for updates to markdown file -> regenerate and source R script each time
#knitr::purl("../CogSci2022-giannino/CogSci2022-giannino.Rmd")
#source("CogSci2022-giannino.R")

# IMPORT DATA -------------------------------------------------------------

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

# create list of study-related objects
study.related <- c("camera", "vest", "camera cover")

# FUNCTIONS ---------------------------------------------------------------
get_top_objects <- function(dv) {
  ranked.objects.list <- list()
  for (i in sites) {
    if (dv == "% Children") {
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
    if (dv == "% Photos") {
      ranked.objects <- data %>%
        filter(site == i) %>%
        group_by(sub_num) %>%
        mutate(photos = length(unique(image))) %>%
        group_by(object, sub_num, photos) %>%
        summarize(n.photos = length(unique(image)), 
                  category = category) %>%
        ungroup() %>%
        distinct() %>%
        ungroup() %>%
        complete(sub_num, object, fill = list(n.photos = 0)) %>%
        mutate(prop.photos = ifelse(n.photos == 0, 0, n.photos/photos)) %>%
        complete(sub_num, object, fill = list(prop.photos = 0)) %>%
        group_by(object) %>%
        summarize(prop = mean(prop.photos), 
               category = category) %>%
        ungroup() %>%
        filter(!is.na(category)) %>%
        distinct() %>%
        arrange(desc(prop)) %>%
        mutate(rank = row_number(), 
               site = i) 
      ranked.objects.list[[i]] <- ranked.objects
    }
  }
  all.ranked.objects <- do.call(rbind, ranked.objects.list)
}

# define operator that selects all but the things in a list
`%notin%` <- Negate(`%in%`)


shinyApp(
  ui <- fluidPage(
    theme = "flatly",
    navbarPage("ImCo", 
               tabPanel("Object Distribution", 
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("top_objects_dv", "Measure",
                                         c("% Children", "% Photos")
                            ),
                            radioButtons("top_objects_site", "Site",
                                         c("Rossel" = "Rossel", 
                                           "Tseltal" = "Tseltal")
                            ),
                            sliderInput("top_objects_count", 
                                        label = "Number of Objects", 
                                        min = 0, max = 50, 
                                        value = 25, step = 5)
                          ),
                          mainPanel(
                            plotOutput("top_objects_fig")
                          ))),
               tabPanel("Category Effects"), 
               tabPanel("Age Effects")
    )
  ),
  
  server <- function(input, output) {
    
    top_objects_input <- reactive({
      get_top_objects({ input$top_objects_dv })
    })
    
    output$top_objects_fig <- renderPlot({
      
      top_objects_input() %>%
        # remove study-related and recalculate ranks
        filter(object %notin% study.related, site %in% { input$top_objects_site }) %>%
        group_by(site) %>%
        arrange(desc(prop)) %>%
        mutate(rank = row_number(), 
               # add var to indicate whether object appears in both sites
               both = ifelse(object %in% 
                               filter(top_objects_input(), 
                                      site == "Rossel" & 
                                        rank <= { input$top_objects_count })$object 
                             & object %in% 
                               filter(top_objects_input(), 
                                      site == "Tseltal" & 
                                        rank <= { input$top_objects_count })$object, 
                             1, 0), 
               category.label = factor(category,
                                       levels = categories, labels = category.labels),
               label = paste0(str_to_sentence(str_remove(
                 object, "-M|-W|empty drink ")), " (", category.label, ")"), 
               site = factor(site, levels = sites)) %>%
        filter(rank <= { input$top_objects_count }) %>%
        ggplot(aes(x = rank, y = prop*100, color = site, fill = site)) +
        facet_grid(. ~ site) +
        geom_bar(aes(alpha = as.factor(both)), stat = "identity") +
        geom_text(aes(y = prop*100/2, label = label),
                  color = "black", srt = 90) +
        scale_alpha_manual(values = c(0.2, 0.7)) +
        scale_color_manual(values = site.colors) +
        scale_fill_manual(values = site.colors) +
        labs(x = paste0("Top ", { input$top_objects_count }, " Objects"), 
             y = { input$top_objects_dv }) +
        theme_test(base_size = 25) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
              legend.position = "none")
    })
  }
)