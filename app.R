library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggeffects)
library(lme4)

# WHILE STILL IN DEVELOPMENT:
# check for updates to markdown file -> regenerate and source R script each time
#knitr::purl("CogSci2022-giannino/CogSci2022-giannino.Rmd")
#source("CogSci2022-giannino.R")

# IMPORT DATA -------------------------------------------------------------

raw.data <- read_csv("data/casillas/all-data.csv")

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

# DEFINE FUNCTIONS -----------------------------------------------------------

# create tibble of top objects for each site either by kids or by images
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
        mutate(total.images = length(unique(image))) %>%
        group_by(object, sub_num, total.images) %>%
        summarize(n.images = length(unique(image)), 
                  category = category) %>%
        ungroup() %>%
        distinct() %>%
        ungroup() %>%
        complete(sub_num, object, fill = list(n.images = 0)) %>%
        mutate(prop.images = ifelse(n.images == 0, 0, n.images/total.images)) %>%
        complete(sub_num, object, fill = list(prop.images = 0)) %>%
        group_by(object) %>%
        summarize(prop = mean(prop.images), 
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

get_age_effects <- function(dv) {
  if (dv == "Unique Objects/Hour") {
    age_effects_input <- data %>%
      group_by(site, sub_num, age, hour) %>%
      summarize(y = length(unique(object))) %>%
      ungroup()
  }
  
  else if (dv == "Object Transitions/Hour") {
    hourly.objects.byage <- data %>%
      group_by(site, sub_num, age, hour) %>%
      summarize(n.objects = length(unique(object))) %>%
      ungroup()
    
    age_effects_input <- data %>%
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
      mutate(y = n.transitions/n.objects) %>%
      filter(y > 0)
  }
}

# create not in operator
`%notin%` <- Negate(`%in%`)


shinyApp(
  # define ui---
  ui <- 
    fluidPage(
      # add theme
      theme = "flatly",
      # add tabs
      navbarPage("",
               id = "inTabset",
               tabPanel("Home",
                        fluidRow(
                          h1("Sticks, leaves, buckets, and bowls: Distributional 
                             patterns of children’s at-home object handling in two 
                             subsistence societies (CogSci 2022)"),
                          h5("Kennedy Casey, Mary Elliott, Elizabeth Mickiewicz, 
                             Anapaula Silva Mandujano, Kimberly Shorter, Mara Duquette, 
                             Elika Bergelson, & Marisa Casillas"),
                          h2("Supporting Online Information"),
                          br(),
                          
                          # buttons to jump to other tabs
                          actionButton("go_objects", "Explore distributions of objects", class = "btn-success"),
                          actionButton("go_categories", "Explore effects of object categories", class = "btn-success"),
                          actionButton("go_age", "Explore effects of age", class = "btn-success"),
                          
                          # links to external pages
                          h2("External links"),
                          h4(img(src = "https://chatterlab.uchicago.edu/img/logo.png", height = "20px"), 
                             tags$a(href = "https://chatterlab.uchicago.edu/lab-publications/Casey_et_al_submitted_Distributional_patterns_of_at_home_object_handling.pdf", 
                                    "Read the full proceedings paper")), 
                          h4(img(src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png", height = "30px"), 
                             tags$a(href = "https://github.com/kennedycasey/daylong-object-ids", 
                                    "Find all data and code")), 
                          h4(img(src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png", height = "30px"),
                             tags$a(href = "https://github.com/kennedycasey/ImCo2", 
                                    "Access the annotation tool")),
                          br(),
                          br(),
                          h5("Built with",
                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                             "from",
                             img(src = "https://www.rstudio.com/assets/img/logo.svg", height = "30px"))
                        )
               ), 
               tabPanel("Objects",
                        sidebarLayout(
                        sidebarPanel(
                          h1("Top Objects"),
                          sliderInput("top_objects_count", 
                                      label = "Number of Objects", 
                                      min = 5, max = 50, 
                                      value = 25, step = 5),
                          radioButtons("top_objects_site", "Site",
                                       c("Rossel" = "Rossel", 
                                         "Tseltal" = "Tseltal")
                          ),
                          radioButtons("top_objects_dv", "DV",
                                       c("Overall % of children handling the target object at least once" = "% Children", 
                                         "Average % of photos featuring handling of the target object across children" = "% Photos")
                          ), 
                          selectInput("top_objects_category", "Category", 
                                      choices = c("All Categories", "Food", "Synthetic", 
                                                  "Natural", "Toy", "Mealtime Tool", 
                                                  "Clothing", "Immovable", 
                                                  "Work Tool"))
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs", 
                            tabPanel("Figure", 
                                        plotOutput("top_objects_fig"), 
                                        h5("Top objects defined based on either (a) the 
                                          number of children handling the object at least 
                                          once, or (b) the number of photos in which the
                                          object appeared (averaged across children. Filled 
                                          bars represent objects that were among the top 
                                          objects for both sites.")), 
                            tabPanel("Table", 
                                        dataTableOutput("top_objects_tbl"))
                        )))),
             tabPanel("Categories"), 
             tabPanel("Developmental Changes",
                      sidebarLayout(
                        sidebarPanel(
                          h1("Age Effects"),
                          selectInput("age_effects_dv", "DV", 
                                      choices = c("Unique Objects/Hour", "Object Transitions/Hour"))
                        ),
                        mainPanel(
                          plotOutput("age_effects_fig"), 
                                               h5("")), 
                          ),)
  )
  ),
  
  # define server logic---
  server <- function(input, output, session) {
  
    # jump to relevant tab after user presses button on homepage
    observeEvent(input$go_objects, {
      updateNavbarPage(session, inputId = "inTabset", 
                       selected = "Objects")
    })
    
    observeEvent(input$go_categories, {
      updateNavbarPage(session, inputId = "inTabset", 
                       selected = "Categories")
    })
    
    observeEvent(input$go_age, {
      updateNavbarPage(session, inputId = "inTabset", 
                       selected = "Developmental Changes")
    })
    
    # regenerate top objects tibble after any change to user inputs
    top_objects_input <- reactive({
      get_top_objects({ input$top_objects_dv })
    })
    
    # draw top objects figure with and without category labels
    output$top_objects_fig <- renderPlot({
      
      if ({ input$top_objects_category } == "All Categories") {
        top_objects_input() %>%
          # remove study-related and recalculate ranks
          filter(object %notin% study.related & site %in% { input$top_objects_site }) %>%
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
      }
      
      else {
        top_objects_input() %>%
          # remove study-related and recalculate ranks
          filter(object %notin% study.related & site %in% { input$top_objects_site } & category == { input$top_objects_category }) %>%
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
                   object, "-M|-W|empty drink "))), 
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
      }
    })
    
    # create top objects table
    output$top_objects_tbl <- renderDataTable({
      if ({ input$top_objects_category } == "All Categories") {
        top_objects_input() %>%
          # remove study-related and recalculate ranks
          filter(object %notin% study.related & site %in% { input$top_objects_site }) %>%
          arrange(desc(prop)) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= { input$top_objects_count }) %>%
          transmute(Object = str_remove(object, "-M|-W|empty drink "), 
                    Category = category, 
                    Rank = rank,
                    `DV %` = round(prop*100, 1))
      }
      else {
        top_objects_input() %>%
          # remove study-related and recalculate ranks
          filter(object %notin% study.related & site %in% { input$top_objects_site } & category == { input$top_objects_category }) %>%
          arrange(desc(prop)) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= { input$top_objects_count }) %>%
          transmute(Object = str_remove(object, "-M|-W|empty drink "), 
                    Category = category, 
                    Rank = rank,
                    `DV %` = round(prop*100, 1))
      }
      # set default table size to 10 with options in multiples of 5
    }, options = list(lengthMenu = seq(5, { input$top_objects_count }, 5), pageLength = 10))
    
    # regenerate top objects tibble after any change to user inputs
    age_effects_input <- reactive({
      get_age_effects({ input$age_effects_dv })
    })
    
    
    # draw top objects figure with and without category labels
    output$age_effects_fig <- renderPlot({
        ggplot() +
          # add points for individual kids
          geom_jitter(age_effects_input(),
                      mapping = aes(x = age, y = y,
                                    color = site, fill = site), 
                      alpha = 0.6, size = 3) +
        # add trend line from lmer output
        geom_line(ggpredict(
          lmer(y ~ site * age + (1|sub_num), 
               age_effects_input()), terms = c("site", "age [all]"), type = "fixed",
          back.transform = TRUE) %>%
            rename(age = group, site = x) %>%
            mutate(age = as.numeric(as.character(age))),
                  mapping = aes(x = age, y = predicted, color = site), size = 2) +
        # add SE from lmer output
        geom_ribbon(ggpredict(
          lmer(y ~ site * age + (1|sub_num), 
               age_effects_input()), terms = c("site", "age [all]"), type = "fixed",
          back.transform = TRUE) %>%
            rename(age = group, site = x) %>%
            mutate(age = as.numeric(as.character(age))),
                    mapping = aes(x = age , ymin = predicted - conf.low,
                                  ymax = predicted + conf.low, fill = site),
                    alpha = 0.1) +
          scale_color_manual(values = site.colors) +
          scale_fill_manual(values = site.colors) +
          scale_x_continuous(breaks = c(0, 12, 24, 36, 48)) +
          labs(x = "Age (months)", y = {input$age_effects_dv},
               color = "Site", fill = "Site") +
          theme_test(base_size = 25)
    })
  }
  
)