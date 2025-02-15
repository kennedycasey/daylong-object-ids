library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggeffects)
library(lme4)
library(plotly)
library(bslib)
library(showtext)

# IMPORT DATA -------------------------------------------------------------

raw.data <- read_csv("object-data.csv")

# pre-set colors and category labels
sites <- c("Tseltal", "Rossel")

site.colors <- c("Tseltal" = "sandybrown", 
                 "Rossel" = "brown4")

categories <- c("Consumable", "Synthetic", "Natural", "Toy",
                "Mealtime Tool", "Clothing", "Immovable", 
                "Work Tool")

category.labels <- c("Consumable", "Synthetic", "Natural", "Toy",
                     "Tool-M", "Clothing", "Immovable", 
                     "Tool-W")

category.labels.short <- c("C", "S", "N", "T",
                     "M", "CL", "I", 
                     "W")

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

# create tibble of ranked objects by site and kid

get_ranked_objects <- function(dv) {
  if (dv == "% Images") {
    ranked_objects_input <- data %>%
      filter(object %notin% study.related) %>%
      group_by(site, sub_num, age, object, category) %>%
      summarize(n.images = n()) %>%
      ungroup() %>%
      group_by(site, sub_num, age) %>%
      summarize(total = sum(n.images), 
                n.images = n.images,
                object = object, 
                category = category) %>%
      distinct() %>%
      mutate(prop = n.images/total*100) %>%
      arrange(sub_num, desc(prop)) %>%
      ungroup()
  }
  
  else {
    ranked_objects_input <- data %>%
      filter(object %notin% study.related) %>%
      group_by(site, sub_num, age, object, category) %>%
      summarize(n.images = n()) %>%
      ungroup() %>%
      group_by(site, sub_num, age) %>%
      summarize(total = sum(n.images), 
                n.images = n.images,
                object = object,
                category = category) %>%
      distinct() %>%
      mutate(prop = log(n.images/total*100)) %>%
      arrange(sub_num, desc(prop)) %>%
      ungroup()
    
  }
}

get_category_effects <- function(dv) {
  if (dv == "Unique Objects/Hour") {
    category_effects_input <- data %>%
      group_by(site, sub_num) %>%
      mutate(n.images = length(unique(image))) %>%
      group_by(site, sub_num, n.images, hour, category) %>%
      summarize(n.objects = length(unique(object))) %>%
      ungroup() %>%
      mutate(synthetic = ifelse(category == "Synthetic", 1, 0), 
             natural = ifelse(category == "Natural", 1, 0), 
             consumable = ifelse(category == "Consumable", 1, 0), 
             mealtime = ifelse(category == "Mealtime Tool", 1, 0),
             work = ifelse(category == "Work Tool", 1, 0), 
             clothing = ifelse(category == "Clothing", 1, 0),
             toy = ifelse(category == "Toy", 1, 0), 
             immovable = ifelse(category == "Immovable", 1, 0)) %>%
      group_by(site, sub_num, category) %>%
      summarize(y = mean(n.objects)) %>%
      ungroup() %>%
      mutate(category = factor(category, levels = categories))
  }
  
  else if (dv == "Overall % Photos") {
    category_effects_input <- data %>%
      group_by(sub_num, site) %>%
      mutate(n.images = length(unique(image))) %>%
      group_by(sub_num, site, n.images) %>%
      count(category, .drop = FALSE) %>%
      summarize(y = n/n.images*100, 
                category = category) %>%
      distinct()
  }
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
  else if (dv == "Categories/Hour") {
    age_effects_input <- data %>%
      group_by(site, sub_num, age, hour) %>%
      summarize(y = length(unique(category))) %>%
      ungroup()
  }
  else if (dv == "Category Transitions/Hour") {
    # compute # unique categories per hour per kid
    hourly.total.categories <- data %>%
      group_by(sub_num, hour) %>%
      summarize(n.categories = length(unique(category)))
    
    # compute # of transitions between categories per hour per kid
    # and compare that to the total number of categories in each hour
    # to get the relative number of category transitions ("rel.transitions")
    age_effects_input <- data %>%
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
      mutate(y = n.transitions/n.categories) %>%
      filter(y > 0)
  }
}

# create not in operator
`%notin%` <- Negate(`%in%`)

# create custom theme
custom <- bs_theme(bootswatch = "materia",
                   base_font = font_google("Helvetica"), 
                   font_scale = 1.25)

shinyApp(

# DEFINE UI ---------------------------------------------------------------
  ui <- 
    fluidPage(
      # add theme
      theme = custom,
      # add tabs
      navbarPage("",
               id = "inTabset",
               tabPanel("Home",
                        h1("Sticks, leaves, buckets, and bowls: Distributional 
                           patterns of children’s at-home object handling in two 
                           subsistence societies"),
                        br(),
                        h2("Data Visualization Tool"),
                        h5("Object-centric interactions provide rich learning 
                          moments for young children, including opportunities 
                          to discover word meanings. Children’s first-person 
                          object handling experiences, in particular, form a 
                          key source of input---one that varies across cultures 
                          and across development. Using daylong photo streams 
                          from child-worn cameras, we analyze >17k images to 
                          identify the frequency and targets of child object 
                          handling across the first four years in two small-scale 
                          subsistence farming communities on opposite sides of 
                          the globe (Rossel Papuan and Tseltal Mayan)."),
                        h5("The figures and tables on this site are associated with our", 
                          a(href = "https://chatterlab.uchicago.edu/lab-publications/Casey_et_al_submitted_Distributional_patterns_of_at_home_object_handling.pdf", 
                                  "CogSci 2022 paper.")), 
                        br(),
                        br(),
                        # buttons to jump to other tabs
                        actionButton("go_objects", 
                                     "Explore distributions of objects", 
                                     class = "btn-success", 
                                     style = 'padding:30px; font-size:120%'),
                        
                        actionButton("go_categories", 
                                     "Explore effects of object categories", 
                                     class = "btn-success", 
                                     style = 'padding:30px; font-size:120%'),
                        
                        actionButton("go_age", 
                                     "Explore effects of age", 
                                     class = "btn-success", 
                                     style = 'padding:30px; font-size:120%'),
                        br(),
                        br(),
                        
                        # links to external pages
                        h2("External links"),
                        h5(img(src = "https://chatterlab.uchicago.edu/img/logo.png", height = "20px"), 
                           a(href = "https://psyarxiv.com/yfnj4/", 
                                  "Read the full proceedings paper")), 
                        h5(img(src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png", height = "30px"), 
                           a(href = "https://github.com/kennedycasey/daylong-object-ids", 
                                  "Find all supporting data and code")), 
                        h5(img(src = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png", height = "30px"),
                           a(href = "https://github.com/kennedycasey/ImCo2", 
                                  "Access the image annotation tool")),
                        br(), 
                        
                        # links to email contact
                        h2("Author Contact"),
                        h5("Kennedy Casey (", a(href = "mailto:kbcasey@uchicago.edu", 
                                                   "kbcasey@uchicago.edu"), ")"),
                        h5("Marisa Casillas (", a(href = "mailto:mcasillas@uchicago.edu", 
                                                     "mcasillas@uchicago.edu"), ")"),
                        br(), 
                        
                        # add attribution
                        h2("Citation"), 
                        h5("Casey, K., Elliott, M., Mickiewicz, E., 
                           Silva Mandujano, A., Shorter, K., Duquette, M., 
                           Bergelson, E., & Casillas, M. (2022). Sticks, leaves, 
                           buckets, and bowls: Distributional patterns of children’s 
                           at-home object handling in two subsistence societies.", 
                          em("Proceedings of the 44th Annual Meeting of the Cognitive 
                           Science Society.")),
                        br(),
                        br(),
                        h5("Built with",
                           img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                           "from",
                           img(src = "https://www.rstudio.com/assets/img/logo.svg", height = "30px"))
               ), 
               navbarMenu("Objects",
                 tabPanel("Top Objects",
                          sidebarLayout(
                          sidebarPanel(
                            h2("Top Objects"),
                            sliderInput("top_objects_count", 
                                        label = "Number of Objects", 
                                        min = 5, max = 50, 
                                        value = 25, step = 5),
                            radioButtons("top_objects_site", "Site",
                                         c("Tseltal" = "Tseltal", 
                                           "Rossel" = "Rossel")
                            ),
                            radioButtons("top_objects_dv", "DV",
                                         c("Overall % of children handling the target object at least once" = "% Children", 
                                           "Average % of photos featuring handling of the target object across children" = "% Photos")
                            ), 
                            selectInput("top_objects_category", "Category", 
                                        choices = c("All Categories", "Consumable", "Synthetic", 
                                                    "Natural", "Toy", "Mealtime Tool", 
                                                    "Clothing", "Immovable", 
                                                    "Work Tool"))
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs", 
                              tabPanel("Figure", 
                                          plotlyOutput("top_objects_fig"), 
                                          h6("Top objects defined based on either the 
                                            number of children handling the object at least 
                                            once, or the number of photos in which the
                                            object appeared (averaged across children). Filled 
                                            bars represent objects that were among the top 
                                            objects for both sites."), 
                                          h6(paste0("Categories include consumables (",
                                                    "C), synthetic objects (",
                                                    "S), natural objects (",
                                                    "N), toys (", 
                                                    "T), mealtime tools (", 
                                                    "M), clothing (", 
                                                    "Cl), ", "immovable objects (", 
                                                    "I), and work tools (", "W)."))), 
                              tabPanel("Table", 
                                          dataTableOutput("top_objects_tbl"))
                          )))), 
                 tabPanel("Object Distribution",
                          sidebarLayout(
                            sidebarPanel(
                              h2("Object Distribution"),
                              sliderInput("ranked_objects_count", 
                                          label = "Number of Objects", 
                                          min = 5, max = 65, 
                                          value = 65, step = 5),
                              sliderInput("ranked_objects_age", 
                                          label = "Age Range", 
                                          min = 0, max = 48, 
                                          value = c(0, 48), step = 6),
                              selectInput("ranked_objects_dv", "DV",
                                           choices = c("Log-scaled % Images",
                                                       "% Images")
                              ), 
                              selectInput("ranked_objects_category", "Category", 
                                          choices = c("All Categories", "Consumable", "Synthetic", 
                                                      "Natural", "Toy", "Mealtime Tool", 
                                                      "Clothing", "Immovable", 
                                                      "Work Tool")), 
                              checkboxInput("ranked_objects_ptcps", 
                                            label = "Show individual participants", 
                                            value = FALSE, width = NULL)), 
                          mainPanel(
                            plotOutput("ranked_objects_fig"))
                          ))),
             tabPanel("Categories",
                       sidebarLayout(
                         sidebarPanel(
                           h2("Category Effects"),
                           selectInput("category_effects_site", "Site",
                                        c("Both" = "Both",
                                          "Tseltal" = "Tseltal", 
                                          "Rossel" = "Rossel")),
                           selectInput("category_effects_dv", "DV", 
                                       choices = c("Unique Objects/Hour", "Overall % Photos"))
                         ),
                         mainPanel(
                           plotOutput("category_effects_fig"), 
                           h5("")) 
                       )),
             tabPanel("Developmental Changes",
                      sidebarLayout(
                        sidebarPanel(
                          h2("Age Effects"),
                          selectInput("age_effects_dv", "DV", 
                                      choices = c("Unique Objects/Hour", "Object Transitions/Hour", "Categories/Hour", "Category Transitions/Hour"))
                        ),
                        mainPanel(
                          plotOutput("age_effects_fig"), 
                                               h5("")) 
                          ))
)),


# DEFINE SERVER LOGIC -----------------------------------------------------
  server <- function(input, output, session) {
  
    # jump to relevant tab after user presses button on homepage
    observeEvent(input$go_objects, {
      updateNavbarPage(session, inputId = "inTabset", 
                       selected = "Top Objects")
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
    
    output$top_objects_fig <- renderPlotly({
      
      if ({ input$top_objects_category } == "All Categories") {
        d <- top_objects_input() %>%
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
                                         levels = categories,
                                         labels = category.labels),
                 {if ({ input$top_objects_count > 30}) { category.label = factor(category,
                                                                                 levels = categories,
                                                                                 labels = category.labels.short)}},
                 label = paste0(str_to_sentence(str_remove(
                   object, "-M|-W|empty drink ")), " (", category.label, ")"), 
                 site = factor(site, levels = sites)) %>%
          filter(rank <= { input$top_objects_count }) %>%
          mutate(y = prop*100)
        
        p <- ggplot(d, aes(x = rank, y = prop*100, color = site, fill = site, text = label)) +
          facet_grid(. ~ site) +
          geom_bar(aes(alpha = as.factor(both)), stat = "identity") +
          scale_alpha_manual(values = c(0.2, 0.7)) +
          scale_color_manual(values = site.colors) +
          scale_fill_manual(values = site.colors) +
          labs(x = paste0("Top ", { input$top_objects_count }, " Objects"), 
               y = { input$top_objects_dv }) +
          theme_test(base_size = 25) +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
                legend.position = "none", text = element_text(family = "Helvetica"))
      }
      
      else {
        d <- top_objects_input() %>%
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
                 label = paste0(str_to_sentence(str_remove(
                   object, "-M|-W|empty drink "))), 
                 site = factor(site, levels = sites)) %>%
          filter(rank <= { input$top_objects_count }) %>%
          mutate(y = prop*100)
        
        p <- ggplot(d, aes(x = rank, y = prop*100, color = site, fill = site, text = label)) +
          facet_grid(. ~ site) +
          geom_bar(aes(alpha = as.factor(both)), stat = "identity") +
          scale_alpha_manual(values = c(0.2, 0.7)) +
          scale_color_manual(values = site.colors) +
          scale_fill_manual(values = site.colors) +
          labs(x = paste0("Top ", { input$top_objects_count }, " Objects"), 
               y = { input$top_objects_dv }) +
          theme_test(base_size = 25) +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
                legend.position = "none", text = element_text(family = "Helvetica"))
      }
      
      ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = FALSE) %>%
        add_annotations(x = d$rank,
                    y = d$y/2,
                    text = d$label, 
                    showarrow = FALSE,
                    font = list(size = ifelse({ input$top_objects_count } >= 45, 4.5,
                                              ifelse({input$top_objects_count} >= 35, 5.75,
                                                     ifelse({input$top_objects_count} >= 25, 6,
                                                            ifelse({input$top_objects_count} == 20, 6.45,
                                                                   ifelse({input$top_objects_count} == 15, 8.25, 13.5)))))), 
                    textangle = -90)
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
    
    
    # regenerate ranked objects tibble after any change to user inputs
    ranked_objects_input <- reactive({
      get_ranked_objects({ input$ranked_objects_dv })
    })
    
    # draw ranked objects figure
    output$ranked_objects_fig <- renderPlot({
      
      if ({ input$ranked_objects_category } == "All Categories") {
        d <- ranked_objects_input() %>%
          group_by(site, sub_num, age) %>%
          arrange(desc(prop)) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= { input$ranked_objects_count } & 
                   age >= min({ input$ranked_objects_age }) & 
                   age <= max({ input$ranked_objects_age }))
      }
      
      else {
        d <- ranked_objects_input() %>%
          filter(category == { input$ranked_objects_category } & 
                   age >= min({ input$ranked_objects_age}) &
                   age <= max({ input$ranked_objects_age })) %>%
          group_by(site, sub_num, age) %>%
          arrange(desc(prop)) %>%
          mutate(rank = row_number()) %>%
          filter(rank <= { input$ranked_objects_count })
      }
      
      if ({ input$ranked_objects_ptcps }) {
        ggplot(d, aes(x = rank, y = prop, color = site, fill = site)) +
          facet_grid(. ~ site) +
          geom_smooth(data = (d %>% group_by(sub_num) %>% 
                        mutate(n_distinct = length(unique(rank))) %>% filter(n_distinct > 2)), 
                      aes(x = rank, y = prop, color = site, group = sub_num), 
                      se = FALSE, method = "loess", 
                      alpha = 0.25, size = 0.75) +
          geom_smooth(se = FALSE, method = "loess", size = 1.5, color = "black") +
          scale_color_manual(values = site.colors) +
          scale_fill_manual(values = site.colors) +
          labs(x = "Objects Ranked by Frequency", y = { input$ranked_objects_dv }, 
               color = "Site", fill = "Site") +
          theme_test(base_size = 25) +
          theme(legend.position = "none", text = element_text(family = "Helvetica"))
      } 
      
      else {
        ggplot(d, aes(x = rank, y = prop, color = site, fill = site)) +
          facet_grid(. ~ site) +
          geom_jitter(aes(group = sub_num), alpha = 0.25, size = 2) +
          geom_smooth(se = FALSE, method = "loess", size = 1.5, color = "black") +
          scale_color_manual(values = site.colors) +
          scale_fill_manual(values = site.colors) +
          labs(x = "Objects Ranked by Frequency", y = { input$ranked_objects_dv }, 
               color = "Site", fill = "Site") +
          theme_test(base_size = 25) +
          theme(legend.position = "none", text = element_text(family = "Helvetica"))
      }
      
    })
    
    # regenerate age effects tibble after any change to user inputs
    age_effects_input <- reactive({
      get_age_effects({ input$age_effects_dv })
    })
    
    
    # draw age effects figure
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
          theme_test(base_size = 25) + 
          theme(text = element_text(family = "Helvetica"))
    })
    
    # regenerate categories tibble after any change to user inputs
    category_effects_input <- reactive({
      get_category_effects({ input$category_effects_dv })
    })
    
    # draw category effects figure
    output$category_effects_fig <- renderPlot({
      
      if ({ input$category_effects_site} != "Both") {
        category_effects_input <- category_effects_input() %>%
          filter(site %in% { input$category_effects_site })
      }
      else {
        category_effects_input <- category_effects_input()
      }
      ggplot(category_effects_input,
               aes(x = category, y = y, 
                   fill = site, color = site)) +
        geom_point(position = position_jitterdodge(
          dodge.width = 0.75, jitter.width = 0.25), size = 3, alpha = 0.5) +
        geom_boxplot(width = 0.75, outlier.shape = NA,
                     alpha = 0.5, color = "black", size = 1) +
        scale_color_manual(values = site.colors) + 
        scale_fill_manual(values = site.colors) +
        scale_x_discrete(labels = category.labels) + 
        labs(x = "Categories", y = { input$category_effects_dv },
             color = "Site", fill = "Site") +
        theme_classic(base_size = 25) +
        theme(plot.tag = element_text(face = "bold"), 
              legend.justification = c(1, 1),
              legend.position = c(1, 0.9), 
              legend.direction = "horizontal", 
              axis.text.x = element_text(size = 15), 
              text = element_text(family = "Helvetica")) + 
        {if ({ input$category_effects_site } != "Both" ) theme(legend.position = "none")}
    })
  }
)