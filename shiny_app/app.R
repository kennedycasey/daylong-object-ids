library(shiny)

# WHILE STILL IN DEVELOPMENT:
# check for updates to markdown file -> regenerate and source R script each time
knitr::purl("../CogSci2022-giannino/CogSci2022-giannino.Rmd")
source("CogSci2022-giannino.R")

# copy code for unnamed fig -> this can be fixed later!
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
           "Plot1" = plot1(),
           "Plot2" = plot2(),
           "Plot3" = plot3(),
           "Plot4" = plot4())
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
