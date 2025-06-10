library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(tidyr)

# Load the mpg dataset
data(mpg)

# Get unique model names and faceting variables
unique_models <- unique(mpg$model)
facet_vars <- c("trans", "cyl", "class", "drv", "fl", "manufacturer")

ui <- page_sidebar(
  title = "MPG Dataset Explorer",
  sidebar = sidebar(
    title = "Controls",
    selectInput(
      "selected_models", 
      "Select car models to highlight:",
      choices = unique_models,
      multiple = TRUE
    ),
    selectInput(
      "facet_var", 
      "Facet by:",
      choices = facet_vars
    )
  ),
  
  card(
    card_header("Summary of Selected Models"),
    card_body(
      tableOutput("model_summary")
    )
  ),
  
  card(
    card_header("MPG over Time"),
    card_body(
      plotOutput("time_plot", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for filtered data
  selected_data <- reactive({
    if (length(input$selected_models) > 0) {
      mpg %>% 
        mutate(selected = model %in% input$selected_models)
    } else {
      mpg %>% 
        mutate(selected = FALSE)
    }
  })
  
  # Generate summary of selected models
  output$model_summary <- renderTable({
    if (length(input$selected_models) == 0) {
      return(data.frame(Message = "No models selected"))
    }
    
    mpg %>%
      filter(model %in% input$selected_models) %>%
      group_by(manufacturer, model) %>%
      summarize(
        Avg_City_MPG = mean(cty, na.rm = TRUE),
        Avg_Highway_MPG = mean(hwy, na.rm = TRUE),
        Engine_Size = mean(displ, na.rm = TRUE),
        Classes = paste(unique(class), collapse = ", "),
        Count = n(),
        .groups = "drop"
      ) %>%
      arrange(manufacturer, model)
  })
  
  # Generate the line plot for cty and hwy over time
  output$time_plot <- renderPlot({
    data <- selected_data()
    
    if (length(input$selected_models) == 0) {
      # If no models are selected, show a message
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Please select at least one model") + 
               theme_void())
    }
    
    # Get the facet variable
    facet_var <- input$facet_var
    
    # Filter only selected models and prepare data for plotting
    plot_data <- data %>%
      filter(selected) %>%
      group_by(model, year, .data[[facet_var]]) %>%
      summarize(
        avg_cty = mean(cty, na.rm = TRUE),
        avg_hwy = mean(hwy, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = c(avg_cty, avg_hwy),
        names_to = "metric",
        values_to = "mpg"
      ) %>%
      mutate(
        metric = ifelse(metric == "avg_cty", "City MPG", "Highway MPG")
      )
    
    # Convert facet variable to factor for proper faceting
    plot_data[[facet_var]] <- as.factor(plot_data[[facet_var]])
    
    # Create the line plot with faceting
    ggplot(plot_data, aes(x = year, y = mpg, color = model, linetype = metric, group = interaction(model, metric))) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      facet_wrap(as.formula(paste("~", facet_var))) +
      labs(
        title = paste("MPG over Time for Selected Models (Faceted by", facet_var, ")"),
        x = "Year",
        y = "Miles Per Gallon",
        color = "Model",
        linetype = "Metric"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.background = element_rect(fill = "lightblue", color = "black"),
        strip.text = element_text(face = "bold")
      )
  })
}

shinyApp(ui, server)
