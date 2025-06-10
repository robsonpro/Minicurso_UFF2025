library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(tidyr)

# Define distributions and their parameters
distributions <- list(
  "Normal" = list(
    params = c("mean" = 0, "sd" = 1),
    r_func = function(n, params) rnorm(n, mean = params$mean, sd = params$sd),
    d_func = function(x, params) dnorm(x, mean = params$mean, sd = params$sd),
    range = function(params) c(params$mean - 4*params$sd, params$mean + 4*params$sd)
  ),
  "Uniform" = list(
    params = c("min" = 0, "max" = 1),
    r_func = function(n, params) runif(n, min = params$min, max = params$max),
    d_func = function(x, params) dunif(x, min = params$min, max = params$max),
    range = function(params) c(params$min - 0.1, params$max + 0.1)
  ),
  "Exponential" = list(
    params = c("rate" = 1),
    r_func = function(n, params) rexp(n, rate = params$rate),
    d_func = function(x, params) dexp(x, rate = params$rate),
    range = function(params) c(0, 5/params$rate)
  ),
  "Gamma" = list(
    params = c("shape" = 2, "rate" = 1),
    r_func = function(n, params) rgamma(n, shape = params$shape, rate = params$rate),
    d_func = function(x, params) dgamma(x, shape = params$shape, rate = params$rate),
    range = function(params) c(0, qgamma(0.995, shape = params$shape, rate = params$rate))
  ),
  "Beta" = list(
    params = c("shape1" = 2, "shape2" = 2),
    r_func = function(n, params) rbeta(n, shape1 = params$shape1, shape2 = params$shape2),
    d_func = function(x, params) dbeta(x, shape1 = params$shape1, shape2 = params$shape2),
    range = function(params) c(0, 1)
  ),
  "Lognormal" = list(
    params = c("meanlog" = 0, "sdlog" = 1),
    r_func = function(n, params) rlnorm(n, meanlog = params$meanlog, sdlog = params$sdlog),
    d_func = function(x, params) dlnorm(x, meanlog = params$meanlog, sdlog = params$sdlog),
    range = function(params) c(0, qlnorm(0.995, meanlog = params$meanlog, sdlog = params$sdlog))
  )
)

ui <- page_sidebar(
  title = "Continuous Distributions Explorer",
  sidebar = sidebar(
    title = "Controls",
    
    # Distribution selection
    selectInput(
      "dist_type", 
      "Select Distribution:",
      choices = names(distributions)
    ),
    
    # Dynamic parameter inputs
    uiOutput("param_inputs"),
    
    # Sample size for random generation
    numericInput(
      "sample_size", 
      "Sample Size:",
      value = 1000,
      min = 10,
      max = 10000
    ),
    
    # Number of bins for histogram
    numericInput(
      "bins", 
      "Number of Histogram Bins:",
      value = 30,
      min = 5,
      max = 100
    ),
    
    # Generate button
    actionButton("generate", "Generate Random Sample", class = "btn-primary"),
    
    # Download button
    downloadButton("download_data", "Download Sample as CSV")
  ),
  
  # Main content area
  card(
    card_header("Probability Density Function (PDF)"),
    card_body(
      plotOutput("pdf_plot", height = "300px")
    )
  ),
  
  card(
    card_header("Random Sample Histogram"),
    card_body(
      plotOutput("hist_plot", height = "300px")
    )
  ),
  
  card(
    card_header("Sample Data Preview"),
    card_body(
      tableOutput("data_preview")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive for current distribution info
  current_dist <- reactive({
    distributions[[input$dist_type]]
  })
  
  # Dynamic parameter inputs based on selected distribution
  output$param_inputs <- renderUI({
    dist <- current_dist()
    param_list <- dist$params
    
    param_inputs <- lapply(names(param_list), function(param) {
      default_value <- param_list[[param]]
      numericInput(
        paste0("param_", param),
        label = paste0(param, ":"),
        value = default_value
      )
    })
    
    do.call(tagList, param_inputs)
  })
  
  # Get current parameter values
  get_params <- reactive({
    dist <- current_dist()
    param_names <- names(dist$params)
    
    params <- lapply(param_names, function(param) {
      input_name <- paste0("param_", param)
      if (!is.null(input[[input_name]])) {
        input[[input_name]]
      } else {
        dist$params[[param]]  # Use default if not yet available
      }
    })
    
    names(params) <- param_names
    return(params)
  })
  
  # Generate random sample (reactive)
  random_sample <- reactiveVal(NULL)
  
  observeEvent(input$generate, {
    dist <- current_dist()
    params <- get_params()
    n <- input$sample_size
    
    sample_data <- dist$r_func(n, params)
    random_sample(sample_data)
  })
  
  # PDF Plot
  output$pdf_plot <- renderPlot({
    dist <- current_dist()
    params <- get_params()
    
    x_range <- dist$range(params)
    x <- seq(x_range[1], x_range[2], length.out = 500)
    y <- dist$d_func(x, params)
    
    df <- data.frame(x = x, y = y)
    
    # Format parameters for title
    param_text <- paste(names(params), unlist(params), sep = " = ", collapse = ", ")
    
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "blue", linewidth = 1) +
      labs(
        title = paste(input$dist_type, "Distribution PDF"),
        subtitle = paste("Parameters:", param_text),
        x = "Value",
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })
  
  # Histogram of random sample
  output$hist_plot <- renderPlot({
    sample_data <- random_sample()
    
    if (is.null(sample_data)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Click 'Generate Random Sample' to view histogram") + 
               theme_void())
    }
    
    # Format parameters for title
    params <- get_params()
    param_text <- paste(names(params), unlist(params), sep = " = ", collapse = ", ")
    
    ggplot(data.frame(value = sample_data), aes(x = value)) +
      geom_histogram(bins = input$bins, fill = "skyblue", color = "white", alpha = 0.7) +
      labs(
        title = paste("Histogram of", input$dist_type, "Random Sample"),
        subtitle = paste("Parameters:", param_text, "| n =", length(sample_data)),
        x = "Value",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })
  
  # Sample data preview
  output$data_preview <- renderTable({
    sample_data <- random_sample()
    
    if (is.null(sample_data)) {
      return(data.frame(Message = "Generate a sample to see preview"))
    }
    
    df <- data.frame(value = sample_data)
    head(df, 10)
  })
  
  # Download handler for CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$dist_type, "_sample_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      sample_data <- random_sample()
      
      if (!is.null(sample_data)) {
        df <- data.frame(value = sample_data)
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
