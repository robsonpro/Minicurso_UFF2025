library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)

# Load and prepare data
data(mtcars)

# Adicionar rownames como coluna para facilitar o uso
mtcars$car_name <- rownames(mtcars)
rownames(mtcars) <- NULL

# Convertendo variáveis categóricas para fatores
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)

# Adicionar rótulos mais descritivos
mtcars$vs <- factor(mtcars$vs, levels = c("0", "1"), 
                    labels = c("V-engine", "Straight engine"))
mtcars$am <- factor(mtcars$am, levels = c("0", "1"), 
                    labels = c("Automatic", "Manual"))

# Definir quais são as variáveis categóricas e contínuas
categorical_vars <- c("cyl", "vs", "am", "gear", "carb")
continuous_vars <- c("mpg", "disp", "hp", "drat", "wt", "qsec")

# Define UI
ui <- page_sidebar(
  useShinyjs(),
  title = "Análise de Carros (Dataset mtcars)",
  sidebar = sidebar(
    title = "Controles",
    
    # Variáveis categóricas para facets e tabela cruzada
    selectInput("cat_var1", "Variável Categórica 1:", 
                choices = categorical_vars,
                selected = "cyl"),
    
    selectInput("cat_var2", "Variável Categórica 2:", 
                choices = categorical_vars,
                selected = "am"),
    
    # Checkboxes para valores da variável categórica 1 (limitado a 3 seleções)
    uiOutput("cat_var1_values"),
    
    # Checkboxes para valores da variável categórica 2 (limitado a 3 seleções)
    uiOutput("cat_var2_values"),
    
    selectInput("var_x", "Variável X (Contínua):", 
                choices = continuous_vars,
                selected = "mpg"),
    
    selectInput("var_y", "Variável Y (Contínua):", 
                choices = continuous_vars,
                selected = "hp"),
    
    # Mensagem de aviso para seleção limitada
    tags$div(id = "warning_message", 
             style = "color: red; margin-top: 10px; font-weight: bold;")
  ),
  
  layout_columns(
    card(
      card_header(uiOutput("scatter_title")),
      plotOutput("scatter_plot")
    ),
    card(
      card_header(uiOutput("cross_table_title")),
      DTOutput("cross_table")
    )
  ),
  
  layout_columns(
    card(
      card_header(uiOutput("hist_x_title")),
      plotOutput("hist_plot_x")
    ),
    card(
      card_header(uiOutput("hist_y_title")),
      plotOutput("hist_plot_y")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Gerar UI dinâmico para checkboxes da variável categórica 1
  output$cat_var1_values <- renderUI({
    var_values <- unique(mtcars[[input$cat_var1]])
    checkboxGroupInput("cat1_selected", 
                       paste("Valores de", input$cat_var1, "(máximo 3):"),
                       choices = var_values,
                       selected = var_values[1:min(2, length(var_values))])
  })
  
  # Gerar UI dinâmico para checkboxes da variável categórica 2
  output$cat_var2_values <- renderUI({
    var_values <- unique(mtcars[[input$cat_var2]])
    checkboxGroupInput("cat2_selected", 
                       paste("Valores de", input$cat_var2, "(máximo 3):"),
                       choices = var_values,
                       selected = var_values[1:min(2, length(var_values))])
  })
  
  # Títulos dinâmicos dos cards
  output$scatter_title <- renderUI({
    HTML(paste(input$var_x, "x", input$var_y))
  })
  
  output$hist_x_title <- renderUI({
    HTML(paste("Distribuição de", input$var_x))
  })
  
  output$hist_y_title <- renderUI({
    HTML(paste("Distribuição de", input$var_y))
  })
  
  output$cross_table_title <- renderUI({
    HTML(paste(input$cat_var1, "vs", input$cat_var2))
  })
  
  # Observadores para limitar seleção a 3 valores da variável categórica 1
  observeEvent(input$cat1_selected, {
    req(input$cat1_selected)
    if (length(input$cat1_selected) > 3) {
      updateCheckboxGroupInput(session, "cat1_selected", 
                               selected = tail(input$cat1_selected, 3))
      html("warning_message", paste("Limite de 3 valores de", input$cat_var1, "atingido!"))
    } else {
      html("warning_message", "")
    }
  })
  
  # Observadores para limitar seleção a 3 valores da variável categórica 2
  observeEvent(input$cat2_selected, {
    req(input$cat2_selected)
    if (length(input$cat2_selected) > 3) {
      updateCheckboxGroupInput(session, "cat2_selected", 
                               selected = tail(input$cat2_selected, 3))
      html("warning_message", paste("Limite de 3 valores de", input$cat_var2, "atingido!"))
    } else {
      html("warning_message", "")
    }
  })
  
  # Filtered dataset based on user selections
  filtered_data <- reactive({
    req(input$cat1_selected, input$cat2_selected)
    data <- mtcars
    
    if (length(input$cat1_selected) > 0) {
      data <- data %>% filter(.data[[input$cat_var1]] %in% input$cat1_selected)
    }
    
    if (length(input$cat2_selected) > 0) {
      data <- data %>% filter(.data[[input$cat_var2]] %in% input$cat2_selected)
    }
    
    return(data)
  })
  
  # Cross-tabulation table
  output$cross_table <- renderDT({
    req(input$cat1_selected, input$cat2_selected)
    # Create cross-tab for selected categorical variables
    if (length(input$cat1_selected) > 0 && length(input$cat2_selected) > 0) {
      # Filter data for selected values only
      filtered <- mtcars %>% 
        filter(.data[[input$cat_var1]] %in% input$cat1_selected, 
               .data[[input$cat_var2]] %in% input$cat2_selected)
      
      # Create cross tab
      cross_tab <- table(filtered[[input$cat_var1]], filtered[[input$cat_var2]])
      cross_tab <- as.data.frame.matrix(cross_tab)
      
      # Add row totals
      cross_tab$Total <- rowSums(cross_tab)
      
      # Add column totals
      col_totals <- colSums(cross_tab)
      cross_tab <- rbind(cross_tab, Total = col_totals)
      
      datatable(cross_tab, 
                options = list(dom = 't', 
                               pageLength = 50),
                caption = paste("Tabela Cruzada:", input$cat_var1, "vs", input$cat_var2)) %>%
        formatStyle(0, target = 'row', lineHeight = '80%')
    } else {
      # If no selections, show message
      datatable(data.frame(Mensagem = paste("Selecione pelo menos um valor para", 
                                            input$cat_var1, "e", input$cat_var2)),
                options = list(dom = 't'))
    }
  })
  
  # Scatter plot with facets
  output$scatter_plot <- renderPlot({
    req(input$cat1_selected, input$cat2_selected)
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      facet_formula <- as.formula(paste(input$cat_var1, "~", input$cat_var2))
      
      ggplot(data, aes_string(x = input$var_x, y = input$var_y, color = input$cat_var1)) +
        geom_point(alpha = 0.7, size = 3) +
        facet_grid(facet_formula) +
        labs(x = input$var_x, y = input$var_y) +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      # Empty plot if no data
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Sem dados para exibir") +
        theme_void()
    }
  })
  
  # Histogram for X variable with density
  output$hist_plot_x <- renderPlot({
    req(input$cat1_selected, input$cat2_selected)
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      facet_formula <- as.formula(paste(input$cat_var1, "~", input$cat_var2))
      
      ggplot(data, aes_string(x = input$var_x, fill = input$cat_var1)) +
        geom_histogram(aes(y = after_stat(density)), alpha = 0.7, position = "identity", bins = 20) +
        geom_density(alpha = 0.3) +
        facet_grid(facet_formula) +
        labs(x = input$var_x, y = "Densidade") +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      # Empty plot if no data
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Sem dados para exibir") +
        theme_void()
    }
  })
  
  # Histogram for Y variable with density
  output$hist_plot_y <- renderPlot({
    req(input$cat1_selected, input$cat2_selected)
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      facet_formula <- as.formula(paste(input$cat_var1, "~", input$cat_var2))
      
      ggplot(data, aes_string(x = input$var_y, fill = input$cat_var1))
      
      ggplot(data, aes_string(x = input$var_y, fill = input$cat_var1)) +
        geom_histogram(aes(y = after_stat(density)), alpha = 0.7, position = "identity", bins = 20) +
        geom_density(alpha = 0.3) +
        facet_grid(facet_formula) +
        labs(x = input$var_y, y = "Densidade") +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      # Empty plot if no data
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Sem dados para exibir") +
        theme_void()
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)