library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyjs)

# Verificar e instalar palmerpenguins se necessário
if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
  install.packages("palmerpenguins")
}
library(palmerpenguins)

# Load and prepare data
data("penguins")
penguins <- na.omit(penguins)

# Define UI
ui <- page_sidebar(
  useShinyjs(),
  title = "Análise dos Pinguins de Palmer",
  sidebar = sidebar(
    title = "Controles",
    
    # Checkboxes para espécies (limitado a 3 seleções)
    checkboxGroupInput("species", "Espécies (máximo 3):", 
                       choices = unique(as.character(penguins$species)),
                       selected = unique(as.character(penguins$species))[1:2]),
    
    # Checkboxes para ilhas (limitado a 3 seleções)
    checkboxGroupInput("island", "Ilhas (máximo 3):", 
                       choices = unique(as.character(penguins$island)),
                       selected = unique(as.character(penguins$island))[1:2]),
    
    selectInput("var_x", "Variável X (Contínua):", 
                choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                selected = "bill_length_mm"),
    
    selectInput("var_y", "Variável Y (Contínua):", 
                choices = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                selected = "body_mass_g"),
    
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
      card_header("Espécie vs Ilha"),
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
  
  # Observadores para limitar seleção a 3 espécies
  observeEvent(input$species, {
    if (length(input$species) > 3) {
      updateCheckboxGroupInput(session, "species", 
                               selected = tail(input$species, 3))
      html("warning_message", "Limite de 3 espécies atingido!")
    } else {
      html("warning_message", "")
    }
  })
  
  # Observadores para limitar seleção a 3 ilhas
  observeEvent(input$island, {
    if (length(input$island) > 3) {
      updateCheckboxGroupInput(session, "island", 
                               selected = tail(input$island, 3))
      html("warning_message", "Limite de 3 ilhas atingido!")
    } else {
      html("warning_message", "")
    }
  })
  
  # Filtered dataset based on user selections
  filtered_data <- reactive({
    data <- penguins
    
    if (length(input$species) > 0) {
      data <- data %>% filter(species %in% input$species)
    }
    
    if (length(input$island) > 0) {
      data <- data %>% filter(island %in% input$island)
    }
    
    return(data)
  })
  
  # Cross-tabulation table
  output$cross_table <- renderDT({
    # Create cross-tab for selected species and islands
    if (length(input$species) > 0 && length(input$island) > 0) {
      # Filter data for selected species and islands only
      filtered <- penguins %>% 
        filter(species %in% input$species, 
               island %in% input$island)
      
      # Create cross tab
      cross_tab <- table(filtered$species, filtered$island)
      cross_tab <- as.data.frame.matrix(cross_tab)
      
      # Add row totals
      cross_tab$Total <- rowSums(cross_tab)
      
      # Add column totals
      col_totals <- colSums(cross_tab)
      cross_tab <- rbind(cross_tab, Total = col_totals)
      
      datatable(cross_tab, 
                options = list(dom = 't', 
                               pageLength = 50),
                caption = "Tabela Cruzada: Espécie vs Ilha") %>%
        formatStyle(0, target = 'row', lineHeight = '80%')
    } else {
      # If no selections, show message
      datatable(data.frame(Mensagem = "Selecione pelo menos uma espécie e uma ilha"),
                options = list(dom = 't'))
    }
  })
  
  # Scatter plot with facets
  output$scatter_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      ggplot(data, aes_string(x = input$var_x, y = input$var_y, color = "species"),
             show.legend = FALSE) +
        geom_point(alpha = 0.7, size = 3, show.legend = FALSE) +
        facet_grid(species ~ island) +
        labs(x = input$var_x, y = input$var_y) +
        theme_bw() +
        theme(legend.position = "bottom")
    } else {
      # Empty plot if no data
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Sem dados para exibir") +
        theme_void()
    }
  })
  
  # Histogram for X variable with density
  output$hist_plot_x <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      ggplot(data, aes_string(x = input$var_x, fill = "species"),
             show.legend = FALSE) +
        geom_histogram(aes(y = after_stat(density)), alpha = 0.7, position = "identity", bins = 30,
                       show.legend = FALSE) +
        geom_density(alpha = 0.3, show.legend = FALSE) +
        facet_grid(species ~ island) +
        labs(x = input$var_x, y = "Densidade") +
        theme_bw() +
        theme(legend.position = "bottom")
    } else {
      # Empty plot if no data
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Sem dados para exibir") +
        theme_void()
    }
  })
  
  # Histogram for Y variable with density
  output$hist_plot_y <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) > 0) {
      ggplot(data, aes_string(x = input$var_y, fill = "species"),
             show.legend = FALSE) +
        geom_histogram(aes(y = after_stat(density)), alpha = 0.7, position = "identity", bins = 30,
                       show.legend = FALSE) +
        geom_density(alpha = 0.3, show.legend = FALSE) +
        facet_grid(species ~ island) +
        labs(x = input$var_y, y = "Densidade") +
        theme_bw() +
        theme(legend.position = "bottom")
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
