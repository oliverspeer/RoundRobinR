library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyverse)

# shinyapp -------------------------------------------------------------
# Define UI
ui <- fluidPage(
  navbarPage(
    title = div(img(src="logo_pos.png",  
                    height = 28, 
                    width = 130, 
                    style = "margin:1px 3px", "  Klinische Chemie ")
    ), 
    theme = shinytheme("paper"), 
    collapsible = TRUE,
    fluid = TRUE,
    
    tabPanel("Ringversuchsergebnisse", strong(" ", align = "center"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset_input", "Bitte wähle Datensatz:", 
                             choices = c("qmzh.data", "cscq.data")),
                 selectInput("substanz_input", "Bitte wähle Ringversuch:", 
                             choices = NULL)
               ),
               mainPanel(
                 plotOutput("boxplot")
               )
             )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Update selectInput choices based on selected dataset
  observeEvent(input$dataset_input, {
    data <- get(input$dataset_input)
    updateSelectInput(session, "substanz_input", 
                      choices = unique(data$substanz))
  })
  
  # Create boxplot
  output$boxplot <- renderPlot({
    data <- get(input$dataset_input)
    selected_substanz <- input$substanz_input
    
    # Filter data based on selected substanz
    filtered_data <- data[data$substanz == selected_substanz, ]
    
    # Sort data by mq_jahr and mq_quar (if columns exist)
    if (all(c("mq_jahr", "mq_quar") %in% colnames(filtered_data))) {
      sorted_data <- filtered_data[order(filtered_data$jahr, filtered_data$quar), ]
    } else {
      sorted_data <- filtered_data
    }
    
    # Calculate mean and standard deviation
    mean_value <- sorted_data$ziel
    sd_value <- sorted_data$toleranz
    
    # Create boxplot with points
    ggplot(sorted_data, aes(x = paste0(jahr, "Q", quar), y = ziel)) +
      geom_boxplot(width = 0.2, color = "red") +
      geom_errorbar(
        aes(
          ymin = mean_value - mean_value * sd_value / 100,
          ymax = mean_value + mean_value * sd_value / 100
        ),
        width = 0.0,
        color = "darkgrey",
        linewidth = 3.7
      ) +
      geom_point(aes(y = resultat), color = "blue", size = 2) +
      geom_text(aes(label = resultat), nudge_x = 0.2, size = 5) +
      coord_flip() +
      labs(
        x = "",
        y = paste(selected_substanz[1], sorted_data$einheit[1])
      ) +
      theme(
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "whitesmoke")
      )
  })
}

# Run the app
shinyApp(ui, server)
