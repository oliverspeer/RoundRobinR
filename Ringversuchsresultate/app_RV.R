library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

# set working directory ----------------------------------------------------
setwd("C:/R_local/RoundRobinR")

# read data from several csv file
# containing pattern for file names
read_multiple_extqm_csv <- function(directory_path, file_pattern, df_name) {
  file_paths <- list.files(path = directory_path, pattern = file_pattern, full.names = TRUE)
  combined_df <- data.frame()
  
  for (file_path in file_paths) {
    df <- read.csv(file_path, header = T, sep = ";", dec = ".", fileEncoding = "Windows-1253")
    df <- df[,1:14]
    combined_df <- rbind(combined_df, df)
  }
  
  assign(df_name, combined_df, envir = .GlobalEnv)
  saveRDS(df_name, "RVDaten.rds")
}

# read data ------------------------------------------------------------
read_multiple_extqm_csv("c:/R_local/RoundRobinR", "4669_", "qmzh.data")


# shinyapp -------------------------------------------------------------
# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("substanz_input", "Bitte wähle Ringversuch:", 
                  choices = unique(qmzh.data$substanz))
    ),
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create boxplot
  output$boxplot <- renderPlot({
    selected_substanz <- input$substanz_input
    
    # Filter data based on selected substanz
    filtered_data <- qmzh.data[qmzh.data$substanz == selected_substanz, ]
    
    # Sort data by mq_jahr and mq_quar
    sorted_data <- filtered_data[order(filtered_data$mq_jahr, filtered_data$mq_quar), ]
    
    # Calculate mean and standard deviation
    mean_value <- sorted_data$ziel
    sd_value <- sorted_data$toleranz
    
    # Create boxplot with points
    
     ggplot(sorted_data, aes(x = paste0(mq_jahr, "Q", mq_quar), y = ziel)) +
      geom_boxplot(width = 0.2, color = "red") +
      #geom_point(color = "red", size = 3) +
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
      labs(#title = paste("Boxplot of", selected_substanz),
        x = "",
        y = paste(selected_substanz[1], sorted_data$einheit)) +
      theme(
        #plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        panel.background = element_rect(fill = "whitesmoke")
      )
    # Convert ggplot to plotly
    #ggplotly(p)
    
  })
}

# Run the app
shinyApp(ui, server)
