library(shiny)
library(dplyr)
library(ggplot2)
library(pdftools)
library(stringr)

# Define UI
ui <- fluidPage(
  titlePanel("Proficiency Test Results"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload PDF File"),
      actionButton("submit", "Submit"),
      downloadButton("download", "Download Results")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", plotOutput("summary_plot")),
        tabPanel("Results", dataTableOutput("results_table"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create a reactive dataframe to store results
  results <- reactiveValues(data = data.frame(Name = character(),
                                              Value = numeric(),
                                              Unit = character(),
                                              `proz.Abweichung` = character(),
                                              Deviation = character(),
                                              stringsAsFactors = FALSE))
  
  # Extract data from the uploaded PDF file and update the results dataframe
  observeEvent(input$submit, {
    pdf_file <- input$file$datapath
    pdf_text <- pdf_text(pdf_file)|> str_split("\n")
    
    # Extract data using regular expressions
    pdf_text <- pdf_text[-2]
    pdf_text <- pdf_text[[1]][-1:-19]
    pdf_text <- pdf_text[-c(2,4,8:11,13,17:18,22:25,27,31:37)] |> str_squish()
    #pdf_text <- pdf_text[-c(2:4,9:13,18,23:27,32:37)] |> str_squish()
    
    df <- data.frame(
      Name = c(str_extract(pdf_text, "^[^0-9]+[A-Z]*[0-9]*\\s[^0-9]*")),
      Value = as.numeric(str_extract(pdf_text, "(?<=\\s|[^[:alnum:]])[0-9.]+")),
      Unit = str_extract(pdf_text, "(?<= )µmol/l|µg/l|ng/l(?=[ ,])"),
      `proz.Abweichung` = str_extract(pdf_text, "(?<=µmol/l|µg/l|ng/l|%)\\s*[±-]?\\s*[0-9.]+%"),
      Deviation = str_extract(pdf_text, "(?<=%)[^,]+")
    )
    colnames(df)[1] <- str_extract(pdf_text[1], "^[^,]+,?")
    df <- df[-1, ]
    print(df) 
    results$data <- bind_rows(results$data, df)
  })
  
  # Create a summary plot
  output$summary_plot <- renderPlot({
    ggplot(results$data, aes(x = Name, y = Value)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Summary of Proficiency Test Results",
           x = "Test",
           y = "Value")
  })
  
  # Create a table of results
  output$results_table <- renderDataTable({
    results$data
  })
  
  # Download the results as a CSV file
  output$download <- downloadHandler(
    filename = "proficiency_test_results.csv",
    content = function(file) {
      write.csv(results$data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
