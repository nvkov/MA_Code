library(shiny)

# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- load("C:/Users/Nk/Documents/Uni/MA/MA_Code/post_data_prep/variables_summary/summary_lesitung.Rdata")
    data<- get(data)
    if (input$year != "All") {
      data <- data[data$year == input$year,]
    }
    if (input$Typ != "All") {
      data <- data[data$Typ == input$Typ,]
    }
    if (input$Kraftstoff != "All") {
      data <- data[data$Kraftstoff == input$Kraftstoff,]
    }
    if (input$newLeistung != "All") {
      data <- data[data$newLeistung == input$newLeistung,]
    }
    data
  }))
  
})


