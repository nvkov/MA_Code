# Section -----------------------------------------------------------------
library(shiny)


# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)

# Define the overall UI
shinyUI(
  fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4,
             selectInput("year",
                         "Year:",
                         c("All",
                           unique(as.character(summary_leistung$year))))
      ),
      column(4,
             selectInput("Typ",
                         "Typ:",
                         c("All",
                           unique(as.character(summary_leistung$Typ))))
      ),
      column(4,
               selectInput("newLeistung",
                           "Leistung:",
                           c("All",
                             unique(as.character(summary_leistung$newLeistung))))
      ),
      column(4,
             selectInput("Kraftstoff",
                         "Kraftstoff:",
                         c("All",
                           unique(as.character(summary_leistung$Kraftstoff))))
      )
    ),
    # Create a new row for the table.
    fluidRow(
      DT::dataTableOutput("table")
    )
  )
)