#load necessary packages

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    mainPanel(
        imageOutput("preImage")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$preImage <- renderImage({
        
        
        
        # When input$n is 3, filename is ./images/image3.jpeg
        filename <- normalizePath(file.path('.',
                                            paste('washington', '.png', sep='')))
        
        
        # Return a list containing the filename and alt text
        list(src = filename,
             alt = paste("washington.png"), 
             width = 850, 
             height = 800)
        
    }, deleteFile = FALSE)
}



# Run the application 
shinyApp(ui = ui, server = server)

