#load necessary packages

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(plotly)

county_data_shiny <- readRDS("county_data")
state_data_shiny <- readRDS("state_data")


ui <- navbarPage(
    "Coronavirus Cases in the US",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Modeling"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(inputId = 'state', label = 'Choose a state:', choice = " "
                         )),
                     mainPanel(plotOutput("stateplot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$stateplot <- renderPlot({
    
    title <- "Confirmed coronavirus cases"
    
    # Return blank if no choice selected yet
    
    if (input$state == " ") {
        return()
    }
    
    # Filter the dataset based on what state was selected
    
    choice <- state_data_shiny %>%
        filter(state == input$state) 
    
    #Create plot of corona cases over time
    
    ggplot(choice, aes(x = new_date, y = cases)) +
        geom_line() +
        geom_point() + 
        theme_minimal() +

        theme(axis.text.x = element_text(angle = 70, hjust = 1), 
              panel.grid.major = element_blank()) + 
        labs(title =paste("Confirmed Corona Cases for", input$state, sep = ""),  
             x = "Date", 
             y = "Confirmed Cases") +
        scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
        geom_area(mapping=aes(x=new_date), fill="#9898fb", alpha=.5) 
    })
    
    #remember input$state
    
    
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



