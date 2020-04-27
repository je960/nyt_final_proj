#load necessary packages

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(plotly)
library(leaflet)
library(shinythemes)
library(usmap)
library(widgetframe)

county_data_shiny <- readRDS("county_data")
state_data_shiny <- readRDS("state_data")
yes_shiny <-readRDS("yes")

ui <- navbarPage(
    tags$b("Coronavirus Cases in the US"),
    
    theme = shinytheme("lumen"),
    
    #first tab shows coronavirus cases over time
    tabPanel("Model",
             fluidPage(
                 titlePanel("Modeling"),
                 sidebarLayout(
                     sidebarPanel(
                         
                         HTML('<script> document.title = "COVID-19 In America"; </script>'),
                         tags$head(tags$link(rel="shortcut icon", href="https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.poynter.org%2Freporting-editing%2F2020%2Fyouve-probably-seen-this-image-of-the-coronavirus-everywhere-what-is-it-exactly%2F&psig=AOvVaw3S076kS4qN7crlMrFUgzGY&ust=1588053753988000&source=images&cd=vfe&ved=0CAIQjRxqFwoTCLDe7rD3h-kCFQAAAAAdAAAAABAD")),
                         
                         p(tags$em("Be careful about the scale.")),
                         
                         #ask user to input state
                         
                         selectInput(inputId = 'state', 
                                     label = 'Choose a state:', 
                                     choice = levels(state_data_shiny$state)
                                     )
                         ),
                     
                     #output state plot and death plot
                     mainPanel(
                        plotOutput("yes"), br(),
                        plotlyOutput("stateplot"), br(), br(),
                        plotlyOutput("deathplot"))
                     )
                 )
             ),
    
    tabPanel("Discussion",
             titlePanel("Looking Ahead"),
             
             imageOutput("image", width = "100%", height = "100%"),
             p("Image from NYT", align = "center"),
             h1(tags$b("COVID-19 in America"), align = "center"),
             p(tags$em("Analysis of COVID-19 Data from NYT"), align = "center"),
             
             # Add in a fluidRow to center the main text and graphs on the first page.
             
             fluidRow(column(2), column(8, 
                                        
                                        # Add in text to introduce and explain the project.
                                        
                                        p("I have been parsing through the open NYT database in order to see for myself the impact of the coronavirus in the US.
                                          The data is meticulously kept by NYT journalists and was updated to reflect numbers as they stand today, Monday April 27th. 
                                          Though the Shinyapp is a rough draft now, I plan on doing much more with the data"),
                                        p("Firstly, the rest of the modeling page will feature a breakdown of data by county as well as by state. There will be a more robust US map, where the user can interact and see data by county, not just having a static map.
                                          I understand just seeing the data isn't enough. You could go to NYT and see a much better version. How do we add insight once we understand, roughly, what the state of the coronavirus in the US is? Move on to tab two!"),
                                        p("Secondy, I will do analysis on social factors that correlate to the rate of infection in the coronavirus. Using US Census data, I can see what factors best account for high spread in coronavirus or, alternatively, low efficiency in COVID-19 testing. 
                                          I will look at population density, racial makeup, income, and age. Are there other factors you think I should look at?"),
                                        p("Thirdly, I will do some analysis on looking at the rate of infection, in order to better visualize the curve the we all seem to be trying to get over."), br(), br(),
                                        
                    )
                )

             ),
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, let me tell you about myself."),
             h3("About Me"),
             p("My name is Jerrica Li and I study Comparative Literature. Until about a few days ago, I had no idea what any part of a Shinyapp was. Today you can view this mess I've made in r and Shiny!
             You can reach me at jerrica_li@college.harvard.edu."))
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #tab one, render plot of cases over time
    output$stateplot <- renderPlotly({
    
        title <- "Confirmed coronavirus cases"
    
        # Filter the dataset based on what state was selected
        choice <- state_data_shiny %>%
            filter(state == input$state)
    
        #Create plot of corona cases over time
        b <- ggplot(choice, aes(x = date, y = cases)) +
            geom_line() +
            geom_point() + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 70, hjust = 1), 
                  panel.grid.major = element_blank()) + 
            labs(title =paste("Confirmed Corona Cases for", input$state, sep = " "),  
                x = "Date", 
                y = "Confirmed Cases") +
            scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
            geom_area(mapping=aes(x=new_date), fill="#9898fb", alpha=.5) 
    
        #render
        b
        })
    
    #tab one, render plot of deaths over time
    output$deathplot <- renderPlotly({
        
        title <- "Confirmed coronavirus cases"
        
        # Return blank if no choice selected yet
        if (input$state == " ") {
            return()
            }
        
        # Filter the dataset based on what state was selected
        choice_2 <- state_data_shiny %>%
            filter(state == input$state) 

        #Create plot of deaths over time
        c <- ggplot(choice_2, aes(x = date, y = deaths)) +
            geom_line() +
            geom_point() + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 70, hjust = 1), 
                  panel.grid.major = element_blank()) + 
            labs(title =paste("Confirmed Deaths by Corona for", input$state, sep = " "),  
                 x = "Date", 
                 y = "Confirmed Cases") +
            scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
            geom_area(mapping=aes(x=new_date), fill="purple", alpha=.5) 
        
        #render
        c
        })
    
    output$yes <- renderPlot({
       plot_usmap(data = yes_shiny , values = "cases", region =  "counties", size = 0.05) + 
            theme(panel.background = element_rect(color = "white", fill = "white")) +
            scale_fill_continuous(low = "white", high = "blue4", name = "Positive Cases") +
            labs(title = paste("Positive Coronavirus Cases in the US"))

    },
    
    # Set the best height and width for aesthetic purposes
    
    height = 400,
    width = 800
    )

    # Load in the image for the top of front page
    
    output$image <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './cybor.jpg', 
             height = 400,
             width = 550, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)



