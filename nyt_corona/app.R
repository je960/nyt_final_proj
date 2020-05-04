#load necessary packages

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggthemes)
library(plotly)
library(leaflet)
library(shinythemes)
library(gganimate)
library(tidycensus)
library(viridis)

county_data_shiny <- readRDS("county_data")
state_data_shiny <- readRDS("state_data")
yes_shiny <-readRDS("yes")

# Census API Key

Sys.getenv("CENSUS_API_KEY")

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
                                     ),
                     br(),br(),
                     
                     #ask user to input state
                     uiOutput("countySelection")
                 ),
                     
                     #output state plot and death plot
                     mainPanel(
                        plotOutput("yes"), br(),br(),
                        h1(tags$b("Corona Virus Statistics by State"), align = "center"),
                        plotlyOutput("stateplot"), br(), br(),
                        plotlyOutput("deathplot"), br(), br(), br(), br(), br(), br(),
                        h1(tags$b("Corona Statistics by County"), align = "center"),
                        plotlyOutput("countyplot"), br(), br(),
                        plotlyOutput("countydeath"))
                     )
                 )
             ),
    
    tabPanel("Logarithmic Functions",
            titlePanel("Modeling with Logarithmic Functions"),
                    sidebarLayout(
                        sidebarPanel(
            
                            p(tags$em("Be careful about the scale.")),
                            
                            #ask user to input state
                            
                            selectizeInput(inputId = "selectize", label = "Select states to show on graph", choices = levels(state_data_shiny$state), multiple = TRUE,
                            options = list(maxItems = 10, placeholder = "States")),
                            br(),br(),

                            ),
    
                        mainPanel(
                            plotlyOutput("logplot")
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
    
    #updateSelectizeInput(inputId = "selectize", label = "Select states to show on graph", choices = data, server = TRUE)
    
    #change county input to dynamically change with chosen state
    #using Ui/uiOutput function to dynamically generate a selectInput
    
    output$countySelection <- renderUI({
        selectInput(inputId = 'county', 
                    label = 'Choose a county:', 
                    choice = unique(county_data$county[county_data$state == input$state]))
    })
    
    #model tab, state cases over time plot
    output$stateplot <- renderPlotly({
    
        title <- "Confirmed coronavirus cases by State"
    
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
            geom_area(aes(x=new_date), fill="royalblue2", alpha=.5)
    
        #render
        b
        })
    
    #model tab, state deaths over time plot
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
            geom_line(color = "orangered4") +
            geom_point(color = "orangered4") + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 70, hjust = 1), 
                  panel.grid.major = element_blank()) + 
            labs(title =paste("Confirmed Deaths by Corona for", input$state, sep = " "),  
                 x = "Date", 
                 y = "Confirmed Cases") +
            scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
            geom_area(aes(x=new_date), fill="orangered2", alpha=.5) 
            
        
        #render
        c
        })
    
    #model tab, county cases over time plot
    output$countyplot <- renderPlotly({
        
        title <- "Confirmed coronavirus cases by county"
        
        # Filter the dataset based on what state was selected
        choice <- county_data_shiny %>%
            filter(state == input$state,
                   county == input$county)
        
        #Create plot of corona cases over time
        b <- ggplot(choice, aes(x = date, y = cases)) +
            geom_line() +
            geom_point() + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 70, hjust = 1), 
                  panel.grid.major = element_blank()) + 
            labs(title =paste("Confirmed Corona Cases for", input$county, sep = " "),  
                 x = "Date", 
                 y = "Confirmed Cases") +
            scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
            geom_area(aes(x=new_date), fill="royalblue2", alpha=.5)
        
        #render
        b
    })
    
    #model tab, county deaths over time plot
    output$countydeath <- renderPlotly({
        
        title <- "Confirmed coronavirus cases by county"
        
        # Filter the dataset based on what state was selected
        choice <- county_data_shiny %>%
            filter(state == input$state,
                   county == input$county)
        
        #render plot
        c <- ggplot(choice, aes(x = date, y = deaths)) +
            geom_line(color = "orangered4") +
            geom_point(color = "orangered4") + 
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 70, hjust = 1), 
                  panel.grid.major = element_blank()) + 
            labs(title =paste("Confirmed Deaths by Corona for", input$state, sep = " "),  
                 x = "Date", 
                 y = "Confirmed Cases") +
            scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
            geom_area(aes(x=new_date), fill="orangered2", alpha=.5)
        
        #render
        c
    })
    
    output$yes <- renderPlot({
        #Require state selected or else error
        req(input$select_state)
        
        #filter county data
        x <- filter(county_data_shiny, state == input$state)
        
        #find latest date
        latest_date <- x %>% 
            arrange(desc(date)) %>% 
            slice(1) %>% 
            pull(date)
        
        #only pull latest date
        z <- filter(x, date == latest_date) 
        
        #nice
        z %>%
            ggplot(map = aes(fill = cases, geometry = geometry)) +
            geom_sf(data = z) + 
            scale_fill_viridis_c(option = "plasma") + 
            labs(caption = "Sources: The New York Times and the American Community Survey 2014-2018", 
                 fill = "Total Cases") + 
            theme_void()

    })
    
    #model tab, county cases over time plot
    output$logplot <- renderPlotly({
        

        # Filter the dataset based on what state was selected
        choice_1 <- state_data_shiny %>%
            filter(state == input$selectize[1])
        
        choice_two <- state_data_shiny %>%
            filter(state == input$selectize[2])
        
        #Create plot of corona cases over time
        b <- ggplot(choice_1, aes(x = date, y = cases)) +
            geom_line() +
            geom_point() + 
            geom_line(data = choice_two, aes(x = date, y = cases)) +
            geom_point(data = choice_two, aes(x = date, y = cases)) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 70, hjust = 1), 
                  panel.grid.major = element_blank()) + 
            labs(title =paste("Confirmed Corona Cases"),  
                 x = "Date", 
                 y = "Confirmed Cases") +
            scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
            geom_area(aes(x=new_date), fill="royalblue2", alpha=.5)
        
        #render
        b
    })

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



