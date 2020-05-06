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

county_data_shiny <- readRDS("county_data")
state_data_shiny <- readRDS("state_data")
county_map_data <- readRDS("county_map_data.rds")
leafletmap_shiny <- readRDS("leafmap.rds")


ui <- navbarPage(
    tags$b("Coronavirus Upclose"),
    
    theme = shinytheme("journal"),
    
    #Project Panel
    tabPanel("The Project",
             
             tabsetPanel(
                 
             #first Project tab, Overview
             tabPanel("Overview",
             
             imageOutput("image", width = "100%", height = "100%"),
             
             p("Image from NYT", align = "center"),
             
             h1(tags$b("The State of COVID-19"), align = "center"),
             
             p(tags$em("On the day I am writing this, Wednesday May 6th, the headlines of major US news are:"), align = "center"),

             imageOutput("headline", width = "100%", height = "100%"),
             
             # Add in a fluidRow to center the main text and graphs on the first page.
             fluidRow(column(2), column(8, 
                                        p("These headlines are a snapshot of the fear carried along with the spread of the coronavirus - ramifications that impact the public health, economy, and sanity of the US. Looking to our hospitals, the US alone currently 
                                          has over 1.2 million confirmed cases of COVID-19 and over 70,000 deaths reported deaths from the virus. Looking to our streets, cities, and states, most of the country, about 214 million people or ~ 65 % of the population, is in lockdown to prevent further spread of the virus."),
                                        p("Unemployment statistics which come out on Friday are expected to show unemployment rose to about 16.1% and a loss of over 22 million nonfarm payroll jobs. That is the same as eliminating every new job created in the last decade."),
                                        p("The COVID-19 pandemic exceeds the scope and imagination of so many, especially when distorted by misinformation in the media, by the President. This project aims to visualize the spread of COVID-19 in the US to the county level with data from the NYT."),
                                        p("For more information about the sources please see the next tab, “Data Sources”. "), br(), br(),
                                        )
                    )),
            #second Project tab, Data Sources
            tabPanel("Credits",
                     
                     h1(tags$b("Data Sources"), align = "center"),
                     
                     fluidRow(column(2), column(8, 
                                                p("State and county data coronavirus counts and deaths are from the meticulously kept NYT database. (John Hopkins Coronavirus Resource Center also has robust international counts)"),
                                                p(a(href="https://www.nytimes.com/article/coronavirus-county-data-us.html", "Find it here.")),
                                                br(),
                                                p("State testing counts are from the Covid Tracking Project under The Atlantic."),
                                                p(a(href="https://covidtracking.com/data", "Find it here.")),
                                                br(),
                                                p("Mapping was done with shapefiles from the US Census Bureau."),
                                                p(a(href="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html", "Find it here.")),
                            ))
            ),
            
            #third Project tab, Data Sources
            tabPanel("About Me",
                     titlePanel("Jerrica Li"), 
                     
                             #body text 
                             imageOutput("jerrica", width = "100%", height = "100%"),
                             h1(tags$b("I am a junior studying data science at Harvard University."), align = "center"),
                             fluidRow(column(2), column(8, 
                                                        p("I major in Comparative Literature, though I also have academic interests in data science and visualization, US immigration and immigrant rights especially within the Boston Chinese community, and using technology as tools for education."),
                                                        p("This Shiny app is the final project for the course Government 1005: Data and my first ever! The topic is personal, as it is with everyone in a world affected by the pandemic. On Tuesday, March 10th, 2020, Harvard College students were forced to move out of campus within 5 days. 
                                                          The campus was in mayhem, heightened with chaos, confusion, denial, and reckoning with the severity of COVID-19. So in some ways, this project is as much for the viewers of the app as it is for me personally to process the effects of the pandemic and explore where 
                                                          the US can continue to improve its handling of this epidemic."),
                                                        br(),
                                                        p("You can find me at:"),
                                                        p(a(href="mailto:jerrica_li@college.harvard.edu?Subject=Hello!", "My college email")),
                                                        p(a(href="https://github.com/je960", "My Github account")),
                                                        p(a(href="https://www.linkedin.com/in/jerrica-li/", "My LinkedIn account"))
                             ))
                    )
            )
            ),
    
    #State Data panel
    tabPanel("State Data",
             fluidPage(
                 titlePanel("Modeling the Pandemic"),
                 sidebarLayout(
                     sidebarPanel(
                         
                         HTML('<script> document.title = "COVID-19 In America"; </script>'),
                         tags$head(tags$link(rel="shortcut icon", href="https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.poynter.org%2Freporting-editing%2F2020%2Fyouve-probably-seen-this-image-of-the-coronavirus-everywhere-what-is-it-exactly%2F&psig=AOvVaw3S076kS4qN7crlMrFUgzGY&ust=1588053753988000&source=images&cd=vfe&ved=0CAIQjRxqFwoTCLDe7rD3h-kCFQAAAAAdAAAAABAD")),
                         
                         p(tags$em("Be aware of the fitted scale.")),
                         
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
    
   
    tabPanel("State Maps", 
             titlePanel("Mapping the Pandemic"),
             sidebarLayout(
                sidebarPanel(
                    p(tags$em("Let's explore the spread of the pandemic in America, looking at the most recent numbers. Keep in mind that testing is not widely available
                                in the US, so many states may be underreporting their numbers. Hover over each state for a quick look, or click on each state.")),
                    p(tags$bold("Choose a variable:")),
                ),
                mainPanel(
                    leafletOutput("state_map")
                )
             )
    )
)






# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #updateSelectizeInput(inputId = "selectize", label = "Select states to show on graph", choices = data, server = TRUE)
    
    #State data tab, county
    #change county input to dynamically change with chosen state
    #using Ui/uiOutput function to dynamically generate a selectInput
    output$countySelection <- renderUI({
        selectInput(inputId = 'county', 
                    label = 'Choose a county:', 
                    choice = unique(county_data$county[county_data$state == input$state]))
    })
    
    
    #State data tab, state cases over time plot
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
    
    #State data tab, state deaths over time plot
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
    
    #State Data tab, county cases over time plot
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
    
    #State tab, county deaths over time plot
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
    
    #State Maps Panel, cases 
    output$state_cases <- renderLeaflet({
        
        pal <- colorNumeric("YlOrRd", NULL, n = 10)
        
        labels <- sprintf(
            "%s<br/>%g cases",
            leafmap$NAME, leafmap$cases
        ) %>% lapply(htmltools::HTML)
        
        leaflet(data = leafmap) %>% 
            addTiles() %>%
            setView(-96, 37.8, 4) %>%
            addPolygons(fillColor = ~pal(cases),
                        fillOpacity = 0.8,
                        color = "#BDBDC3",
                        weight = 1,
                        popup = popup_dat,
                        label = labels,
                        labelOptions = labelOptions(
                            textsize = "15px",
                            direction = "auto")) %>%
            addLegend(position = "bottomright", pal = pal, values = ~cases,
                      title = "Number of cases",
                      opacity = 1 ) 

    })
    
    #Logarithmic Panel, county cases over time plot
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

    
    # Project panel, Overview tab, COVID-19 NYT Illustration
    output$image <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './cybor.jpg', 
             height = 400,
             width = 550, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    
    # Project panel, Overview tab, newspaper headline image
    output$headline <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './headline.png', 
             height = 700,
             width = 600, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    
    # Project panel, About Me tab, bio photo
    output$jerrica <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './jerrica.jpg', 
             height = 400,
             width = 300, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
}



# Run the application 
shinyApp(ui = ui, server = server)



