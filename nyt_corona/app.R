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
library(DT)
library(gt)

theme_set(theme_bw())
ok <- tibble("Variable" = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."), 
             "Value" = c(.5782, 3.4116, 5.6641, 6.7036, 8.8152, 47.3151))

gt_tbl <- ok %>% gt() %>% 
    tab_header(
        title = "Trouble Index Statistics")

county_data_shiny <- readRDS("county_data")
state_data_shiny <- readRDS("state_data")
county_map_data <- readRDS("county_map_data.rds")
leafletmap_shiny <- readRDS("leafmap.rds")
leafletmap_deaths <- readRDS("leafmap_deaths")
testing_shiny <- readRDS("test.rds")
trouble_map <- readRDS("trouble_map.rds")


ui <- navbarPage(
    tags$b("Coronavirus Up Close"),
    
    theme = shinytheme("journal"),
    
    #Project Panel
    tabPanel("Overview",
             
             tabsetPanel(
                 
             #first Overview tab, Project
             tabPanel("The Project",
             
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
                                        br(),
                                        br()
                                        )
                    )),

            #third Overview tab, About me
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
            ),
            
            #second Overview tab, Data Sources
            tabPanel("Credits",
                     
                     h1(tags$b("Data Sources"), align = "center"),
                     
                     fluidRow(column(2), column(8, 
                                                p("State and county data coronavirus counts and deaths are from the meticulously kept NYT database (John Hopkins Coronavirus Resource Center also has robust international counts). Personally, the NYT have been my main source of news throughout this crisis as I trust their accuracy and dedication to quality journalism.
                                                  I grew up in a non-political household, but I began listening to their daily podcasts as a sophomore in college and it became my main source of education to foray into American politics. They are doing amazing work on the frontlines and keeping the American public informed.", a(href="https://www.nytimes.com/article/coronavirus-county-data-us.html", "Find the NYT data here.")),
                                                br(),
                                                p("State testing counts are from the Covid Tracking Project under The Atlantic. The team at Atlantic were concerned with minimal reporting and inadequate testing, the result of the US and national leadership being so slow to implement mass testing. It's a phenomenal effort of hundreds of volunteer data-gatherers, developers, scientists, reporters etc.", 
                                                  a(href="https://covidtracking.com/data", "Find COVID Tracking Project here"), "and there is a", a(href="https://covidtracking.com/race", "COVID Racial Data Tracker"), "as well."),
                                                br(),
                                                p("Mapping was done with shapefiles from the US Census Bureau. It was a long arduous journey to learn Leaflet, but the availability of these kinds of cartography boundaries by the US Census Bureau makes mapping much more accesible than I first imagined.", a(href="https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html", "Find the US Census Bureau map files here.")),
                                                br(), 
                                                p("Thank you to my friends and all the educators, staff, and fellow Odysseus-figures in Gov 1005 who have touched this work: the herioc Preceptor, the caffeinated June Hwang, the Leaflet wise Evelyn Cai, Joshua Pan, Katelyn Li, Jamie Bikales, James Bedford, Katherine Miclau, and Sara Li. It takes a village for a complit girl to learn R.")
                                                ))
            ))
    ),
    
    #State Data panel
    tabPanel("State Data",
             fluidPage(
                 titlePanel("Modeling the Pandemic"),
                 sidebarLayout(
                     sidebarPanel(
                         
                         p(tags$em("Be aware of the fitted scale.", 
                                   "it's interactive")),
                         
                         #ask user to input state
                         
                         selectInput(inputId = 'state', 
                                     label = 'Choose a state:', 
                                     choice = levels(state_data_shiny$state)
                         ),
                         br(),
                         
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
    
    tabPanel("State Maps", 
             titlePanel("Mapping the Pandemic"),
             sidebarLayout(
                 sidebarPanel(
                     p(tags$em("Let's explore the spread of the pandemic in America, looking at the most recent numbers. Keep in mind that testing is not widely available
                                in the US, so many states may be underreporting their numbers of confirmed cases. Hover over each state for a quick look and zoom in and out to view states like Hawaii, Alaska, and Puerto Rico.")),
                 ),
                 mainPanel(
                     h2(tags$b("US Map: Cases by State"), align = "center"),
                     leafletOutput("state_map"),
                     h2(tags$b("US Map: Deaths by State"), align = "center"),
                     leafletOutput("death_map"),
                     br(),br(), br(), br()
                 )
             )
    ), 
    
    tabPanel("Logarithmic Functions",
             titlePanel("Modeling with Logarithmic Functions"),
             sidebarLayout(
                 sidebarPanel(
                     p("Another way of looking at COVID-19 data, first made popular by", a(href="https://www.youtube.com/watch?v=54XLXg4fYsc", "Minute Physics,"), "is to look at logarithmic scales.",
                       "This graph is helpful because it help us visualize whether a state has gotten past the curve, ie when we see the line make a big drop downwards."),
                     p("This is because of the transformed axes: the x-axis is the log of total confirmed cases and the y-axis is the log of all new cases each week. 
                       Notice how time is not an axis; this graph is purely looking at the rate of increase of new cases. This is helpful because instead of a linear axis, 
                       the log gives us an exponential scale which is the rate an epidemic moves! In this log graph, exponential growth is shown by a linear line, so when a state's line 
                       makes a big drop, it means they're past the curve and stopped the exponential growth of the virus."),
                     p("One final note is that this graph may diminish the human side of the epidemic because the each tic mark exponentially grows to encompass more and more lives at risk
                       in ways it's hard to imagine. Treat this graph as a big picture image that shows the virus to scale."),
                     p(tags$em("Please be patient for about 30 seconds as the animation loads. It's worth the wait!")),
                     
                     #ask user to input state
                     
                     selectInput(inputId = "logstate", label = "Select states to show on graph", choices = levels(state_data_shiny$state), multiple = TRUE),
                     br(),br()
                 ),
                 mainPanel(
                     h1(tags$b("Coronavirus to Scale"), align = "center"), 
                     imageOutput("logplot")
                 )
             )
    ),
    
    tabPanel("Underreporting Experiment",
             titlePanel(
                 h1("Underreporting COVID-19 Cases in the US",
                    align = "center")
             ),
             
             fluidRow(column(2), column(8, 
             p("I had plans to run regressions on the correlation of COVID-19 cases by county to variables like income, racial demographics, rural/ urban areas, etc. 
               But then I began thinking about the", a(href ="https://www.nytimes.com/2020/03/13/opinion/china-response-china.html", "aggregious delay"), "in the US's reaction to taking COVID-19 seriously and", 
               a(href ="https://www.nytimes.com/2020/03/19/opinion/coronavirus-testing.html", "to testing."), "Was there a way to account for and understand how bad undertesting and underreporting is in the US?"),
             p("With so much public COVID-19 data publically available, let's try to run some models on our own. Why not give it a try?"),
            
             hr(), 
             
             h2("What are our base assumptions?"),
             p("Our data analysis rests on a few assumptions about people's behaviors when they go to get tested. Because the US is not at the stage yet where 
                states can do random population sample testing, one of our assumptions is that everyone who goes to get a test thinks equally that they have coronavirus
                and that each state has these similar testing behaviors"),
             
             hr(), 
             
             h2("What are our testing variables?"),
             p("The data available is tests positive and negative by state. It would be spectular to have testing numbers by county, but the US does not have a uniform healthcare system and those numbers are availble 
               sporadically by state."), 
             h3("People Per Test"),
             p("Regardless, let's first make a variable of the state's population divided by number of tests and call it people_per_test. This shows us how many people one test result represents. 
               So for example, New York state has a population of about 19 million people and has done around 1 million tests, so when we look at confirmed cases, one test represents 13 people in that state. 
               This is a variable of resolution of the state's testing numbers."),
             h3("Percent Positive"),
             p("Next, let's make a variable of the percent of positive tests called perc_positive. We are dividing the number of total tests by the number of positive tests. This shows us the magnitude of the spread of the virus"), 
             h3("Trouble Index"), 
             p("Lastly, let's create our trouble index by multiplying people per test by percent positive. So, our index takes two things into account, resolution of testing and the magnitude of the virus in each state. 
               Therefore, a high index means a low testing resolution and/or a high percentage of positive tests. Let's take a look at our data arranged from low to high by our trouble index. Are you surprised?"),
             br(),
             DT::dataTableOutput("variable_table"), 
             br(), 
             p("And just to understand the range of values of our index, here is a summary:"),
             gt_output("trouble_stats"),
             
             hr(), 
             
             h2("Let's map it!"), 
             leafletOutput("trouble")

             ))
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
            geom_area(aes(x=date), fill="royalblue2", alpha=.5)
        
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
            labs(title =paste("Confirmed Deaths by Corona for", input$county, sep = " "),  
                 x = "Date", 
                 y = "Confirmed Cases") +
            scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
            geom_area(aes(x=date), fill="orangered2", alpha=.5)
        
        #render
        c
    })
    
    #State Maps Panel, cases 
    output$state_map <- renderLeaflet({
        
        
            
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
            
    output$death_map <- renderLeaflet({

            pal <- colorNumeric("YlOrRd", NULL, n = 10)
            
            labels <- sprintf(
                "%s<br/>%g deaths",
                leafletmap_deaths$NAME, leafletmap_deaths$deaths
            ) %>% lapply(htmltools::HTML)
            
            leaflet(data = leafletmap_deaths) %>% 
                addTiles() %>%
                setView(-96, 37.8, 4) %>%
                addPolygons(fillColor = ~pal(cases),
                            fillOpacity = 0.8,
                            color = "#BDBDC3",
                            weight = 1,
                            label = labels,
                            labelOptions = labelOptions(
                                textsize = "15px",
                                direction = "auto")) %>%
                addLegend(position = "bottomright", pal = pal, values = ~deaths,
                          title = "Number of deaths",
                          opacity = 1 )
            
        })
         
    
    #Logarithmic Panel, county cases over time plot
    output$logplot <- renderImage({
        
        outfile <- tempfile(fileext='.gif')
        
        
        # Return blank if no choice selected yet
        if (input$logstate == " ") {
            return()
        }
        
        # Filter the dataset based on what state was selected
        logplot <- state_data_shiny %>%
            filter(state %in% input$logstate)%>%
            arrange(state) %>%  
            group_by(state) %>% 
            slice(which(row_number() %% 7 == 1))
        
        logplot$change <- NA
        for(i in 2:nrow(logplot)){
            logplot$change[i] <- logplot$cases[i] - logplot$cases[i - 1]}
        
        logplot$change[logplot$change < 0] <- 1
        logplot$change[is.na(logplot$change)] <- 1
        
        logplot <- logplot %>% 
            mutate(log_case = log(cases)) 

        #Create plot of corona cases over time
        b <- ggplot(logplot, aes(x = log_case, y = log(change), color = state)) +
                geom_line() +
                geom_point() +
                theme_minimal() +
                labs(x = "Confirmed cases", 
                     y = "New cases", 
                     color = "State") +
                transition_reveal(log_case)
        
        anim_save("outfile.gif", animate(b))
        
        #Return a list containing the filename
        list(src = "outfile.gif",
             contentType = 'image/gif',
              width = 600,
              height = 650,
              alt = "This is an animation graph of the trajectory of coronavirus.", 
            style="display: block; margin-left: auto; margin-right: auto;"
        )}, deleteFile = TRUE)

    
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
             height = 390,
             width = 275, 
             style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    
    output$variable_table = DT::renderDataTable({
        testing_shiny %>% 
            select(state, everything()) %>% 
            select(-GEOID, -negative_test) %>% 
            arrange(desc(trouble))
    })
    
    
    #Logarithmic Panel, county cases over time plot
    output$trouble_stats <- render_gt(
        expr = gt_tbl
    )
    
    
    output$trouble <- renderLeaflet({
        
        popup_dat <- paste0("<strong>State: </strong>",
                            trouble_map$state,
                            "<br><strong> Index: </strong>",
                            trouble_map$trouble)
        
        
        pal <- colorNumeric("YlOrRd", NULL, n = 10)
        
        leaflet(data = trouble_map) %>% 
            addTiles() %>%
            setView(-96, 37.8, 4) %>%
            addPolygons(fillColor = ~pal(trouble),
                        fillOpacity = 0.8,
                        color = "#BDBDC3",
                        weight = 1,
                        popup = popup_dat) %>%
            addLegend("bottomright", pal = pal, values = ~trouble,
                      title = "Trouble Index",
                      opacity = 1 )
    })
}



# Run the application 
shinyApp(ui = ui, server = server)



