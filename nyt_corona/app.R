#shiny app: https://je960.shinyapps.io/nyt_corona/

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

#for gganimate
theme_set(theme_bw())

#gt table the Underestimate panel
ok <- tibble("Variable" = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."), 
             "Value" = c(.5782, 3.4116, 5.6641, 6.7036, 8.8152, 47.3151))

gt_tbl <- ok %>% gt() %>% 
    tab_header(
        title = "Trouble Index Statistics")

#read in files
county_data_shiny <- readRDS("county_data.rds")
state_data_shiny <- readRDS("state_data.rds")
leafmap_1 <- readRDS("leafmap_1.rds")
testing_shiny <- readRDS("test.rds")
trouble_pr <- readRDS("trouble_map_PR.rds")


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
                                        br(),br()
                                    )
                )),

            #third Overview tab, About me
            tabPanel("About Me",
                     
                     # This is amazing, set the favicon image and wording
                     HTML('<script> document.title = "Coronavirus Up Close"; </script>'),
                     tags$head(tags$link(rel="shortcut icon", href="https://lh5.googleusercontent.com/ZyNIMBOw4Y6u-zgwfO6rjzXPDJzRSOJRQ0Iz829tncPk0v_fB76fuqJwpnPYVFXJON7gDZDYUlX93W1HmwNvdbSX1yGmTIikfrmIYN84bsTTrpKGNe_QBTsTMH0cTAR2zPBAgZFT")),
                     
                     
                     titlePanel("Jerrica Li"), 
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
                         
                         p("Note each graph's fitted scale and scroll down to see state data. All of these graphs are interactive, so hover and click to see exact numbers for each date."),
                         
                         #ask user to input state
                         selectInput(inputId = 'state', 
                                     label = 'Choose a state:', 
                                     choice = levels(state_data_shiny$state)
                         ),
                         br(),
                         
                         #ask user to input county (which shows up first)
                         uiOutput("countySelection")
                     ),
                     
                     #output state plot and death plot
                     mainPanel(
                         #state
                         h1(tags$b("Corona Statistics by County"), align = "center"),
                         plotlyOutput("countyplot"), br(), br(),
                         plotlyOutput("countydeath"), br(), br(), br(), br(), br(), br(),
                         
                         #county
                         h1(tags$b("Corona Virus Statistics by State"), align = "center"),
                         plotlyOutput("stateplot"), br(), br(),
                         plotlyOutput("deathplot"))
                 )
             )
    ),
    
    #US state map panel
    tabPanel("State Maps", 
             titlePanel("Mapping the Pandemic"),
             sidebarLayout(
                 sidebarPanel(
                     p("Let's explore the spread of the pandemic in America, looking at the most recent numbers. Keep in mind that testing is not widely available
                                in the US, so many states may be underreporting their numbers of confirmed cases. Hover or click over each state for a quick look and zoom in and out to view states like Hawaii, Alaska, and Puerto Rico."),
                     
                     #input variable for map
                     radioButtons(inputId="death", label="Choose your variable:",
                                  choices=list("deaths", "cases")),
                     ),
                 
                 mainPanel(
                     h2(tags$b("US Map: Corona by State "), align = "center"),
                     leafletOutput("state_map"),
                     br(),br(), br(), br()
                 )
             )
    ), 
    
    #Logarithm Axes Panel
    tabPanel("Logarithmic Functions",
             titlePanel("Modeling with Logarithmic Functions"),
             sidebarLayout(
                 sidebarPanel(
                     p("Another way of looking at COVID-19 data, first made popular by", a(href="https://www.youtube.com/watch?v=54XLXg4fYsc", "Minute Physics,"), "is to look at logarithmic scales.",
                       "This graph is helpful because it help us visualize whether a state has gotten past the curve, ie when we see the line make a big drop downwards."),
                     h3("Axis Transformation"),
                     p("This is because of the transformed axes: the x-axis is the log of total confirmed cases and the y-axis is the log of all new cases each week. 
                       Notice how time is not an axis; this graph is purely looking at the rate of increase of new cases. This is helpful because instead of a linear axis, 
                       the log gives us an exponential scale which is the rate an epidemic moves! In this log graph, exponential growth is shown by a linear line, so when a state's line 
                       makes a big drop, it means they're past the curve and stopped the exponential growth of the virus."),
                     h3("Note"),
                     p("One final note is that this graph may diminish the human side of the epidemic because the each tic mark exponentially grows to encompass more and more lives at risk
                       in ways it's hard to imagine. Treat this graph as a big picture image that shows the virus to scale."),
                     p(tags$em("Please be patient for about 30 seconds as the animation loads. The screen may start grey and after a few moments will be in full color. It's worth the wait!")),
                     
                     #ask user to input state
                     selectInput(inputId = "logstate", label = "Select states to show on graph", choices = levels(state_data_shiny$state), multiple = TRUE, selected = "New York"),
                     br(),br()
                 ),
                 
                 mainPanel(
                     h1(tags$b("Coronavirus to Scale"), align = "center"), 
                     imageOutput("logplot")
                 )
             )
    ),
    
    #Experiment!
    tabPanel("Underreporting Experiment",
             titlePanel(
                 h1("Underreporting COVID-19 Cases in the US",
                    align = "center")
             ),
             
             fluidRow(column(2), column(8, 
             p("I had plans to run regressions on the correlation of COVID-19 cases by county to variables like income, racial demographics, rural/ urban areas, etc. 
               But then I began thinking about the", a(href ="https://www.nytimes.com/2020/03/13/opinion/china-response-china.html", "aggregious delay"), "in the US's reaction to taking COVID-19 seriously and", 
               a(href ="https://www.nytimes.com/2020/03/19/opinion/coronavirus-testing.html", "to testing."), "I could run those regressions, but confirmed cases is only half the picture when we don't know how many cases
               are going unconfirmed. A classmate showed me a data set by the COVID Tracking Project which shows the number of tests per state. With this data, is there a way to account for and understand how bad undertesting and underreporting is in the US?"),
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
               Therefore, a high index means a low testing resolution and/or a high percentage of positive tests, which suggests that actual COVID-19 cases are much higher than confirmed. Let's take a look at our data arranged from low to high by our trouble index."),
             br(),
             DT::dataTableOutput("variable_table"), 
             br(), 
             p("And just to understand the range of values of our index, here is a summary:"),
             gt_output("trouble_stats"),
             
             hr(), 
             
             h2("Let's map it!"), 
             p("Mapping out the data helps us understand visually where the worst states are. Remember, a high index means that these states are not doing enough testing and the scale of the problem is probably much worse than reported. 
               For the sake of these graphs, I ommitted Puerto Rico as an outlier so that you can see the definition within the continental US. Feel free to zoom out and move the map. Click each state to see the numbers."),
             radioButtons(inputId="variable", label="Choose your variable:",
                          choices=list("People Per Test" = 1, "Number of Tests" = 2, "Trouble Index" = 3)), 
             
             leafletOutput("trouble"),
             
             hr(),
        
             h2("Let's take a closer look at our assumptions."),
             p("Great, we've created a proxy variable to look at US states and their effectiveness against COVID-19 in a new light. But we had a little bit of trouble with the obvious outlier - Puerto Rico. Puerto Rico has very low testing which means it had sky high people per test ratios.
               This in turn affected it's index number to be far too high. Is that fair? Should there be some weight, after a certain limit, so that extremes of both variables are less significant?"),
             p("Now lets think back to our assumptions - that all of these states are exhibiting the same behaviors towards testing. Though that probably is a good assumption, for now, let's consider why that could be biased. 
               For instance, maybe when a state is first starting to test (so when people per test is very high), they are only testing people they are very sure are sick. So in the beginning, the percent of positive tests might be reaching 100% or 50% or some other very high number."),
             p("With this assumption, there would be a bias in our data where a smaller number of tests corresponds to a higher percent positive, just by the nature of how people are testing. Is this true? Maybe we do need to weight our index!"),
             p("It makes sense to look at the trajectory of the top few states with the highest number of cases and their curve over time: New York, Massachusetts, and New Jersey.
               If we plot the trajectory of these three states on a graph with people per test on the x-axis and percent positive on the y-axis, and if this assumption is true, then we might expect to see a line which we can use as a weight to balance our index. Let's test it out and graph the three top states on our axes."),
             imageOutput("try_1", width = "100%", height = "100%"), 
             br(),
             p("This is a crazy graph! Just for fun, let's add a linear regression line for each state where the shadow is a 95% confidence interval."),
            imageOutput("regression", width = "100%", height = "100%"),
            br(),
            p("What's interesting about this graph is that all the numbers are bunched up closer to the origin because this is a different graph than we're used to reading (from left to right). Instead, time moves right to left because the more tests, the lower number of people per tests. This graph doesn't tell us much because we can't see what's going on where all the points are 
              bunched and it looks like the regression slope for some states are positive, others negative. To fix this, what if we tried flipping our x-axis? What if we plotted the x-axis as population divided by total tests instead of the other way around? That variable would be tests per people. That's a little funny to think about, but at least now it's more logicial. As testing (and time)
              increases, tests per person increases. And hopefully we'll see a curve easier to read!"),
            imageOutput("testgif", width = "100%", height = "100%"),
            br(),
            p("That is a beautiful plot! We can see that actually, after the initial mess when the testing is just getting started, the behavior of the three states is quite similar and parabolic. As testing increases, there is an increase in percent positive, but this gradually decreases. What do you think of these graphs? Is it enough to claim that testing behavior is biased towards a lower number of tests?"),
            br(), br()
             ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    #State data tab, change county input to dynamically change with chosen state
    #using UI function to dynamically generate a selectInput
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
    
    #State Maps Panel, map
    output$state_map <- renderLeaflet({
            
        if(input$death == "deaths"){
            pal <- colorNumeric("YlOrRd", NULL, n = 10)
            
            popup_dat <- paste0("<strong>State: </strong>", 
                                leafmap_1$NAME, 
                                  "<br><strong>Deaths: </strong>", 
                                leafmap_1$deaths)
            
            labels <- sprintf(
                "%s<br/>%g deaths",
                leafmap_1$NAME, leafmap_1$deaths
            ) %>% lapply(htmltools::HTML)
            
            leaflet(data = leafmap_1) %>% 
                addTiles() %>%
                setView(-96, 37.8, 4) %>%
                addPolygons(fillColor = ~pal(deaths),
                            fillOpacity = 0.8,
                            color = "#BDBDC3",
                            weight = 1,
                            popup = popup_dat,
                            label = labels,
                            labelOptions = labelOptions(
                                textsize = "15px",
                                direction = "auto")) %>%
                addLegend(position = "bottomright", pal = pal, values = ~deaths,
                          title = "Number of Deaths",
                          opacity = 1 )
        }
        
        else if (input$death == "cases"){
            
        pal <- colorNumeric("YlOrRd", NULL, n = 10)
        
        popup_dat <- paste0("<strong>State: </strong>", 
                            leafmap_1$NAME, 
                            "<br><strong>Cases: </strong>", 
                            leafmap_1$cases)
        
        labels <- sprintf(
            "%s<br/>%g cases",
            leafmap_1$NAME, leafmap_1$cases
        ) %>% lapply(htmltools::HTML)
        
        leaflet(data = leafmap_1) %>% 
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
        }
        
        
        })
 
    #Logarithmic Panel, animation
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
        list(src = './pictures/illustration.jpg', 
             height = 400,
             width = 550, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    
    # Project panel, Overview tab, newspaper headline image
    output$headline <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './pictures/headline.png', 
             height = 700,
             width = 600, style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    
    # Project panel, About Me tab, bio photo
    output$jerrica <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './pictures/jerrica.jpg', 
             height = 390,
             width = 275, 
             style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    #Underreport panel, all variables gt table 
    output$variable_table = DT::renderDataTable({
        testing_shiny %>% 
            select(state, everything()) %>% 
            select(-GEOID, -negative_test) %>% 
            arrange(desc(trouble))
    })
    
    
    #Underreport panel, trouble gt
    output$trouble_stats <- render_gt(
        expr = gt_tbl
    )
    
    #Underreport panel, US map 
    output$trouble <- renderLeaflet({
        
        if(input$variable == "1") {
            
            pal <- colorNumeric("YlOrRd", n = 10, domain = trouble_pr$people_per_test)
            
            state_popup <- paste0("<strong>State: </strong>", 
                                  trouble_pr$NAME, 
                                  "<br><strong>People Per Test: </strong>", 
                                  trouble_pr$people_per_test)
            
            leaflet(data = trouble_pr) %>% 
                addTiles() %>%
                setView(-96, 37.8, 4) %>%
                addPolygons(fillColor = ~pal(people_per_test),
                            fillOpacity = 0.8,
                            color = "#BDBDC3",
                            weight = 1,
                            popup = state_popup) %>%
                addLegend("bottomright", pal = pal, values = ~people_per_test,
                          title = "People Per Test",
                          opacity = 1 )
        }
        
       
        else if(input$variable == "2"){
            
            state_popup <- paste0("<strong>State: </strong>", 
                                  trouble_pr$NAME, 
                                  "<br><strong>Numbers Tested: </strong>", 
                                  trouble_pr$total_test)
            
            pal <- colorNumeric("YlOrRd", n = 10, domain = trouble_pr$total_test)
            
            leaflet(data = trouble_pr) %>% 
                addTiles() %>%
                setView(-96, 37.8, 4) %>%
                addPolygons(fillColor = ~pal(total_test),
                            fillOpacity = 0.8,
                            color = "#BDBDC3",
                            weight = 1,
                            popup = state_popup) %>%
                addLegend("bottomright", pal = pal, values = ~total_test,
                          title = "Total Tests",
                          opacity = 1 )
        }
        
        else {
            pal <- colorNumeric("YlOrRd", n = 10, domain = trouble_pr$trouble)
            
            popup_dat <- paste0("<strong>State: </strong>",
                                trouble_pr$NAME,
                                "<br><strong> Index: </strong>",
                                trouble_pr$trouble)
            
            leaflet(data = trouble_pr) %>% 
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
        }
        
    })
    
    #Underestimate panel, gif 
    output$testgif <- renderImage({
        
        list(src = "./pictures/testing.gif",
         contentType = 'image/gif',
         width = 575,
         height = 610, 
         style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE)
    
    # Underestimating panel, graph of top three states
    output$try_1 <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './pictures/top_three.png', 
             height = 600,
             width = 850, 
             style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    # Underestimating panel, regression of top three states
    output$regression <- renderImage({
        # Return a list containing the filename and alt text
        list(src = './pictures/regression.png', 
             height = 660,
             width = 655, 
             style="display: block; margin-left: auto; margin-right: auto;")
    }, deleteFile = FALSE
    )
    
    
}



# Run the application 
shinyApp(ui = ui, server = server)



