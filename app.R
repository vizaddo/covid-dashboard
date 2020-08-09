#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(plotly)
library(sf)
library(leaflet)
library(htmltools)
library(tidyverse)
library(jsonlite)
library(data.table)
library(ggthemes)
library(RColorBrewer)
library(shinycssloaders)

# Load data from open-source
dt.case <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv")
dt.case$date <- as.Date(dt.case$date)
loc <- unique(dt.case$location)
dt.case <- dt.case[continent=="", continent:='World']

# Load coordinate data
world.coordinate <- st_read(
    "Longitude_Graticules_and_World_Countries_Boundaries/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
df.recent <- dt.case %>%
    filter(date == max(date))
df.recent.log <- df.recent %>%
    mutate(new_cases_plus = new_cases+1)

# join with data
df.recent.with.coord <- world.coordinate %>%
    left_join(df.recent.log, by = c('CNTRY_NAME'='location'))
pal.tot <- colorNumeric('PuBu', domain = log(df.recent.with.coord$total_cases))
pal.new <- colorNumeric('PuBu', domain = log(df.recent.with.coord$new_cases_plus))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 World Dashboard"),
    "Data from ourworldindata.org | 2020",
    
    # Create the structure for the web-app
    tabsetPanel(
            tabPanel(title = "World Case Today",
                sidebarLayout(
                    sidebarPanel(h2(paste0("COVID-19 cases as of today (", max(dt.case$date), ")")), 
                                 radioButtons("button_map_color", label = 'Color based on', 
                                              choices = c('Total Cases', "New Cases"), selected = 'Total Cases'),
                                 position = 'right'),
                    mainPanel(withSpinner(leafletOutput('map_world_case')))
                )
                
            ),
            #### show the number of tests across country ####
            tabPanel(title = "Cases", 
                # Sidebar layout for case
                sidebarLayout(
                    sidebarPanel(
                        selectInput("select_country", "Select Country", choices = loc, 
                                    selected = c('Indonesia', 'Malaysia'), multiple = T),
                        selectInput("select_y_axis", "Select Measure", 
                                    choices = c('total_cases', 'total_deaths', 'new_cases', 'new_deaths'),
                                    selected = 'new_cases'),
                        dateRangeInput("date", "Select Date", start = "2020-01-01", min = "2020-01-01", max = "2021-01-01")    
                    ),
                    # Show a plot of the generated distribution
                    mainPanel(plotlyOutput("plot_case"),
                              dataTableOutput("dt_case")
                    )
                )
            ),
            
            #### show the number of tests across continents ####
            tabPanel(title = "Tests", 
                # sidebar layout for tests
                 sidebarLayout(
                    sidebarPanel(
                        selectInput("select_country_test", "Select Country", choices = loc, 
                                    selected = c('Indonesia', 'Malaysia'), multiple = T),
                        selectInput("select_test_measure", "Select Measure",
                                    choices = c('total_tested','average_tested_daily', 'percent_population_tested'),
                                    selected = 'total_tested')    
                    ),
                    mainPanel(plotlyOutput("plot_test"),
                              dataTableOutput("dt_tests_country"))
                )
                
            ),
            
            #### by continent ####
            tabPanel(title = "Case by Continent",
                     sidebarLayout(
                         sidebarPanel(
                             selectInput(
                                 inputId = "select_continent",
                                 label = "Select Continent",
                                 choices = unique(dt.case$continent),
                                 selected = c("Asia", 'Europe', 'South America'), multiple = T
                             ),
                             selectInput("select_y_axis_continent", "Select Measure", 
                                         choices = c('totalcases', 'totaldeaths', 'newcases', 'newdeaths'),
                                         selected = 'newcases'),
                         ),
                         mainPanel(
                             plotlyOutput("plot_case_continent"),
                             dataTableOutput("dt_case_continent")
                         )
                     )
            )
    )
)


##### Define server logic required to draw a histogram ####
server <- function(input, output) {

    output$map_world_case <- renderLeaflet({
        
        map <- leaflet(data = df.recent.with.coord, 
                       options = leafletOptions(minZoom = 1.25, maxZoom = 4)) %>%
            addTiles() %>%
            setView(lng = 28.0339 , lat = 1.6596, zoom = 1.25)
        if(input$button_map_color == 'Total'){
            map %>%
                addPolygons(weight = 2, fillOpacity = 0.7, 
                            fillColor = ~pal.tot(log(total_cases)), 
                            color ='white', 
                            label = ~paste(CNTRY_NAME, '- click for detail'),
                            popup =  ~paste("<b>", CNTRY_NAME, "</b> <br> Total Case:", total_cases, "<br>",
                                            "New Case:", new_cases, "<br>",
                                            "Total Deaths:", total_deaths),
                            highlight = highlightOptions(color = 'darkblue', weight = 5, bringToFront = T),
                            group = "Total Case")    
        } else {
            map %>%
            addPolygons(weight = 2, fillOpacity = 0.7, 
                        fillColor = ~pal.new(log(new_cases_plus)),
                        color = 'white',
                        label = ~paste(CNTRY_NAME, '- click for detail'),
                        popup =  ~paste("<b>", CNTRY_NAME, "</b> <br> Total Case:", total_cases, "<br>",
                                        "New Case:", new_cases, "<br>",
                                        "Total Deaths:", total_deaths),
                        highlight = highlightOptions(color = 'darkblue', weight = 5, bringToFront = T),
                        group = "New Case")
        }
            
    })
    
    output$plot_case <- renderPlotly({
        dt.case %>% 
            filter(location %in% input$select_country) %>%
            filter(date %between% c(input$date)) %>%
            ggplot(aes(x=date,  col = location)) + geom_line(aes_string(y = input$select_y_axis)) + 
            labs(title = paste("COVID-19", input$select_y_axis),
                 subtitle = "Data Updated Daily from ourworldindata (c)",
                 x = "Date", y = paste(input$select_y_axis)) + theme_classic()
    })
    
    output$plot_case_continent <- renderPlotly({
        dt.case %>%
            filter(date %between% c(input$date)) %>%
            filter(continent %in% input$select_continent) %>%
            group_by(continent, date) %>%
            summarize(newcases = sum(new_cases, na.rm=T), totalcases = sum(total_cases, na.rm=T),
                      totaldeaths = sum(total_deaths, na.rm=T), newdeaths = sum(new_deaths, na.rm=T), 
                      totaltests = sum(total_tests, na.rm=T)) %>%
            ungroup() %>%
            ggplot(aes(x=date,  col = continent)) + geom_line(aes_string(y = input$select_y_axis_continent)) + 
            labs(title = paste("COVID-19", input$select_y_axis_continent, "accross continents."),
                 x = "Date", y = paste(input$select_y_axis_continent)) + theme_classic()
    })
    
    output$dt_case <- renderDataTable({
        dt.case %>%
            group_by(location) %>%
            filter(date == last(date)) %>%
            ungroup() %>%
            select(Country = location, Continent = continent, `New Cases` = new_cases, `Total Cases` = total_cases,
                   `Total Deaths`=total_deaths, `New Deaths`=new_deaths, `Total Cases per Million`=total_cases_per_million, 
                   `Total Tests`=total_tests, `Total Tests per 1000`=total_tests_per_thousand)
    })
    
    output$dt_case_continent <- renderDataTable({
        dt.case %>%
            filter(continent %in% input$select_continent) %>%
            group_by(continent, date) %>%
            summarize(newcases = sum(new_cases, na.rm=T), totalcases = sum(total_cases, na.rm=T),
                      totaldeaths = sum(total_deaths, na.rm=T), newdeaths = sum(new_deaths, na.rm=T), 
                      totaltests = sum(total_tests, na.rm=T)) %>%
            ungroup() %>%
            filter(date == last(date))
    })
    
    dt.country.summary <- reactive({
        dt.case %>% 
            filter(location %in% input$select_country_test) %>%
            group_by(location) %>% 
            summarise(population = max(population), 
                      total_tested = max(total_tests, na.rm = T), 
                      average_tested_daily = mean(total_tests, na.rm=T),
                      percent_population_tested = round(total_tested/population,4)*100) %>%
            ungroup() %>% as.data.table()
    })
    output$plot_test <- renderPlotly({
        
        ggplot(dt.country.summary(), aes(x = location, fill = population)) + 
            geom_col(aes_string(y = input$select_test_measure)) + 
            labs(title = paste("COVID-19 most recent", input$select_test_measure), x = "Country", y = input$select_test_measure) + 
            coord_flip() + theme_classic()
    })
    
    output$dt_tests_country <- renderDataTable({
        
        dt.country.summary()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
