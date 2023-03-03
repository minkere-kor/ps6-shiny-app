library(shiny)
library(tidyverse)
getwd()
troposphere <- read_delim("data/UAH-lower-troposphere-long.csv.bz2")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("General Information",
             titlePanel("General Information"),
             
             sidebarLayout(
               sidebarPanel(
                 p("The data set is taken from the UAH's measurement of temperature changes in the troposphere"),
                 p("It includes", strong(nrow(troposphere)), "observations as well as", strong(ncol(troposphere)), "variables"),
                 p("The", em("temperature"), "is measured for each", em("region"), "and is then further recorded for each", em("month"), "and", em("year.")),
                 p("The", em("change in temperature"), "is recorded in", strong("Celcius"), "and the timespan ranges between", strong(min(troposphere$year)), "and", strong(max(troposphere$year))),
                 p("To the right are ten samples taken from the Troposphere Data Set.")
               ),
               
               mainPanel(
                 dataTableOutput("tropTable")
                 )
               )
             ),
             
    tabPanel("Plot Information",
             titlePanel("Plot Information"),
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year", "Select Year Range", min = 1978, max = 2023, value = 2023),
                 checkboxGroupInput("regions",
                                    "Select a Region",
                                    choices = unique(troposphere$region))
               ),
               
               mainPanel(
                 plotOutput("tropPlot"),
                 
               )
             )
             ),
    
    tabPanel("Table Information",
             titlePanel("Table Information"),
             
             
             )
  ),
)

server <- function(input, output) {
    output$tropTable <- renderDataTable({
      troposphere %>% 
        sample_n(10)
    })
    
    output$tropPlot <- renderPlot({
      troposphere %>% 
        mutate(timeframe = year + month/12) %>% 
        filter(region %in% input$regions) %>% 
        filter(timeframe <= input$year) %>% 
        ggplot(aes(timeframe, temp, col=factor(region))) +
        geom_point() +
        geom_smooth(method="lm")
    })
}

shinyApp(ui = ui, server = server)
