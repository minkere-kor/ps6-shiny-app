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
                 p("The data set is taken from the UAH's measurement of temperature in the troposphere"),
                 p("It includes", strong(nrow(troposphere)), "observations as well as", strong(ncol(troposphere)), "variables"),
                 p("The", em("temperature"), "is measured for each", em("region"), "and is then further recorded for each", em("month"), "and", em("year.")),
                 p("The", em("temperature"), "is recorded in", strong("Celcius"), "and the timespan ranges between", strong(min(troposphere$year)), "and", strong(max(troposphere$year))),
                 p("To the right are ten samples taken from the Troposphere Data Set.")
               ),
               
               mainPanel(
                 dataTableOutput("tropOverview")
                 )
               )
             ),
             
    tabPanel("Plot Information",
             titlePanel("Plot Information"),
             
             sidebarLayout(
               sidebarPanel(
                 p("This Page provides a plot where you can analyze global trends in temperature.
                 The color palette of the graph can be changed as well which years and regions where you want to see trends for.
                 The mean of the temperature for the regions and years provided is shown at the bottom of the plot"),
                 sliderInput("year", "Select Year Range", min = 1978, max = 2023, value = 2023),
                 
                 radioButtons("color", "Choose Color Palette",
                              choices = c("Set1", "Set2", "Set3")),
                 
                 checkboxGroupInput("regions",
                                    "Select a Region",
                                    choices = unique(troposphere$region)
                 )
               ),
               
               mainPanel(
                 plotOutput("tropPlot"),
                 textOutput("plotInfo")
               )
             )
             ),
    
    tabPanel("Table Information",
             titlePanel("Table Information"),
             
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("filterby",
                                    "Filter by:",
                                    choices = names(troposphere),
                                    selected = names(troposphere)),
               ),
               
               mainPanel(
                 p("awagga"),
                 dataTableOutput("tropTable")
               )
             )
             )
  ),
)

server <- function(input, output) {
  # General Information
    output$tropOverview <- renderDataTable({
      troposphere %>% 
        sample_n(10)
    })
    
  # Plot Information
    tropospherePlot <- reactive({
      troposphere %>% 
        mutate(timeframe = year + month/12) %>% 
        filter(region %in% input$regions) %>% 
        filter(timeframe <= input$year)
    })
    
    output$tropPlot <- renderPlot({
       tropospherePlot() %>% 
        ggplot(aes(timeframe, temp, col=factor(region))) +
        geom_point() +
        geom_smooth(method="lm") +
        scale_color_brewer(palette = input$color) +
        labs(title = "Tropograph Temperature over Years", x = "Time Frame (Years)", y = "Temperature (Celcius)", col="Region")
    })
  
    output$plotInfo <- renderText({
      tropospherePlot() %>% 
        pull(temp) %>% 
        mean(., 2) %>% 
        paste("The average temperature for the given year range and region is:", .)
    })
    
  # Table Information  
    output$tropTable <- renderDataTable({
      troposphere %>% 
        select(input$filterby)
    })
}

shinyApp(ui = ui, server = server)
