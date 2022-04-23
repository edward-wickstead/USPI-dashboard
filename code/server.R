library(tidyverse)
library(shiny)
library(DT)
library(bslib)

source("code/tidy_raw_data.R")
source("code/graph_types.R")

function(input, output, session) {
  
  output$trend <- renderPlot({
    trend_through_time_plot(data_type = input$trend_measure_type, 
                            measures = input$trend_measure, 
                            areas = input$state, 
                            min_year = input$trend_year[1],
                            max_year = input$trend_year[2])
  })
  
  output$map <- renderPlot({
    hex_map(measures = input$map_measure,
            map_year = input$map_year)
  })  
  
  output$density_plot <- renderPlot({
    density_plot(indicator = input$density_measure,
                 selected_year = input$density_year)
  })
  
  observe({
    updateSelectInput(session, "trend_measure", 
                      choices = combined_measures %>% 
                        filter(measure_type == gsub(" ", "_", str_to_lower(input$trend_measure_type))) %>% 
                        select(measure_name) %>% 
                        unique() %>% 
                        pull())
  })
  
}