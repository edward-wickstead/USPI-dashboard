library(tidyverse)
library(shiny)
library(DT)
library(bslib)

source("code/tidy_raw_data.R")
source("code/graph_types.R")


# UI ----------------------------------------------------------------------

ui <- fluidPage(tabsetPanel(
  
  tabPanel("Trends",
           theme = bs_theme(bootswatch = "yeti"),
           titlePanel("US Prosperity Index"),
        
           sidebarLayout(
             
             sidebarPanel(
               
               sliderInput(inputId = "trend_year",
                         label = "Years shown",
                         min = 2004,
                         max = 2021,
                         value = c(2004, 2021),
                         step = 1,
                         round = TRUE,
                         sep = "",
                         ticks = FALSE),
               
               radioButtons(inputId = "trend_measure_type",
                          label = "Select measure type: ",
                          choices = c("Score", "Raw value"),
                          selected = "Score"),
               
               selectInput(inputId = "state",
                         label = "State",
                         choices = sort(area_lookup$area_name),
                         selected = "Alabama",
                         multiple = TRUE),
               
               selectInput(inputId = "trend_measure",
                        label = "Measure",
                        choices = "Net electricity generation"),
               
               width = 2
               
               ),
           
           mainPanel(
             plotOutput("trend",
                        height = 800)
             )
           
           )),
  
  tabPanel("Map",
           theme = bs_theme(bootswatch = "yeti"),
           titlePanel("US Prosperity Index"),
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput(inputId = "map_measure",
                           label = "Measure",
                           choices = sort(measure_lookup$measure_name),
                           selected = "Prosperity"),
               
               selectInput(inputId = "map_year",
                           label = "Select year: ",
                           choices = seq(min(rank$year), max(rank$year)),
                           selected = 2021),
               
               width = 2
               
             ),
             
             mainPanel(
               plotOutput("map",
                          height = 800)
             )
             
           )),
  
  tabPanel("Distribution",
           theme = bs_theme(bootswatch = "yeti"),
           titlePanel("US Prosperity Index"),
           
           sidebarLayout(
             
             sidebarPanel(
               
               selectInput(inputId = "density_measure",
                           label = "Indicator",
                           choices = sort(unique(filter_measure_level(combined_measures, measure_levels = "Indicator")$measure_name)),
                           selected = "Mass shooting deaths"),
               
               radioButtons(inputId = "density_year",
                            label = "Select year to highlight: ",
                            choices = seq(2021, 2004))
               
             ),
             
             mainPanel(
               plotOutput("density_plot",
                          height = 800)
             )
             
           )),
  
  # tabPanel("Missing values",
  #          theme = bs_theme(bootswatch = "yeti"),
  #          titlePanel("US Prosperity Index"),
  # 
  #          sidebarLayout(
  # 
  #            sidebarPanel(
  # 
  #              checkboxInput(inputId = "missing_all_only",
  #                            label = "Only display if values are missing for all years",
  #                            value = TRUE),
  # 
  #              checkboxInput(inputId = "hide_missing_years",
  #                            label = "Omit years in summary",
  #                            value = TRUE),
  # 
  #              width = 2
  # 
  #            ),
  # 
  #            mainPanel(
  #              dataTableOutput("missing_areas")
  #            )
  # 
  #          ))
))



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
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
  
  # output$missing_areas <- renderDataTable({
  #   check_missing_areas(dataframe = combined_measures,
  #                       missing_all_only = TRUE)
  # })
  
  observe({
    updateSelectInput(session, "trend_measure", 
                      choices = combined_measures %>% 
                        filter(measure_type == gsub(" ", "_", str_to_lower(input$trend_measure_type))) %>% 
                        select(measure_name) %>% 
                        unique() %>% 
                        pull())
  })
  
}


# Main --------------------------------------------------------------------

shinyApp(ui, server)
