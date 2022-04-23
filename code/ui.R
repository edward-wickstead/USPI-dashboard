library(tidyverse)
library(shiny)
library(DT)
library(bslib)

source("code/tidy_raw_data.R")
source("code/graph_types.R")

fluidPage(tabsetPanel(
  
  # Trend through time by area
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
  
  # Hex-map by state
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
  
  # Distribution of scores/values by measure
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
             
           ))
))