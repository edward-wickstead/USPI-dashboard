library(tidyverse)
library(readxl)
library(glue)
library(concatenate)
library(broom)
library(rgeos)


# Load raw data -----------------------------------------------------------

sheet_names = c("Prosperity Index",
                "Pillars x 11",
                "Elements x 48",
                "Indicators x 217")

index <- read_xlsx("raw_data/USPI_2021_State_Data.xlsx", 
                   sheet = sheet_names[1])
pillars <- read_xlsx("raw_data/USPI_2021_State_Data.xlsx",
                     sheet = sheet_names[2])
elements <- read_xlsx("raw_data/USPI_2021_State_Data.xlsx",
                      sheet = sheet_names[3])
indicators <- read_xlsx("raw_data/USPI_2021_State_Data.xlsx",
                        sheet = sheet_names[4]) %>% 
  mutate(across(starts_with("score_"), ~ .x * 100))

dfs <- list(index, pillars, elements, indicators)


# Create area lookup ------------------------------------------------------

area_lookup <- indicators %>% 
  select(area_name, area_code, area_group) %>% 
  unique()


# Measure look-up ---------------------------------------------------------------

determine_measure_level <- function(data) {
  level <- case_when(("indicator_name" %in% names(data)) ~ "Indicator",
                     ("element_name" %in% names(data)) ~ "Element",
                     ("pillar_name" %in% names(data)) ~ "Pillar",
                     TRUE ~ "Index")
  
  return(level)
}

get_unique_measures <- function(data) {
  
  data <- data %>% 
    select_if(names(.) %in% c('pillar_name', 'element_name', 'indicator_name'))
  
  level <- determine_measure_level(data)
  
  if (level != 'Index') {
    data <- data %>% 
      unique()
  }
  
  if (level == "Index") {
    data <- tibble(measure_name = "Prosperity",
                   element_name = NA,
                   pillar_name = NA,
                   measure_level = level)
  } else if (level == "Pillar") {
    data <- data %>% 
      mutate(measure_name = pillar_name,
             element_name = NA,
             measure_level = level)
  } else if (level == "Element") {
    data <- data %>% 
      mutate(measure_name = element_name,
             measure_level = level)
  } else {
    data <- data %>% 
      mutate(measure_level = level) %>% 
      rename(measure_name = indicator_name)
  }
  
  return(data)
}

measure_lookup <- dfs %>% 
  lapply(get_unique_measures) %>% 
  bind_rows() %>% 
  select(measure_level, pillar_name, element_name, measure_name)


# Combine tables ------------------------------------------------------------

get_measure_name <- function(data){
  level <- determine_measure_level(data)

  if (level == "Index") {
    data <- data %>% 
      mutate(measure_name = "Prosperity")
  } else if (level == "Pillar") {
    data <- data %>% 
      rename(measure_name = pillar_name)
  } else if (level == "Element") {
    data <- data %>% 
      rename(measure_name = element_name) %>% 
    select(-pillar_name)
  } else {
    data <- data %>% 
      rename(measure_name = indicator_name) %>% 
      select(-c(pillar_name, element_name))
  }
  
  return(data)
}

extract_and_pivot <- function(data) {
  data <- data %>% 
    select(-c(area_code, area_group)) %>% 
    pivot_longer(cols = starts_with(c("rank", "score", "raw_value")),
                 names_to = "measure_type",
                 values_to = "value") %>% 
    mutate(year = as.numeric(substr(measure_type, nchar(measure_type) - 3, nchar(measure_type))),
           measure_type = substr(measure_type, 1, nchar(measure_type) - 5))
  
  return(data)
}

all_data <- dfs %>%
  lapply(get_measure_name) %>% 
  lapply(extract_and_pivot) %>% 
  bind_rows()


# Separate measure types --------------------------------------------------

rank <- all_data %>% 
  filter(measure_type == 'rank') %>% 
  select(-measure_type) %>% 
  rename(rank = value)


combined_measures <- all_data %>% 
  filter(measure_type != 'rank') %>% 
  left_join(rank, by = c("area_name", "measure_name", "year")) %>% 
  select(area_name, year, measure_name, measure_type, value, rank)


# Hex map data load -------------------------------------------------------------

spdf <- geojson_read("raw_data/us_states_hexgrid.geojson",  what = "sp")

spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), id = spdf@data$iso3166_2))


# Filter measure level ----------------------------------------------------

filter_measure_level <- function(dataframe = measure_lookup, measure_levels) {
  
  filtered_measures <- measure_lookup %>% 
    filter(measure_level %in% measure_levels) %>% 
    select(measure_name)
  
  dataframe_filtered <- dataframe %>% 
    filter(measure_name %in% filtered_measures$measure_name)
  
  return(dataframe_filtered)
  
}


# Subset measures ---------------------------------------------------------

subset_measures <- function(dataframe, measures_to_subset) {
  
  
  
  return(dataframe)
  
}
