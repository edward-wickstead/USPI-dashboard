library(tidyverse)
library(ggthemes)
library(geojsonio)
library(broom)
library(viridis)
library(ggstance)
library(gghighlight)
library(ggrepel)
source("code/tidy_raw_data.R")


# Define palettes ---------------------------------------------------------

viridis_palette <- rev(magma(8))[c(-1,-8)]


# Trend through time ------------------------------------------------------

trend_through_time_plot <- function(data_type, measures, areas = c("Alabama", "California", "Delaware"), min_year = 2004, max_year = 2021) {
  
  data <- combined_measures %>% 
    filter(measure_type == str_to_lower(gsub(" ", "_", data_type)))
  
  y_lims <- data %>% 
    filter(measure_name %in% measures) %>% 
    summarize(min = plyr::round_any(min(value), 10, f = floor),
              max = plyr::round_any(max(value), 10, f = ceiling))
  
  plot_data <- data %>% 
    filter(year >= min_year,
           year <= max_year,
           area_name %in% areas,
           measure_name %in% measures) %>%
    ggplot(aes(x = year, y = value, color = area_name)) +
    geom_line(size = 1.5,
              alpha = 0.8) +
    
    theme_fivethirtyeight() +
    theme(axis.title = element_text(),
          axis.title.x = element_blank()) +
    labs(title = glue("{measures} in {cc_and(areas, oxford = TRUE)}"),
         subtitle = glue("{min_year}-{max_year}"),
         color = "State") +
    scale_color_brewer(palette = "Set1") +
    scale_x_continuous(breaks = 0:2100)
  
  if (data_type == "Score") {
    plot_data <- plot_data +
      ylim(y_lims[[1]], y_lims[[2]]) +
      ylab(paste0(measures, " (Score)"))
  } else {
    plot_data <- plot_data +
      ylab(measures)
  }
  
  return(plot_data)
}


# Hex plot ---------------------------------------------------------------

hex_map <- function(measures, map_year) {
  
  data <- rank %>% 
    filter(measure_name %in% measures,
           year == map_year) %>% 
    mutate(bin = cut(rank, breaks = c(1, 10, 20, 30, 40, 51), 
                     labels = c("1-10", "11-20", "21-30", "31-40", "41-51"), 
                     include.lowest = TRUE)) %>% 
    right_join(spdf_fortified, by = c("area_name" = "id"))
  
  plot_data <- data %>%
    ggplot() +
    geom_polygon(data = data, aes(fill = bin, x = long, y = lat, group = group), size = 0, alpha = 0.9) +
    geom_text(data = centers, aes(x = x, y = y, label = id), color = "white", size = 3, alpha = 0.6) +
    theme_void() +
    scale_fill_manual(values = viridis_palette, name = "Rank",
                      guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                           keywidth = unit(12, units = "mm"), 
                                           label.position = "bottom", 
                                           title.position = 'top', 
                                           nrow = 1)) +
    theme(
      legend.position = c(0.5, 0.9),
      text = element_text(color = "#22211d"),
      plot.title = element_text(size = 22, hjust = 0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
    ) +
    coord_map() +
    ggtitle(glue("{measures} in the United States, {map_year}"))
    
    return(plot_data)
  
}



# Density plot -----------------------------------------------------------

density_plot <- function(indicator, selected_year = NULL) {
  
  data <- combined_measures %>% 
    filter_measure_level(measure_levels = "Indicator") %>% 
    filter(measure_type == "raw_value",
           measure_name == indicator) %>% 
    select(-measure_type, -rank)
  
  plot_output <- data %>% 
    ggplot(aes(x = value, y = 0)) +
    geom_violin(aes(group = year),
                alpha = 0.3,
                colour = FALSE,
                fill = "#2597C5",
                position = "identity") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          plot.subtitle = element_text(size = 16)) +
    labs(title = glue("Distribution of {str_to_lower(indicator)} indicator values"),
         subtitle = ifelse(!is.null(selected_year), glue("{selected_year}"),""))
  
  if (!is.null(selected_year)) {
    plot_output <- plot_output +
      gghighlight(year == selected_year)
    
    points <- data %>% 
    filter(year == selected_year,
           value %in% boxplot.stats(value)$out)
    
    if (dim(points)[1] > 0) {
    plot_output <- plot_output +
      geom_point(data = points,
                 aes(x = value, y = 0),
                 fill = "#000000",
                 colour = "#000000") +
      geom_text_repel(data = points,
                      aes(x = value, y = 0, label = area_name),
                      nudge_y = 0.05,
                      nudge_x = 0.5,
                      hjust = 0)
    }
    
  } else {
    
    points <- data %>% 
      filter(value %in% boxplot.stats(value)$out) %>% 
      select(-year) %>% 
      distinct()
    
    if (dim(points)[1] > 0) {
      plot_output <- plot_output +
        geom_point(data = points,
                   aes(x = value, y = 0),
                   fill = "#000000",
                   colour = "#000000") +
        geom_text_repel(data = points,
                        aes(x = value, y = 0, label = area_name),
                        nudge_y = 0.05,
                        nudge_x = 0.5,
                        hjust = 0)
    }
  }
  
  return(plot_output)
  
}

