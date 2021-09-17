# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: global.R for a Shiny app that shows times from Idaho
# Interscholastic Cycling League races.

# Load resources
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(dplyr)
library(forcats)
library(lubridate)
library(feather)

# Specify & set custom ggplot theme
my_theme <- function() {
    theme_classic() %+replace%
        theme(
            panel.grid.major = element_line(color = "gray90", size = .3)
            ,panel.spacing = unit(1, "lines")
            ,strip.background = element_rect(color = NA)
            ,strip.text = element_text(size = 10)
            ,axis.title.y = element_text(angle = 0, vjust = .5)
        )
}
theme_set(my_theme())

# Read in data
df <- read_feather("data_long.feather") %>%
    filter(!is.na(time_min)) %>%
    mutate(Race = as_factor(paste(Location, year(Date)))
           
           # Levels of Race are ordered from most to least recent
           ,Race = fct_reorder(Race, Date, .desc = TRUE)
           ,Division = paste(Division, Gender),
           
           # This is to give a clean version of time_char to show in tooltips
           `Lap Time` = time_char)

# List of riders in alphabetical order
rider_list <- df %>%
    pull(Name) %>%
    unique() %>%
    sort()

# List of divisions
division_list <- df %>%
    pull(Division) %>%
    unique() %>%
    sort()

# List of places starting with 1
place_list <- df %>%
    pull(Place) %>%
    unique()

# List of races
race_list <- levels(df$Race)

# Maximum number of riders that can be displayed
max_riders <- 10
