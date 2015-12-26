library(shiny)

# Plotting 
library(ggplot2)
library(rCharts)
library(ggvis)

# Data processing libraries
library(data.table)
library(reshape2)
library(dplyr)

# Required by includeMarkdown
library(markdown)

# It has to loaded to plot ggplot maps on shinyapps.io
library(mapproj)
library(maps)

# Load helper functions
source("helper.R", local = TRUE)

# Load data
baby_names <- fread("baby-names-by-state.csv", header = TRUE, sep = ",")
baby_names <- baby_names[!(baby_names$name %in% c("Unnamed","Unknown")), ]
baby_names <- baby_names[baby_names$number!="NA", ]

all_states <- sort(unique(baby_names$state))
all_names <- sort(unique(baby_names$name))

shinyServer(function(input, output, session) {
    
    #sidebar
    values <- reactiveValues()
    values$all_states <- c()
    
    output$choose_babyname <- renderUI({
        selectInput("name", "1. Name (==1) : ", all_names)
    })
    
    output$choose_states <- renderUI({
        selectInput("states", "3. States (>=1) : ", all_states,  selected=values$all_states, multiple=TRUE)
    })
    
    observe({
        if(input$select_all == 0) return()
        values$all_states <- all_states
    })
    
    #tab panel - by state
    state_agg <- reactive({
        aggregate_by_state(baby_names, input$name, input$range[1], input$range[2], input$states)
    })
    
    output$ByState <- renderPlot({
        states_map <- map_data("state")
        p <- ggplot(state_agg(), aes_string(fill = "number"))
        p <- p + geom_map(aes(map_id = state), map = states_map, colour = "white")
        p <- p + expand_limits(x = states_map$long, y = states_map$lat)
        p <- p + coord_map() + theme_bw()
        p <- p + labs(x = "Longitude", y = "Latitude")
        p <- p + scale_fill_gradient(low = "#132B43", high = "#56B1F7", name = "Number(%)")
        print(p)
    })
    
    #tab panel - by year
    year_agg <- reactive({
        aggregate_by_year(baby_names, input$name, input$range[1], input$range[2], input$states)
    })

    output$ByYear <- renderPlot({
        library(grid)
        p2 <- ggplot(year_agg(), aes(x = year, y = number))
        p2 <- p2 + geom_line(aes(colour = state))
        p2 <- p2 + labs(x = "Year", y = "Number") + theme_bw()
        p2 <- p2 + theme(plot.margin = unit(c(1,1,3,1), "cm"))
        p2
    })
    
})

