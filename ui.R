# ui.R
library(shiny)
library(ggplot2)
shinyUI(fluidPage(
    titlePanel("Simpson's Paradox"),
    sidebarLayout(
        sidebarPanel(
            selectInput("number_of_groups", 
                        label = h3("Choose the number of groups."), 
                        choices = 2:10,
                        selected = 6
                        ),
            selectInput("points_per_group", 
                        label = h3("Choose the number of points in each group."), 
                        choices = 2:200,
                        selected = 20
                        ),
            selectInput("separation", 
                        label = h3("Choose how separated the groups should be. 1 is least separated and 10 is most separated."), 
                        choices = 1:10,
                        selected = 5
                        ),
            checkboxInput("show_abline", "Show the regression line for the entire data set.", FALSE),
            checkboxInput("show_groups", "Show the data by color coded groups.", FALSE),
            checkboxInput("show_group_ablines", "Show the regression line for each group.", FALSE)
        ),
        mainPanel(
            h3("Plot"),
            plotOutput("plot")
        )
    )
))