# ui.R
library(shiny)
library(ggplot2)
shinyUI(fluidPage(
    titlePanel("Simpson's Paradox Explored Graphically"),
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
            sliderInput("separation", 
                        label = h3("Choose how separated the groups should be. 1 is least separated and 20 is most separated."),
                        min = 1, max = 20, value = 5),
            checkboxInput("show_abline", "Show the regression line for the entire data set.", FALSE),
            checkboxInput("show_groups", "Show the data by color coded groups.", FALSE),
            checkboxInput("show_group_ablines", "Show the regression line for each group.", FALSE)
        ),
        mainPanel(
            h3("Instructions/Documentation"),
            helpText("Adjust the choices in the side bar to manipulate this plot.  For most configurations, there will be an overall increasing trend in the data.  However, if you show the data by groups, then you'll see that within each group there is a negative trend.  Play with the options to see if you can make the presense of a confounder more obvious or less before the groupings are revealed.  Acknowledgement to @rafalab (see link below), whose gif inspired this project."),
            uiOutput("tweetlink"),
            plotOutput("plot"),
            h4(textOutput("slopetext"), align = "center")
        )
    )
))