# server.R
library(ggplot2)
library(shiny)

xstd <- 3
ystd <- 0.5
group_width <- 6
group_height <- 10



# Server ########################################################

shinyServer(function(input, output) {
    
    dfserver <- reactive({

        # Options #####################################

        # number_of_groups <- input$number_of_groups
        # points_per_group <- input$points_per_group
        # separation <- input$separation
        # show_abline <- input$show_abline
        # show_groups <- input$show_groups
        # show_group_ablines <- input$show_group_ablines

        # Data Frame building ############################

        # Fix anchor data frame of centers of each group.
        anchors_x <- rep(seq(2, 
                             2 + (input$number_of_groups - 1) * input$separation, 
                             by = input$separation),
                         input$points_per_group)
        anchors_y <- rep(seq(2, 
                             2 + (input$number_of_groups - 1) * input$separation, 
                             by = input$separation), 
                         input$points_per_group)
        anchors <- data.frame(x = anchors_x, y = anchors_y)
        
        # Fix groups to be added to rhe anchors
        branches_x <- rep(seq(-input$group_width/2, 
                              input$group_width/2, 
                              length.out = input$points_per_group), 
                          input$number_of_groups)
        branches_y <- rep(seq(input$group_height/2, 
                              -input$group_height/2, 
                              length.out = input$points_per_group), 
                          input$number_of_groups)
        branches <- data.frame(x = branches_x, y = branches_y)
        
        # Add noise.
        set.seed(11)
        noise_x <- rnorm(input$points_per_group * input$number_of_groups, 
                         sd = xstd)
        noise_y <- rnorm(input$points_per_group * input$number_of_groups, 
                         sd = ystd)
        noise <- data.frame(x = noise_x, 
                            y = noise_y)
        
        # Assemble data frame, and add factor groups indexed by capital letter.
        df <- anchors + branches + noise
        df$z <- rep(LETTERS[1:number_of_groups], points_per_group)
        
        # Modify factor group to include slopes, if group ablines are shown.  
        # This adds the slopes into the plot legend.
        if(input$show_group_ablines){
            for(i in 1:input$number_of_groups){
                dfs <- df[df$z == LETTERS[i], ]
                fit <- lm(y ~ x, data = dfs)
                slope <- fit$coefficients[2] %>% round(2)
                df[df$z == LETTERS[i], ]$z <- paste(df[df$z == LETTERS[i], ]$z,
                                                    ": slope = ",
                                                    as.character(slope), 
                                                    sep = "")
            }
        }
    })        
    
    output$plot <- renderPlot({
       
        # Plotting ##############################################
        # Plot x and y, and color by z if specified.
        p <- ggplot(data = dfserver(), aes(x, y)) 
        p <- p + geom_point(if(input$show_groups){aes(color = z)})
        p <- p + coord_fixed()
        # Show abline if specified.
        if(input$show_abline){
            p <- p + 
                geom_smooth(method = "lm", se = FALSE)
        }
        # Show group ablines if specified.
        if(input$show_group_ablines){
            dfserver()$z <- rep(LETTERS[1:input$number_of_groups], 
                                input$points_per_group)
            for(i in 1:input$numgrps){
                dfs <- dfserver()[dfserver()$z == LETTERS[i], ]
                p <- p + stat_smooth(data = dfs, method = "lm", 
                                     se = FALSE, fullrange = TRUE)
            }
        }
        # Return the plot.
        print(p)
    })

})
    