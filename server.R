# server.R
library(ggplot2)
library(shiny)
library(magrittr)

xstd <- 3
ystd <- 0.5
group_width <- 6
group_height <- 10

# Server ########################################################

shinyServer(function(input, output) {
    
    dfserver <- reactive({

        # Options #####################################

        ng <- as.integer(input$number_of_groups)
        ppg <- as.integer(input$points_per_group)
        sep <- as.integer(input$separation)
        shln <- as.logical(input$show_abline)
        shgp <- as.logical(input$show_groups)
        shgplns <- as.logical(input$show_group_ablines)

        # Data Frame building ############################

        # Fix anchor data frame of centers of each group.
        anchors_x <- rep(seq(2, 
                             2 + (ng - 1) * sep, 
                             by = sep),
                         ppg)
        anchors_y <- rep(seq(2, 
                             2 + (ng - 1) * sep, 
                             by = sep), 
                         ppg)
        anchors <- data.frame(x = anchors_x, y = anchors_y)
        
        # Fix groups to be added to rhe anchors
        branches_x <- rep(seq(-group_width/2, 
                              group_width/2, 
                              length.out = ppg), 
                          ng)
        branches_y <- rep(seq(group_height/2, 
                              -group_height/2, 
                              length.out = ppg), 
                          ng)
        branches <- data.frame(x = branches_x, y = branches_y)
        
        # Add noise.
        set.seed(11)
        noise_x <- rnorm(ppg * ng, 
                         sd = xstd)
        noise_y <- rnorm(ppg * ng, 
                         sd = ystd)
        noise <- data.frame(x = noise_x, 
                            y = noise_y)
        
        # Assemble data frame, and add factor groups indexed by capital letter.
        df <- anchors + branches + noise
        df$z <- rep(LETTERS[1:ng], ppg)
        
        # Modify factor group to include slopes, if group ablines are shown.  
        # This adds the slopes into the plot legend.
        if(shgplns){
            for(i in 1:ng){
                dfs <- df[df$z == LETTERS[i], ]
                fit <- lm(y ~ x, data = dfs)
                slope <- fit$coefficients[2] %>% round(2)
                df[df$z == LETTERS[i], ]$z <- paste(df[df$z == LETTERS[i], ]$z,
                                                    ": slope = ",
                                                    as.character(slope), 
                                                    sep = "")
            }
        }
        
        print(df)
    })        
    
    output$plot <- renderPlot({
       
        ng <- as.integer(input$number_of_groups)
        ppg <- as.integer(input$points_per_group)
        sep <- as.integer(input$separation)
        shln <- as.logical(input$show_abline)
        shgp <- as.logical(input$show_groups)
        shgplns <- as.logical(input$show_group_ablines)
        
        df <- as.data.frame(dfserver())
        
        # Plotting ##############################################
        # Plot x and y, and color by z if specified.
        p <- ggplot(data = df, aes(x, y)) 
        p <- p + geom_point(if(shgp){aes(color = z)})
        p <- p + coord_fixed()
        # Show abline if specified.
        if(shln){
            p <- p + 
                geom_smooth(method = "lm", se = FALSE)
        }
        # Show group ablines if specified.
        if(shgplns){
            df$z <- rep(LETTERS[1:ng], 
                                ppg)
            for(i in 1:ng){
                dfs <- df[df$z == LETTERS[i], ]
                p <- p + stat_smooth(data = dfs, method = "lm", 
                                     se = FALSE)
            }
        }
        # Return the plot.
        print(p)
    })
    
    output$slopetext <- renderText({
        shln <- as.logical(input$show_abline)
        df <- as.data.frame(dfserver())
        if(shln){
            fit <- lm(y ~ x, data = df)
            slope <- fit$coefficients[2] %>% round(2)
            st <- paste("All data: slope = ", slope)
        }
    })
    
    output$tweetlink <- renderUI({
        url <- a("Original gif", href = "https://twitter.com/rafalab/status/890935480493146112")
        tagList("", url)
    })

})
    