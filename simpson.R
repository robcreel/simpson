library(ggplot2)
library(magrittr)

# Options ########################################

number_of_groups <- 6
points_per_group <- 20
separation <- 5
xstd <- 3
ystd <- 0.5
group_width <- 6
group_height <- 10
show_abline <- FALSE 
show_groups <- FALSE
show_group_ablines <- FALSE


# Data Frame building ############################

# Fix anchor data frame of centers of each group.
anchors_x <- rep(seq(2, 
                     2 + (number_of_groups - 1) * separation, 
                     by = separation),
                 points_per_group)
anchors_y <- rep(seq(2, 
                     2 + (number_of_groups - 1) * separation, 
                     by = separation), 
                 points_per_group)
anchors <- data.frame(x = anchors_x, y = anchors_y)

# Fix groups to be added to rhe anchors
branches_x <- rep(seq(-group_width/2, 
                      group_width/2, 
                      length.out = points_per_group), 
                  number_of_groups)
branches_y <- rep(seq(group_height/2, 
                      -group_height/2, 
                      length.out = points_per_group), 
                  number_of_groups)
branches <- data.frame(x = branches_x, y = branches_y)

# Add noise.
set.seed(11)
noise_x <- rnorm(points_per_group * number_of_groups, sd = xstd)
noise_y <- rnorm(points_per_group * number_of_groups, sd = ystd)
noise <- data.frame(x = noise_x, y = noise_y)

# Assemble data frame, and add factor groups indexed by capital letter.
df <- anchors + branches + noise
df$z <- rep(LETTERS[1:number_of_groups], points_per_group)

# Modify factor group to include slopes, if group ablines are shown.  
# This adds the slopes into the plot legend.
if(show_group_ablines){
    for(i in 1:number_of_groups){
        dfs <- df[df$z == LETTERS[i], ]
        fit <- lm(y ~ x, data = dfs)
        slope <- fit$coefficients[2] %>% round(2)
        df[df$z == LETTERS[i], ]$z <- paste(df[df$z == LETTERS[i], ]$z,
                                            ": slope = ",
                                            as.character(slope), 
                                            sep = "")
    }
}


# Plotting ##############################################
# Plot x and y, and color by z if specified.
p <- ggplot(df, aes(x, y)) + geom_point(if(show_groups){aes(color = z)})
p <- p + coord_fixed()
# Show abline if specified.
if(show_abline){
    p <- p + 
        geom_smooth(method = "lm", se = FALSE)
}
# Show group ablines if specified.
if(show_group_ablines){
    df$z <- rep(LETTERS[1:number_of_groups], points_per_group)
    for(i in 1:number_of_groups){
        dfs <- df[df$z == LETTERS[i], ]
        p <- p + stat_smooth(data = dfs, 
                             method = "lm", 
                             se = FALSE, 
                             fullrange = TRUE)
    }
}

# Show the plot.
show(p)