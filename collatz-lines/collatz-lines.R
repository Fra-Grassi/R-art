# Collatz Lines ----

# An attempt to draw lines in a generative manner, by using the set of rules of the Collatz Conjecture.  
# The idea comes from the reading of the [aRtsy project](https://cran.r-project.org/web/packages/aRtsy/readme/README.html).
# NOTE: the core of the code, which changes the slope of the line depending on whether one element of the Collatz sequence is even or odd was directly inspired by the [respecitve function from the aRtsy package](https://github.com/koenderks/aRtsy/blob/development/src/canvas_collatz.cpp).

# Algorithm ----

# The algorithm to create Collatz lines is based on the following rules:

# Take a random positive number.
# If the number is even, divide it by 2.
# If the number is odd, multiply the number by 3 and add 1.
# Repeat to get a sequence of numbers.

# Libraries ----
library(tidyverse)
library(scico)
library(ggpubr)

# Function to create Collatz lines ----
draw_collatz <- function(n_lines = 100, seed = 333, angle = pi/2, change_even = 0.007, change_odd = 0.015, col_pal = "romaO", col_bg = "white", invert_x = FALSE, invert_y = FALSE, flip_coords = FALSE) {
  

  # Function to plot lines created by applying Collatz algorithm.
  #
  # Input:
  # - "n_lines": number of lines to plot
  # - "seed": value to set random number generator for sampling initial input for the Collatz algorithm
  # - "angle": initial value of the angle used to draw the line
  # - "change_even": value subtracted from the angle when a value in the Collatz series is even
  # - "change_odd": value added to the angle when a valie in the Collatz series is odd
  # - "col_pal": palette of colors to draw lines. At the moment, only allowed pallettes from library "scico"
  # - "col_bg": background color of the plot
  # - "invert_x", "invert_y", "flip_coords": if TRUE, invert the direction of the x, or y axis, or flip coordinates, respectively
  # Output:
  # - a ggplot object "collatz_plot"
  
  ## Function to apply Collatz algorithm ----
  compute_collatz <- function(start, stop_iter = Inf){
    # Function to apply Collatz algorithm.
    # 
    # Input:
    # - "start": integer, starting point to apply the algorithm
    # - "stop_iter": integer, highest number of iterations to apply
    #
    # The output is a vector containing the computed values across each iteration of the algorithm,
    # until 1 is reached, or until the highest number of iterations is reached.
    
    n <- start  # start algorithm with input number
    collatz_vec <- c(n)  # assign it to output vector
    
    # Keep going till bottom of the series (1) is reached, or when maximum number of iterations is reached:
    while (n != 1 && length(collatz_vec) < stop_iter) {
      
      # Apply algorithm:
      if (n %% 2 == 0){
        n <- n/2  # if number if even, divide it by 2
      } else {
        n <- n*3 + 1  # if odd, multiply by 3 and add 1
      }
      
      collatz_vec <- append(collatz_vec, n)  # add number to output vector
    }
    
    return(collatz_vec)
  }
  
  ## Function to create Collatz lines ----
  collatz_lines <- function(angle, change_even, change_odd){
    # Function to create lines via Collatz algorithm.
    # Function applies the Collatz algorithm starting from a random input number.
    # Then, it uses the output to define the x- and y-coordinates of a line by the rules:
    # - first point of the line has coord (0, 0)
    # - the coordinates of the n-th point are defined by adding a vector of length 1 and angle "angle" to the coordinates of the n-th - 1 element.
    # - if the n-th element of the Collatz series is even:
    # - - "angle" value is decreased by "change_even"
    # - if it's odd:
    # - -"angle" value is increased by "change_odd"
    #
    # Output is a data.frame containing x- and y-coordinates of the line (x-coord start from 1)
    
    start_n <- sample.int(10^6, 1)  # define random starting point for Collatz algorithm
    
    input_data <- compute_collatz(start_n, stop_iter = 1000)  # apply Collatz algorithm
    
    x_coords <- rep(0, times = length(input_data) + 1)  # pre-allocate vector to store x-coords
    y_coords <- rep(0, times = length(input_data) + 1)  # pre-allocate vector to store y-coords
    
    for (i in 1:length(input_data)){
      
      x_coords[i+1] <- x_coords[i] + cos(angle)
      y_coords[i+1] <- y_coords[i] + sin(angle)
      
      if (input_data[i] %% 2 == 0){
        angle <- angle - change_even
      } else {
        angle <- angle + change_odd
      }
      
    }
    
    # Define output df:
    line_df <- data.frame(
      x = x_coords,
      y = y_coords
    )
    
    return(line_df)
  }
  
  ## Create Collatz lines ----
  #
  # Now create a series of lines and add an unique identifier to each 
  
  set.seed(seed)
  
  collatz_list <- seq(1:n_lines) %>% 
  map(
    ~ {
      df <- collatz_lines(angle, change_even, change_odd)  # generate x- and y-coords
      df$id <- .x  # add identifier
      return(df)
    }
  )

  collatz_df <- collatz_list %>% reduce(full_join)  # combine all df in one
  
  ## Add alpha level ----

  # We want each line to fade toward the end.
  # Therefore, add a new column indicating alpha level from 1 (beginning of the line) to 0 (end of the line)
  collatz_df <- collatz_df %>% 
    group_by(id) %>% 
    mutate(alpha = seq(1, 0, length.out = n()))
  
  ## Create plot ----
  
  # Make basic plot:
  collatz_plot <- collatz_df %>% 
    ggplot(aes(x = x, y = y, group = id, color = id, alpha = alpha)) +
    geom_path() +
    scale_color_scico(palette = col_pal) + 
    theme_void() +
    theme(
      plot.background = element_rect(fill = col_bg, colour = NA),
      legend.position = "none"
    )
  
  # Modify plot based on inputs:
  if (invert_x) {
    collatz_plot <- collatz_plot + scale_x_reverse()
  }
  
  if (invert_y) {
    collatz_plot <- collatz_plot + scale_y_reverse()
  }
  
  if (flip_coords) {
    collatz_plot <- collatz_plot + coord_flip()
  }
  
  return(collatz_plot)

}

# Plotting ----

# Plot some figures playing around with function parameters:

collatz_plot_1 <- draw_collatz(
  n_lines = 130,
  seed = 999,
  angle = pi/3,
  change_even = 0.015, 
  change_odd = 0.01, 
  col_pal = "buda", 
  col_bg = "#1C3738", 
  invert_x = FALSE, 
  invert_y = FALSE, 
  flip_coords = FALSE
)

collatz_plot_1

collatz_plot_2 <- draw_collatz(
  n_lines = 200,
  seed = 33,
  angle = 2*pi,
  change_even = 0.03, 
  change_odd = 0.01, 
  col_pal = "vikO", 
  col_bg = "white", 
  invert_x = FALSE, 
  invert_y = FALSE, 
  flip_coords = FALSE
)

collatz_plot_2

collatz_plot_3 <- draw_collatz(
  n_lines = 150,
  seed = 9292,
  angle = pi/5,
  change_even = 0.008, 
  change_odd = 0.03, 
  col_pal = "nuuk", 
  col_bg = "#A53860", 
  invert_x = FALSE, 
  invert_y = FALSE, 
  flip_coords = FALSE
)

collatz_plot_3

collatz_plot_4 <- draw_collatz(
  n_lines = 400,
  seed = 20,
  angle = 2*pi,
  change_even = 0.035, 
  change_odd = 0.09, 
  col_pal = "acton", 
  col_bg = "#96BDC6", 
  invert_x = TRUE, 
  invert_y = FALSE, 
  flip_coords = FALSE
)

collatz_plot_4

# Arrange and save

collatz_plot <- ggarrange(collatz_plot_1,
                          collatz_plot_2,
                          collatz_plot_3,
                          collatz_plot_4,
                          nrow = 1)

collatz_plot
ggsave("collatz-lines/img/collatz-lines.png", plot = collatz_plot, width = 6000, height = 1500, units = "px", dpi = 300)
