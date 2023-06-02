# Fading Circles ----

# Experimenting with code-based art. 
# Taking inspiration from the beautiful work of [@alcrego](https://linktr.ee/alcrego), especially their [• Throbs • 1/4](https://twitter.com/alcrego_/status/1662676901222047748?s=46).

# Aim ----

# Create a figure with many semi-circles, arching across the top and bottom half of the figure. Circles are made of many small points, fading from white to transparent.

# Libraries ----
library(tidyverse)

# Function to create fading line ----
# 
# create_line_df <- function(x_start, x_end, steps, y, group){
#   # Function takes inputs:
#   # - "x_start": x-coordinate of the beginning of the line
#   # - "x_end": x-coordinate of the end of the line
#   # - "steps": distance between each point of the line on the x-axis
#   # - "y": the y-coordinate of each point of the line
#   # - "group": an integer identifying each line  
#   #
#   # Function returns a data.frame with variables:
#   # - "x": the x-coordinate of each point of the line
#   # - "alpha": the alpha value of each point (going from 1 to 0)
#   # - "y": the y-coordinate of each point of the line
#   # - "group": an integer identifying each line
#   
#   x <- seq(x_start, x_end, by = steps)  # define x-coords
#   y <- rep(y, times = length(x))  # define y-coords
#   alpha <- seq(1, 0, length.out = length(x))  # define alpha values
#   group <- rep(group, times = length(x))  # define group
#   
#   # Put variables in a data.frame:
#   df <- data.frame(
#     x = x,
#     y = y,
#     alpha = alpha,
#     group = group
#   )
#   
#   return(df)
#   
# }

create_line_df <- function(n_lines, x_start, x_end_range, x_end_jitter, x_steps_range, y_range, y_jitter){
  # Function to create a df to draw fading circles.
  # Inputs:
  # - "n_lines": number of lines to create
  # - "x_start": single value indicating x-coordinate of all line origins
  # - "x_end_range": vector of two, indicating the min and max x-coordinates of line endings
  # - "x_end_jitter": single value indicating absolute amount of jitter for x-coordinates of line endings
  # - "x_steps_range": vector of two, indicating min and max distance between points in each line
  # - "y_range": vector of two, indicating min and max line's y-coordinate
  # - "y_jitter": single value indicating absolute amount of jitter for line's y-coordinates
  #
  # Function returns a data.frame with variables:
  # - "x": the x-coordinate of each point of the line
  # - "y": the y-coordinate of each point of the line
  # - "alpha": the alpha value of each point (going from 1 to 0)
  # - "group": an integer identifying each line
  
  ## x_end coordinates ----
  #
  # Define the x_end coordinates for each line.
  # In general, we want more external lines to be shorter (fade faster). 
  # However, we don't want lines to be perfectly ordered by their length,
  # therefore we add some jitter to their ending point.

  x_end <- sort(  # order lines by length in ascending order
    sample(
      x_end_range[1]:x_end_range[2], 
      size = n_lines, 
      replace = TRUE
      )
    )  
  
  x_end <- x_end + sample(    # add some jitter to the line ending
    seq(-x_end_jitter, x_end_jitter, by = 0.1), 
    size = n_lines, 
    replace = TRUE
    )
  
  ## distance between points ----
  #
  # Define the distance between points of each line:
  x_steps <- sample(
    seq(x_steps_range[1], x_steps_range[2], by = 0.1), 
    size = n_lines, 
    replace = TRUE
    )
  
  ## y-coordinates ----
  #
  # Define the y-coordinate for each line.
  # We want some variation in the spacing between consecutive lines:
  y_coords <- sort(
    seq(y_range[1], y_range[2], length.out = n_lines) +  # define sequence of y-coords
    sample(  # add some variation
      seq(-y_jitter, y_jitter, by = y_jitter/10),
      size = n_lines,
      replace = TRUE
      ),
    decreasing = TRUE  # to keep the longer lines on the external side
  )
  
  ## groups ----
  #
  # Groups are simply one integer value per line:
  group <- 1:n_lines
  
  # create line df ----
  #
  # define a function that creates one line df from the x- and y-coordinates,
  # then map it to the values of each line defined above
  
  # define function:
  create_single_line <- function(x_start, x_end, y, x_steps, group){
    
    x <- seq(x_start, x_end, by = x_steps)  # define x-coords
    y <- rep(y, times = length(x))  # define y-coords
    alpha <- seq(1, 0, length.out = length(x))  # define alpha values
    group <- rep(group, times = length(x))  # define group
    
    # Put variables in a data.frame:
    df <- data.frame(
      x = x,
      y = y,
      alpha = alpha,
      group = group
    )
    
    return(df)
  }
  
  # map it to all lines:
  lines_list <- pmap(list(
    x_start,
    x_end,
    y_coords,
    x_steps,
    group
    ),
    create_single_line
  )
  
  # Combine all dfs:
  lines_df <- lines_list %>% reduce(full_join)
  
  return(lines_df)
  
}

# Create lines ----

set.seed(333)

# Define top lines:
lines_top_df <- create_line_df(
  n_lines = 50,
  x_start = 0,
  x_end_range = c(10, 100),
  x_end_jitter = 5,
  x_steps_range = c(0.1, 1),
  y_range = c(5, 20),
  y_jitter = 0.1
)

# Define bottom lines:
lines_bottom_df <- create_line_df(
  n_lines = 50,
  x_start = 100,
  x_end_range = c(110, 200),
  x_end_jitter = 5,
  x_steps_range = c(0.1, 1),
  y_range = c(5, 20),
  y_jitter = 0.1
)

# Adjust group for bottom lines and combine dfs:
lines_bottom_df <- lines_bottom_df %>% 
  mutate(group = group + 50)

lines_df <- rbind(lines_top_df, lines_bottom_df)

# Create points ----

# On top of each line, we want to add a brighter point, where the line is between 20 and 30% transparent.
points_df <- lines_df %>% 
  filter(alpha >= 0.20 & alpha <= 0.30) %>%
  group_by(group) %>% 
  slice_sample(n = 1) %>%   # select one random value per line
  select(x, y, group)

# Create horizontal lines ----

# Create two horizontal lines touching the beginning of each line, on the left and the right.
# Since the plot is made by applying polar coordinates, we must actually create vertical lines.
hori_df <- data.frame(
  x = c(0, 100),  # beginning and end x-coords of the two lines
  y_start = rep(min(lines_df$y), 2),  # beginning y-coords (y-coord of the inner most line)
  y_end = rep(max(lines_df$y), 2)  # end y-coords (y-coord of the outer most line)
)

# Plotting ----

fading_circles_plot <- lines_df %>% 
  ggplot(aes(x = x, y = y, alpha = alpha, group = group)) +
  geom_point(size = 0.5, color = "gray70", stroke = NA) +  # add lines
  geom_point(  # add bright points
    data = points_df,
    aes(x = x, y = y, group = group),
    inherit.aes = FALSE,
    size = 0.6,
    color = "white",
    stroke = NA
  ) + 
  geom_segment(  # add horizontal lines
    data = hori_df,
    aes(x = x, xend = x, y = y_start, yend = y_end),
    inherit.aes = FALSE,
    linewidth = 0.4,
    color = "white"
  ) +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(limits = c(2, 20)) +
  coord_polar(start = -pi/2) +  # move the x origin left
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    legend.position = "none"
  )

ggsave("fading-circles/fading_circles.png", plot = fading_circles_plot, width = 3000, height = 3000, units = "px", dpi = 300)
