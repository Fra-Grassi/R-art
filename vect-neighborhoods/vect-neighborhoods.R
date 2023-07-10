# Vectorized Neighborhoods

# An exercise to replicate [Antonio Sánchez Chinchón's](https://fronkonstin.com/2021/01/02/neighborhoods-experimenting-with-cyclic-cellular-automata/) approach to create art via cyclic cellular automata.

# Libraries ----
library(tidyverse)
library(reshape2)
source("vect-neighborhoods/neighborhood-rules.R")

# Settings ----

W <- 1000  # width of cell world
H <- 1000  # height of cell world
M <- 4  # maximum number of states allowed
depth <- 9  # depth (in cell number) of neighbors count
threshold <- 30  # min number of neighbors with state 1 unit greater than each cell
iterations <- 200  # number of world evolution iterations

# Function to generate world matrix ----
mat <- matrix(
  sample(
    x = seq_len(M) - 1,
    size = W*H,
    replace = TRUE
  ),
  nrow = H,
  ncol = W
)

# Function to compute neighbors state ----

count_neighbors <- function(mat, rule, depth) {
  # Function to find neighbors of a given world cell with state value exactly 1 unit higher than the given cell.
  # Neighbors are defined according to a specified rule, at a specified depth
  # Inputs:
  # - "mat": [matrix] world matrix
  # - "rule": [character] neighborhood rule to use. Supported rules are: 
  #   - "moore"
  #   - "vonNeumann"
  #   - "diagonal"
  #   - "shapeS"
  #   - "inv_shapeS"
  # - "depth": [integer] indicates depth at which neighbors are checked
  # Output:
  # - "neigh_count": [matrix] with same dimensions as "mat", indicating for each cell the number of neighbors with state exactly 1 unit higher than the given cell
  
  # Find neighbors ----
  
  # Find world matrix dimensions:
  n_row <- dim(mat)[[1]]
  n_col <- dim(mat)[[2]]
  
  # Create list of shifted matrices representing all neighbors of each cell of world matrix, according to specified rule:
  neigh_list <- neigh_fun(mat, rule, n_row, n_col, depth)

  # Check neighbors state ----
  
  ## Function to compare state values ----
  # Function finds which element in a shifted world matrix has state exactly 1 unit higher than corresponding cell in world matrix
  # Returns a matrix with 1s for elements that meet criterion, 0s otherwise
  find_matching_neigh <- function(shifted_mat){
    
    matching_neigh <- matrix(0, nrow = n_row, ncol = n_col)  # pre-allocate output matrix
    matching_neigh[shifted_mat - (mat + 1)%%M == 0] <- 1  # find neighbors exactly 1 unit higher
    return(matching_neigh)
  }
  
  matching_list <- lapply(neigh_list, find_matching_neigh)  # compare state values for all shifted world matrices
  
  # Calculate number of matching neighbors ----
  # Finally, count how many neighbors, in any direction, have state 1 unit higher than given cell:
  neigh_count <- Reduce("+", matching_list)
  
  return(neigh_count)
  
}

# Function to update world state ----

update_world <- function(mat, rule, depth, threshold, M){
  # Function to update world matrix.
  # Inputs:
  # - "mat": [matrix] world matrix
  # - "rule": [character] neighborhood rule to use. (See function "neigh_fun" for details)
  # - "depth": [integer] indicates depth at which neighbors are checked
  # - "threshold": [integer] minimum number of neighbors with value 1 unit higher than given cell, to update cell status
  # - "M": [integer] highest allowed value for cell state
  # Output:
  # - "updated_mat": [matrix] with same dimensions as "mat", with updated cell states
  #
  # Function applies the following algorithm:
  # - for each cell in world matrix, finds how many neighbors up to specified "depth" have state value exactly 1 unit higher than cell state
  # - if the number of neighbors in such state is higher than "threshold", increase cell state of 1
  # - if cell state becomes higher than "M", it returns to 0
  
  # Find neighbors matching criterion ----
  neigh_count <- count_neighbors(mat, rule, depth)
  
  # Update world status ----
  
  updated_mat <- mat  # pre-allocate updated world matrix
  
  updated_mat[neigh_count >= threshold] <- (updated_mat[neigh_count >= threshold] + 1) %% M  # increase cell state if neighbors above threshold, modulo M
  
  return(updated_mat)
}

# Evolve world state ----

# Update world status for specified number of iterations:
for(i in 1:iterations){
  
  print(i)
  mat <- update_world(
    mat = mat,
    rule = "blades",
    depth = depth,
    threshold = threshold,
    M = M
  )
  
}

# Plot world matrix ----

# Turn world matrix into a data.frame, with one column indicating the state value of each cell, and two columns for the x- and y-coordinates
df <- melt(mat)
colnames(df) <- c("x","y","v")

cols <- c("#ff6d00", "#ff7900", "#ff8500", "#ff9100")  # color palette

ggplot(data = df, aes(x = x, y = y, fill = v)) +
  geom_raster(interpolate = TRUE) +  # interpolate for a "smoother" look
  coord_equal() +
  scale_fill_gradientn(colours = cols) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggsave("vect-neighborhoods/imgs/test_blades-1.png", width = 2000, height = 2000, units = "px", dpi = 300)
