# Neighborhood rules

neigh_fun <- function(in_mat, rule, n_row, n_col, depth){
  # Function to define neighbors according to different rules.
  # Rules are implemented by applying horizontal and vertical shifts to a world matrix in various ways.
  # Input:
  # - "in_mat": [matrix] input world matrix
  # - "rule": [string] neighborhoods rule to apply (see details below)
  # - "n_row": [integer] row number of "in_mat"
  # - "n_col": [integer] col number of "in_mat"
  # - "depth": [integer] indicates distance at which cells are considered neighbors
  # Output:
  # - "neigh_list": [list] containing all shifted matrices, such that, for a given cell "c", 
  #   each matrix contains one of the neighbors of "c" at the location of "c" in the input world matrix.
  #
  # Neighborhoods rules:
  # - "moore":      neighbors of a given cell are all cells surrounding it, up to a specified distance "depth"
  #                 i.e. a "square" matrix, centered around given cell, with length of 2*depth+1
  #                 Implementing rule:
  #                 - shift world matrix horizontally from -"depth" to "depth"
  #                 - shift each horizontally shifted matrix vertically from -"depth" to "depth"
  # - "vonNeumann:  neighbors are all the cells in a diamond shape centered around a given cell, up to distance "depth"
  #                 i.e. a "diamond" matrix, centered around given cell, with horizontal and vertical axes of length 2*depth+1
  #                 Implementing rule:
  #                 - shift world matrix horizontally from -"depth" to "depth"
  #                 - for each horizontally shifted matrix, apply vertical shift
  #                 - distance of vertical shift is maximum (+-"depth") when horizontal shift is minimum (0), and vice-versa
  # - "diagonal":   neighbors are all the cells on the diagonals passing through a given cell, up to distance "depth"
  #                 Implementing rule:
  #                 - shift world matrix horizontally from -"depth" to "depth" (excluding 0)
  #                 - for each horizontal shift, apply vertical shift only to + and - the distance of the horizontal shift
  # - "shapeS":     neighbors are all the cells two square matrices of side "depth", extending in the upper-right and bottom-left corner from a given cell.
  #                 Implementing rule:
  #                 - shift world matrix horizontally from -"depth" to "depth" (excluding 0)
  #                 - for each left-shift, apply down-shift from -1 to -"depth". For a right-shift, apply an up-shift from 1 to "depth"
  # - "inv_shapeS": neighbors are all the cells two square matrices of side "depth", extending in the bottom-right and upper-left corner from a given cell.
  #                 Implementing rule:
  #                 - shift world matrix horizontally from -"depth" to "depth" (excluding 0)
  #                 - for each left-shift, apply up-shift from 1 to "depth". For a right-shift, apply an down-shift from -1 to -"depth"
  # - "cross":      neighbors are all the cells along a "cross" (i.e. vertical and horizontal line) of side of length "depth" centered around a given cell.
  #                 Implementing rule:
  #                 - shift world matrix horizontally from -"depth" to "depth" (including 0)
  #                 - for horizontal shift of 0, shift matrix vertically from -"depth" to "depth" (excluding 0). 
  #                 - for other horizontal shifts, don't apply vertical shift (i.e. vertical shift of 0)
  # - "blades":     - a combination of "diagonal" and "shapeS". Neighbors are the upper part of the matrix extending from the upper-right corner, 
  #                 and the bottom part of the matrix extending from the bottom-left corner, as the matrices were cut by the diagonal passing through the given cell.
  #                 Implementing rule:
  #                 - shift world matrix horizontally from -"depth" to "depth" (excluding 0)
  #                 - for each horizontal shift, apply vertical a vertical shift from the same amount of horizontal shift to "depth", negative for left-shift, positive for right-shift.
  # - "ex_square":  - neighbors are the cells along the perimeter of a square of length -"depth" to "depth".
  #                 Implementing rule:
  #                 - shift world matrix horizontally from -"depth" to "depth" (including 0)
  #                 - for horizontal shift of + and - "depth", shift cell vertically from -"depth" to "depth". For other horizontal shifts, only shift vertically to -"depth" and to "depth"
  
  # General functions ----

  # These functions provide basic functionalities to shift a world matrix horizontally or vertically.

  ## Horizontal shift ----
  
  hori_shift <- function(in_mat, depth_vec){
    
    # Function to shift a world matrix horizontally
    #
    # Input:
    # - "in_mat": [matrix] input world matrix
    # - "depth_vec": [vector] indicates depth values at which world matrix is shifted. 
    #   Negative values indicate left-shift, positive values indicate right-shift. 0 indicates that original world matrix will be part of the output
    # Output:
    # - "hori_shift_list": [list] containing matrices, each created by shifting world matrix horizontally by one value in "depth_vec".
    #   A 0-padding is applied to shifted matrices to keep dimensions consistent with input world matrix
    
    hori_shift_list <- lapply(
      depth_vec,
      function(x){
        # Function to shift matrix horizontally to a distance of "x"
        if(x<0){
          # If "x" is negative, shift matrix to the left and add 0-padding
          cbind(in_mat[ , -(1:abs(x)), drop = FALSE], matrix(0, nrow = n_row, ncol = abs(x)))
        } else if(x==0){
          # If "x" is 0, return input matrix
          in_mat
        } else if(x>0){
          # If "x" is positive, shift matrix to the right and add 0-padding
          cbind(matrix(0, nrow = n_row, ncol = x), in_mat[ , -((n_col-x+1):n_col), drop = FALSE])
        }
      }
    )
      
    return(hori_shift_list)
  }
  
  ## Vertical shift ----
  
  vert_shift <- function(in_mat, depth_vec){
    # Function to shift a world matrix vertically
    #
    # Input:
    # - "in_mat": [matrix] input world matrix
    # - "depth_vec": [vector] indicates depth values at which world matrix is shifted. 
    #   Negative values indicate down-shift, positive values indicate up-shift. 0 indicates that original world matrix will be part of the output
    # Output:
    # - "vert_shift_list": [list] containing matrices, each created by shifting world matrix vertically by one value in "depth_vec".
    #   A 0-padding is applied to shifted matrices to keep dimensions consistent with input world matrix
    
    vert_shift_list <- lapply(
      depth_vec,
      function(x){
        # Function to shift matrix vertically to a distance of "x"
        if(x<0){
          # If "x" is negative, shift matrix down and add 0-padding
          rbind(matrix(0, nrow = abs(x), ncol = n_col), in_mat[-((n_row+x+1):n_row), , drop = FALSE])
        } else if(x==0){
          # If "x" is 0, return input matrix
          in_mat
        } else if(x>0){
          # If "x" is positive, shift matrix up and add 0-padding
          rbind(in_mat[-(1:x), , drop = FALSE], matrix(0, nrow = x, ncol = n_col))
        }
      }
    )
    
    return(vert_shift_list)
  }
  
  ## Apply rule ----
  
  # Define "depth_vec" vectors for horizontal and vertical shift based on rules
  if (rule == "moore") {
    h_depth_vect <- -depth:depth # vector with horizontal shifts (include 0)
    # For each horizontal shift, define vector of vertical shifts.
    # For horizontal shift of 0, do not include vertical shift of 0 (which would be input matrix)
    v_depth_vect <- lapply(
      h_depth_vect,
      function(x) {
        if (x == 0) {
          c(-depth:-1, 1:depth)
        } else {
          -depth:depth
        }
      }
    )
  } else if (rule == "vonNeumann") {
    h_depth_vect <- -depth:depth # vector with horizontal shifts (include 0)
    # For each horizontal shift, define vector of vertical shifts.
    # When horizontal shift == 0, apply vertical shift of +-depth (not including 0)
    # The higher the horizontal shift, the lower the vertical shift, till reaching 0 when horizontal shift is +-"depth"
    v_depth_vect <- lapply(
      h_depth_vect,
      function(x) {
        if (x == 0) {
          c(-depth:-1, 1:depth)
        } else {
          -(depth - abs(x)):(depth - abs(x))
        }
      }
    )
  } else if (rule == "diagonal") {
    h_depth_vect <- c(-depth:-1, 1:depth) # vector with horizontal shifts (exclude 0)
    # For each horizontal shift, define vector of vertical shifts.
    # Values of the vertical shift are only +/- the value of the horizontal shift
    v_depth_vect <- lapply(
      h_depth_vect,
      function(x) {
        c(-abs(x), abs(x))
      }
    )
  } else if (rule == "shapeS") {
    h_depth_vect <- c(-depth:-1, 1:depth) # vector with horizontal shifts (exclude 0)
    # For each horizontal shift, define vector of vertical shifts.
    # From -1 to -"depth" for left-shift. From 1 to "depth" for right-shift.
    v_depth_vect <- lapply(
      h_depth_vect,
      function(x) {
        if (x < 0) {
          -depth:-1
        } else if (x > 0) {
          1:depth
        }
      }
    )
  } else if (rule == "inv_shapeS") {
    h_depth_vect <- c(-depth:-1, 1:depth) # vector with horizontal shifts (exclude 0)
    # For each horizontal shift, define vector of vertical shifts.
    # From 1 to "depth" for left-shift. From -1 to -"depth" for right-shift.
    v_depth_vect <- lapply(
      h_depth_vect,
      function(x) {
        if (x < 0) {
          1:depth
        } else if (x > 0) {
          -depth:-1
        }
      }
    )
  } else if (rule == "cross") {
    h_depth_vect <- -depth:depth # vector with horizontal shifts (include 0)
    # For each horizontal shift, define vector of vertical shifts.
    # For horizontal shift of 0, apply vertical shift from -"depth" to "depth" (exclude 0). Otherwise do not apply vertical shift.
    v_depth_vect <- lapply(
      h_depth_vect,
      function(x) {
        if (x == 0) {
          c(-depth:-1, 1:depth)
        } else {
          0
        }
      }
    )
  } else if (rule == "blades") {
    h_depth_vect <- c(-depth:-1, 1:depth) # vector with horizontal shifts (exclude 0)
    # For each left-shift, vertical shift is from -value of horizontal shift to -"depth".
    # For each right-shift, vertical shift is from value of horizontal to "depth".
    v_depth_vect <- lapply(
      h_depth_vect,
      function(x) {
        if (x < 0) {
          x:-depth
        } else if (x > 0) {
          x:depth
        }
      }
    )
  } else if (rule == "ex_square") {
    h_depth_vect <- -depth:depth # vector with horizontal shifts (include 0)
    # If horizontal shift is == -"depth" or "depth", shift vertically from -"depth" to "depth"
    # For other horizontal shifts, only shift vertically to -"depth" and "depth".
    v_depth_vect <- lapply(
      h_depth_vect,
      function(x) {
        if (abs(x) != depth) {
          c(-depth, depth)
        } else {
          -depth:depth
        }
      }
    )
  }
  
  ## Shift world matrix ----
  
  neigh_list <- 
    list_flatten(  # flatten all output into a single list
      mapply(
        vert_shift,  # apply vertical shift...
        in_mat = hori_shift(  # ... to each horizontally shifted matrix
          in_mat = in_mat,
          depth_vec = h_depth_vect
        ),
        depth_vec = v_depth_vect
      )
    ) 
  
  return(neigh_list)
  
} 
