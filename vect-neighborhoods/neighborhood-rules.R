# Neighborhood rules
#
# Functions to define neighbors according to different rules

# General functions ----

# These functions provide basic functionalities to shift a world matrix horizontally or vertically.

## Horizontal shift ----

hori_shift <- function(in_mat, n_row, n_col, depth_vec){
  # Function to shift a world matrix horizontally
  #
  # Input:
  # - "in_mat": [matrix] input world matrix
  # - "n_row": [integer] row number of "in_mat"
  # - "n_col": [integer] col number of "in_mat"
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

vert_shift <- function(in_mat, n_row, n_col, depth_vec){
  # Function to shift a world matrix vertically
  #
  # Input:
  # - "in_mat": [matrix] input world matrix
  # - "n_row": [integer] row number of "in_mat"
  # - "n_col": [integer] col number of "in_mat"
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

# Neighborhoods rules ----

# Functions implementing specific neighborhoods rules by combining horizontal and vertical shifts

## Moore neighborhoods ----

neigh_moore <- function(in_mat, n_row, n_col, depth){
  # Neighbors of a given cell are all cells surrounding it, up to a specified distance "depth"
  # i.e. a "square" matrix, centered around given cell, with length of 2*depth+1
  # Implementing rule:
  # - shift world matrix horizontally from -"depth" to "depth"
  # - shift each horizontally shifted matrix vertically from -"depth" to "depth"
  # Input:
  # - "in_mat": [matrix] input world matrix
  # - "n_row": [integer] row number of "in_mat"
  # - "n_col": [integer] col number of "in_mat"
  # - "depth": [integer] indicates distance at which cells are considered neighbors
  # Output:
  # - "neigh_list": [list] containing all shifted matrices, such that, for a given cell "c", 
  #   each matrix contains one of the neighbors of "c" at the location of "c" in the input world matrix.
  
  h_depth_vect <- -depth:depth  # vector with horizontal shifts (include 0)
  
  # For each horizontal shift, define vector of vertical shifts.
  # For horizontal shift of 0, do not include vertical shift of 0 (which would be input matrix)
  v_depth_vect <- lapply(
    h_depth_vect,
    function(x){
      if(x==0){
        c(-depth:-1, 1:depth)
      } else{
        -depth:depth
      }
    }
  )
  
  # Shift world matrix:
  neigh_list <- 
    list_flatten(  # flatten all output into a single list
      mapply(
        vert_shift,  # apply vertical shift...
        in_mat = hori_shift(  # ... to each horizontally shifted matrix
          in_mat = in_mat,
          n_row = n_row,
          n_col = n_col,
          depth_vec = h_depth_vect
        ),
        depth_vec = v_depth_vect,
        MoreArgs = list(n_row = n_row, n_col = n_col)
      )
    )
  
  return(neigh_list)
  
}

## von Neumann neighborhood ----

neigh_vonNeumann <- function(in_mat, n_row, n_col, depth){
  # Neighbors are all the cells in a diamond shape centered around a given cell, up to distance "depth"
  # i.e. a "diamond" matrix, centered around given cell, with horizontal and vertical axes of length 2*depth+1
  # Implementing rule:
  # - shift world matrix horizontally from -"depth" to "depth"
  # - for each horizontally shifted matrix, apply vertical shift
  # - distance of vertical shift is maximum (+-"depth") when horizontal shift is minumum (0), and viceversa
  # Input:
  # - "in_mat": [matrix] input world matrix
  # - "n_row": [integer] row number of "in_mat"
  # - "n_col": [integer] col number of "in_mat"
  # - "depth": [integer] indicates distance at which cells are considered neighbors
  # Output:
  # - "neigh_list": [list] containing all shifted matrices, such that, for a given cell "c", 
  #   each matrix contains one of the neighbors of "c" at the location of "c" in the input world matrix.
  
  h_depth_vect <- -depth:depth  # vector with horizontal shifts (include 0)
  
  # For each horizontal shift, define vector of vertical shifts.
  # When horizontal shift == 0, apply vertical shift of +-depth (not including 0)
  # The higher the horizontal shift, the lower the vertical shift, till reaching 0 when horizontal shift is +-"depth"
  v_depth_vect <- lapply(
    h_depth_vect,
    function(x){
      if(x==0){
        c(-depth:-1, 1:depth)
      } else{
        -(depth-abs(x)):(depth-abs(x))
      }
    }
  )
  
  # Shift world matrix:
  neigh_list <- 
    list_flatten(  # flatten all output into a single list
      mapply(
        vert_shift,  # apply vertical shift...
        in_mat = hori_shift(  # ... to each horizontally shifted matrix
          in_mat = in_mat,
          n_row = n_row,
          n_col = n_col,
          depth_vec = h_depth_vect
        ),
        depth_vec = v_depth_vect,
        MoreArgs = list(n_row = n_row, n_col = n_col)
      )
    )
  
  return(neigh_list)
   
}

## Diagonal neighborhood ----

neigh_diagonal <- function(in_mat, n_row, n_col, depth){
  # Neighbors are all the cells on the diagonals passing through a given cell, up to distance "depth"
  # Implementing rule:
  # - shift world matrix horizontally from -"depth" to "depth" (excluding 0)
  # - for each horizontal shift, apply vertical shift only to + and - the distance of the horizontal shift
  # Input:
  # - "in_mat": [matrix] input world matrix
  # - "n_row": [integer] row number of "in_mat"
  # - "n_col": [integer] col number of "in_mat"
  # - "depth": [integer] indicates distance at which cells are considered neighbors
  # Output:
  # - "neigh_list": [list] containing all shifted matrices, such that, for a given cell "c", 
  #   each matrix contains one of the neighbors of "c" at the location of "c" in the input world matrix.
  
  h_depth_vect <- c(-depth:-1, 1:depth)  # vector with horizontal shifts (exclude 0)
  
  # For each horizontal shift, define vector of vertical shifts.
  # Values of the vertical shift are only +/- the value of the horizontal shift
  v_depth_vect <- lapply(
    h_depth_vect,
    function(x){
       c(-abs(x), abs(x)) 
      }
  )
  
  # Shift world matrix:
  neigh_list <- 
    list_flatten(  # flatten all output into a single list
      mapply(
        vert_shift,  # apply vertical shift...
        in_mat = hori_shift(  # ... to each horizontally shifted matrix
          in_mat = in_mat,
          n_row = n_row,
          n_col = n_col,
          depth_vec = h_depth_vect
        ),
        depth_vec = v_depth_vect,
        MoreArgs = list(n_row = n_row, n_col = n_col)
      )
    )
  
  return(neigh_list)

}

## S-shaped neighborhood ----

neigh_shapeS <- function(in_mat, n_row, n_col, depth){
  # Neighbors are all the cells two square matrices of side "depth", extending in the upper-right and bottom-left corner from a given cell.
  # Implementing rule:
  # - shift world matrix horizontally from -"depth" to "depth" (excluding 0)
  # - for each left-shift, apply down-shift from -1 to -"depth". For a right-shift, apply an up-shift from 1 to "depth"
  # Input:
  # - "in_mat": [matrix] input world matrix
  # - "n_row": [integer] row number of "in_mat"
  # - "n_col": [integer] col number of "in_mat"
  # - "depth": [integer] indicates distance at which cells are considered neighbors
  # Output:
  # - "neigh_list": [list] containing all shifted matrices, such that, for a given cell "c", 
  #   each matrix contains one of the neighbors of "c" at the location of "c" in the input world matrix.
  
  h_depth_vect <- c(-depth:-1, 1:depth)  # vector with horizontal shifts (exclude 0)
  
  # For each horizontal shift, define vector of vertical shifts.
  # From -1 to -"depth" for left-shift. From 1 to "depth" for right-shift.
  v_depth_vect <- lapply(
    h_depth_vect,
    function(x){
       if(x<0){
         -depth:-1
       } else if(x>0){
         1:depth
       }
      }
  )
  
  # Shift world matrix:
  neigh_list <- 
    list_flatten(  # flatten all output into a single list
      mapply(
        vert_shift,  # apply vertical shift...
        in_mat = hori_shift(  # ... to each horizontally shifted matrix
          in_mat = in_mat,
          n_row = n_row,
          n_col = n_col,
          depth_vec = h_depth_vect
        ),
        depth_vec = v_depth_vect,
        MoreArgs = list(n_row = n_row, n_col = n_col)
      )
    )
  
  return(neigh_list)

}

## Inversed S-shaped neighborhood ----

neigh_inv_shapeS <- function(in_mat, n_row, n_col, depth){
  # Neighbors are all the cells two square matrices of side "depth", extending in the bottom-right and upper-left corner from a given cell.
  # Implementing rule:
  # - shift world matrix horizontally from -"depth" to "depth" (excluding 0)
  # - for each left-shift, apply up-shift from 1 to "depth". For a right-shift, apply an down-shift from -1 to -"depth"
  # Input:
  # - "in_mat": [matrix] input world matrix
  # - "n_row": [integer] row number of "in_mat"
  # - "n_col": [integer] col number of "in_mat"
  # - "depth": [integer] indicates distance at which cells are considered neighbors
  # Output:
  # - "neigh_list": [list] containing all shifted matrices, such that, for a given cell "c", 
  #   each matrix contains one of the neighbors of "c" at the location of "c" in the input world matrix.
  
  h_depth_vect <- c(-depth:-1, 1:depth)  # vector with horizontal shifts (exclude 0)
  
  # For each horizontal shift, define vector of vertical shifts.
  # From 1 to "depth" for left-shift. From -1 to -"depth" for right-shift.
  v_depth_vect <- lapply(
    h_depth_vect,
    function(x){
       if(x<0){
         1:depth
       } else if(x>0){
         -depth:-1
       }
      }
  )
  
  # Shift world matrix:
  neigh_list <- 
    list_flatten(  # flatten all output into a single list
      mapply(
        vert_shift,  # apply vertical shift...
        in_mat = hori_shift(  # ... to each horizontally shifted matrix
          in_mat = in_mat,
          n_row = n_row,
          n_col = n_col,
          depth_vec = h_depth_vect
        ),
        depth_vec = v_depth_vect,
        MoreArgs = list(n_row = n_row, n_col = n_col)
      )
    )
  
  return(neigh_list)

}