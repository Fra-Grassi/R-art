# Vectorized Neighborhoods

An attempt to replicate Antonio Sánchez Chinchón's approach to create art via **cyclic cellular automata** ([Antonio's blog](https://fronkonstin.com/2021/01/02/neighborhoods-experimenting-with-cyclic-cellular-automata/), [Antonio's GitHub](https://github.com/aschinchon/cyclic-cellular-automata)).

Antonio uses their own C++ code to compute the state of the cell grid at each iteration. Since I have no experience in C++, here I try to replicate the results using a vectorization approach to speed up the code.

Antonio's rules to cycle through cells states are:

+ Create a grid of cells.
+ Give a state to each cell randomly; states is a numbers between 0 and M-1 (you choose the value of M previously).
+ For each cell, count how many of its neighboring cells have their state value exactly 1 unit, in modulo M, greater than the cell’s state.
+ If the resulting number is greater than a certain threshold (that you also choose previously) increment the state of the cell by 1; if cell state reaches value of M, then you have to put 0 (in other words, you have to add 1 modulus M).
+ Repeat two previous steps a number of times.

Different "styles" are obtained by applying different rules for deciding which cells are neighbors.  
At the moment, supported neighborhoods rules are:

- Moore: square matrix around each cell
- von Neumann: "diamond" shaped matrix around each cell
- diagonal: cells along the two diagonals passing through a given cell
- S-shape: square matrix in the upper-right and bottom-left corners around a giving cell
- inversed S-shape: square matrix in the bottom-right and upper-left corners around a giving cell
- cross: cells along the vertical and horizontal axis passing through a given cell
- blades: combination of "diagonal" and "S-shape". Cells along the bottom half of the bottom-left matrix, and the upper half of the upper-right matrix

For each rule, it's possible to set the "depth" (i.e. distance from a given cell) at which cells are considered neighbors.

Inspiration for the vectorization approach comes from [John Mount's R-Bloggers post](https://www.r-bloggers.com/2018/10/conways-game-of-life-in-r-or-on-the-importance-of-vectorizing-your-r-code/) on trading memory for speed in coding Conway's Game of Life