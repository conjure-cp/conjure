Futoshiki 
---------

Author: Gökberk and Ruth

n x n board where each column and row is filled with the unique numbers from 1 to n, similar to a sudoku.
In contrast to sudoku, there are less than and greater than symbols between cells indicating that one cell has to be filled with a number grater than (or less than) than the cell on the other side of the operator.

Let us look at the model first

given n : int
Defines the size of the matrix and the maximal number of elements that we will add.

letting DOMAIN be domain int(1..n)
We start at 1 and go up to n (for both the elements of the cells and the cell hintations).

given hints : function (DOMAIN, DOMAIN) --> DOMAIN
Here we define which cells are already filled in using a function relation. 
The first part of the function is the coordinates and the second part of it is the number that is in that cell.

given less_than : relation of ((DOMAIN, DOMAIN) * (DOMAIN, DOMAIN))
Here we define where the relation symbols are placed, it depends on which way you read the symbol we are "only" modelling less than. 
So should there be a "greater than" signs between two cells, say (2,1) and (1,1) then we would order them as (1,1) is less than (2,1). 

find board : matrix indexed by [DOMAIN, DOMAIN] of DOMAIN 
We are now telling the solver that we are trying to find a n x n board with elements from 1 to n in each cell.

Onwards to define the actual constraints.
such that
    forAll (hint,num) in hints .
        board[hint[1], hint[2]] = num,
This constraint defines the hints, so the cells that are filled in when we get the puzzle.
    forAll i: DOMAIN .
        allDiff(board[i,..]),
This constraint defines that every cell in a row has to be an unique number between 1 and n.
    forAll j: DOMAIN .
        allDiff(board[..,j]),
This constraint defines that every cell in a column has to be an unique number between 1 and n.
    forAll (l,g) in less_than .
        board[l[1],l[2]] < board[g[1],g[2]]
Finally this constraint enforces the less than relation.


The .essence-param file contains the information about our starting board of a specific instance that we want to solve.
See the picture at the beginning to see what it looks like.

letting n be 4
We are dealing with a 4 by 4 board.
letting hints be function(
        (1,1) --> 2,
        (2,2) --> 2
)
There will be 2 two's on the board given as a hint. One in the top left corner (1,1) and the second number 2 in cell (2,2).
letting less_than be relation(
        ((1,1) , (2,1)),
        ((4,2) , (3,2)),
        ((3,3) , (3,4)),
        ((3,4) , (4,4))
)
There are 4 relation symbols on the board, between cells.
