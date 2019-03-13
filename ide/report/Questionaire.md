# Tasks

For each of the problems given, try to find the awnser to the following questions 

First open the problem in visual studio code. 
Open up the the model (.essence) file inside vscode. 
To open the command prompt in visual studio code please press Ctrl+shift+p 
Once this is open you can start to type "Conjure" at which point the relevant commands will appear. 

## Using the model command

Try Running the "Model essence" command on the model. 

As you may have noticed the model requires a parameter for it to be solved. 
Open a parameter file from within the params directory. 

## Using the solve command

Try running the "Solve essence" command on the parameter. 

In the bottom right corner of the screen you should see the "solving" message appear. 
In the case that no solution was found you should see a message appear confirming this. 
In the case that a solution was found then the solution file should open automatically. 

## Using the solve and visualise command

Try running the "Solve and visualise" command on the problem. 
After the conjure has been run a new tab name "Tree Visualiser". 
You will see a single node in the middle of the screen.  
This is the root node of a fully collapsed tree. 

Tree visualiser controls: 

* r - Move to the root node 
* s - Move to the next node 
* w - Move to the parent node 
* a - Take the left branch 
* d - Take the right branch 
* shift - Move to the previous node 
* [ - Move to the next solution node 
* ] - Move to the previous solution node 
* e - expand node (deep) 
* c - collapse node (deep) 
* t - toggle node (shallow) 
* f - collapse all failed branches 






 How many solution nodes were found? 
 What were the domains at the first solution? 
 Which failed branch contain the most nodes? 
 