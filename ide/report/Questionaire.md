# Tasks

Please follow the instructions below for each of the problems given.

To open the command prompt in visual studio code please press Ctrl+shift+p.  
Once this is open typing "Conjure" will cause the relevant commands to appear. 

## Using the model command

First open the problem in visual studio code.  

Open up the the model (.essence) file by selecting it from the file explorer in the left side panel. 

Try Running the "Model essence" command on the model. 

The generated eprime file should automatically open in a new tab.


## Using the solve command

As you may have noticed, the model requires a parameter before it can be executed.

Open up a parameter file from the params directory.

Try running the "Solve essence" command on the parameter. 

* In the bottom right corner of the screen you should see the "solving" message appear. 
* In the case that no solution was found you should see a message confirming this. 
* In the case that a solution was found then the solution file should open automatically. 

## Using the solve and visualise command

Try running the "Solve and visualise" command on the parameter file. 

After the conjure has been run a new tab called "Tree Visualiser" should open automatically. 

You will see a single node in the middle of the screen.  

This is the root node of a fully collapsed tree. 

 ### Tree visualiser controls

#### Keybindings

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

#### Checkboxes
* Pretty Domains  
	* If checked the domains are rendered in a tree list. Sets are visualised in terms of native essence.
	* If unchecked the domains are rendered as a table with no preprocessing.
* Pretty Labels
	* If checked the labels on each branch are displayed in terms of native essence.
	* If unchecked the branches are labeled as they appear in minion.
* Freeze Expressions
	* If checked expressions are not updated
	* If unchecked expressions are updated
* Freeze Domains
	* If checked all domains are frozen

### Questions

Use the visualisation to find the answers to the following questions: 
* Did the domains change after Root Propagation?  
□ Yes 
□ No 
* How many solution nodes were found? 
* What were the domains at the first solution?
* Which failed branch contains the most nodes? 
* At which node did the lowerbound on the cardinality of the set first become greater than zero? 
* At which node was x first excluded from the set? 


### PartB


