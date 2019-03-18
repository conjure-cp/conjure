# The Problem
This problem consists in finding a partition of numbers 1..N into two sets A and B such that:

* A and B have the same cardinality.  
* sum of numbers in A = sum of numbers in B  
* sum of squares of numbers in A = sum of squares of numbers in B  

There is no solution for N<8.  

Here is an example forN=8:A=(1,4,6,7) and B=(2,3,5,8)  
Then from N>=8, there is no solution if N is not a multiple of 4.  


# Tasks

Please follow the instructions below for each of the tasks given.

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

### Part A

Use the visualisation to find the answers to the following questions: 

1. Did the domains change after Root Propagation?  
□ Yes  
□ No  
	How easy was it to find the answer?  
	<pre>
	□ Very Easy     □ Easy     □ Neutral 	□ Hard	   □ Very Hard
	</pre>





2. How many solution nodes were found?   
□ 0   
□ 1   
□ 2   
	How easy was it to find the answer?  
	<pre>
	□ Very Easy     □ Easy     □ Neutral 	□ Hard	   □ Very Hard
	</pre>

3. Which set was 3 included in at the first solution?  
□ Set A  
□ Set B  
	How easy was it to find the answer?  
	<pre>
	□ Very Easy     □ Easy     □ Neutral 	□ Hard	   □ Very Hard
	</pre>


4. Which failed branch contains the most nodes?   
□ First branch  
□ Second branch   
□ Third branch   
□ Fourth branch   
	How easy was it to find the answer?  
	<pre>
	□ Very Easy     □ Easy     □ Neutral 	□ Hard	   □ Very Hard
	</pre>

5. At which node did the upperbound on the cardinality of setB become less than 20?  
□ 1  
□ 11  
□ 14853  
□ 6  
	How easy was it to find the answer?  
	<pre>
	□ Very Easy     □ Easy     □ Neutral 	□ Hard	   □ Very Hard
	</pre>


6. At which node was 10 first excluded from set A?   
□ 41  
□ 22743  
□ 500  
□ 19  
	How easy was it to find the answer?  
	<pre>
	□ Very Easy     □ Easy     □ Neutral 	□ Hard	   □ Very Hard
	</pre>


### Part B (optional)
Within the root directory of the problem you will find a config.json file.  
This file contains command line arguments that will be passed to minion and savilerow when executing commands from within vscode.  
Experiment with different command line arguments and see how the tree changes.

### Part C 

<pre>

What did you like about the extension?

















What aspects would you like to be improved?






















</pre>