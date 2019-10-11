 sh ../../acceptOutput.sh enum/0002_given_and_letting           
 sh ../../acceptOutput.sh enum/0003_letting_and_find            
 sh ../../acceptOutput.sh enum/0004_find_and_find               
 sh ../../acceptOutput.sh int/0002_given_and_letting            
 sh ../../acceptOutput.sh int/0005_find_composition             
 sh ../../acceptOutput.sh int/0003_letting_and_find             
 sh ../../acceptOutput.sh int/0004_find_and_find                
 sh ../../acceptOutput.sh int/0002_letting_and_given            
 sh ../../acceptOutput.sh int/0006_find_composition             
 sh ../../acceptOutput.sh unnamed/0004_find_and_find            

# TODO what's wrong here?
#    permutations.07_compose.enum.0001_given_permutations_in_param: 
#      Running
#      Checking stderr
#        src/test/Conjure/Custom.hs:86:
#        unexpected stderr:
#            Error:
#                permutation.essence:3:1:
#            unexpected given
#            expecting end of input or letting statement
#                given p : permutation of n
#                ^
#            cat: conjure-output/*.solution: No such file or directory
#        was expecting:    
#    permutations.07_compose.int.0001_given_permutations_in_param:  FAIL (0.31s)
#      Running
#      Checking stderr
#        src/test/Conjure/Custom.hs:86:
#        unexpected stderr:
#            Error:
#                permutation.essence:3:1:
#            unexpected given
#            expecting end of input, letting statement, or rest of letting statement
#                given p : permutation of int(1..n)
#                ^
#            cat: conjure-output/*.solution: No such file or directory
#        was expecting:    
