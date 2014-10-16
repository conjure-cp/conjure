module Main where

import Conjure.Prelude
import Conjure.UI.IO ( readModelFromFile )
import Conjure.UI.Model ( initialise, outputAllModels, interactive )
import Conjure.Language.ModelStats ( modelInfo )
import Conjure.Language.Pretty ( renderWide )

main :: IO ()
main = do
    args <- getArgs
    let essencePaths = [ a | a <- args, ".essence" `isSuffixOf` a ]
    -- let paramPaths   = [ a | a <- args, ".param"   `isSuffixOf` a ]
    essencePath <- case essencePaths of
        [a] -> return a
        []  -> error "Provide a *.essence file."
        _   -> error "Provide a single *.essence file."
    essence <- readModelFromFile essencePath
    putStrLn $ renderWide essence
    putStrLn $ renderWide $ modelInfo essence
    outputAllModels interactive putStrLn
                    "conjure-output" 1 (initialise essence)



-- INTERACTIVE

-- (Unless a .essence is given as argument)
-- Conjure works on problem specifications written in Essence.
-- You can load one by giving it as an argument to Conjure
-- Or by using the command `:load <filename>`

-- File parsed and type checked. Options
-- Notice: Intermediate files will be saved in <dir>
-- Conjure by default saves all intermediate files it generates.
-- Better safe than sorry.


-- (Once the model is complete)
-- Do you want to solve this model?
-- (Check if the model needs params)
-- (Unless a .param file is given as argument)
-- This problem seems to require parameters.
-- `:load <filename>`
-- Use default settings for Savile Row and Minion?
-- YES
-- NO, for SR:... for Minion:...
-- (If optimisation, wanna see intermediate solutions?)
-- (If sat, wanna see 1 solution, n solutions, all solutions, just number of solutions?)
-- Solved.
-- SR Time, CSEs, blah...
-- Minion Time, Nodes, blah...


-- conjure compact .essence > .eprime
-- conjure refineParam .eprime .param > .eprime-param
-- conjure translateSolution .eprime .eprime-solution > .solution
-- conjure compact-solve .essence .param > .solution --savilerow-options "" --minion-options ""

-- SOLVING
-- savilerow -in-eprime .eprime -in-param .eprime-param -out-minion .minion (rm everything else)
-- minion .minion > SOLUTION
-- savilerow -mode translateSolution


-- (invokes the INTERACTIVE)
-- conjure 
-- conjure .essence
-- conjure .essence .param






-- INTERACTIVE MODELLING
-- Conjure works by first selecting refinements for abstract parameters and decision variables (declarations)
-- and then refining expressions depending on the domain refinements. (TODO: Expand.)

-- There are # parameters and # decision variables in this problem specification.
-- # of the parameters are abstract and # of the decision variavles are abstract,
-- hence they will require domain refinement.
-- Moreover, Conjure can explore multiple domain refinements for each parameter and decision variable.
-- Channelling constraints will be generated automatically when multiple domain refinements are used in a single model.
-- Options (Channelled or not)
-- 1. Explore all combinations
-- 2. Explore all combinations, except channelled models
-- 3. Explore all combinations, except channelled models for parameters
-- 4. I want to choose per declaration
--          For x:set of tau (occurs # times)
--              no channelling
--              full channelling
--              full channelling + one redundant refinement
--              up to n different refinements
--          ...






