-- -*- encoding: utf-8; fill-column: 95 -*-

-----------------------------------------------------------------------------------------------
--
-- Module        : Data.Global
-- Creation Date : 2011-09-01
-- Authors       : Jean-Marie Gaillourdet <jmg@gaillourdet.net>
-- License       : BSD-style
-- Portability   : all
--
--
-----------------------------------------------------------------------------------------------

-- | 'Data.Global' provides a global namespace of 'IORef's, 'MVar's and 'TVar's. This namespace
-- may be accessed in pure code. Yet reading and writing to those 'IORef's, 'MVar's and 'TVar's
-- happens still in their respective monads.
--
-- 'Data.Global' is designed to meet the following use cases:
--
--   * Simplify the declaration of top-level mutable variables, by avoiding any pragmas as well
--     as 'unsafePerformIO'.
--
--   * Avoid having to pass references explicitly throughout the program in order to let
--     distant parts communicate.
--
--   * Enable a communication by convention scheme, where e.g. different libraries may
--     communicate without code dependencies.
--
--   * Simplify the "configuration problem" - at least for code in the IO monad.
--
--  Note, that this library does not encourage sloppy software design by re-introducing all bad
--  effects of global variables. Nevertheless, sometimes global variables are a suitable
--  solution to a problem. In that case 'Data.Global' simplifies and extends their handling
--  significantly.
module Data.Global (

  -- * Introductory Example
  -- $intro

  -- * The Namespace of Global Variables
  -- $namespace

  -- * Initialization
  -- $init

  -- * Reference of Variable Declaration Functions
    declareIORef
  , declareMVar
  , declareEmptyMVar
  , declareTVar

) where

import Data.Global.Registry



-- $intro
--
-- The most simple usage of 'Data.Global' is as follows:
--
-- Let there be an 'IORef'!
--
-- >>> let ref = declareIORef \"some-cool-variable\" 17
--
-- Use @ref@ like any other 'IORef'.
--
-- >>> readIORef ref
-- 17
--
-- You can do the same with 'MVar's and 'TVar's.



-- $namespace
-- The types of variables: @'IORef' a@, @'MVar' a@, and @'TVar' a@ create separate
-- namespaces. I.e. A variable of type @'IORef' 'Int'@ and one of type @'MVar' 'Int'@ can both
-- exist with the same name.



-- $init
-- References / variables returned by any of the @declare...@ functions are initialized
-- as needed with the value provided to @declare...@. Have a look at this example.
--
-- @
-- someVar1, someVar2 :: IORef Int
-- someVar1 = declareIORef \"my-global-var\" 0
-- someVar2 = declareIORef \"my-global-var\" 1
-- @
--
-- @someVar1@ and @someVar2@ are guaranteed to always denote the exact same 'IORef', but it is
-- unspecified whether the first read access to that 'IORef' returns @0@ or @1@. It can even
-- have any other initial value if it is also accessed from some other part of the program.
