{-# LANGUAGE ScopedTypeVariables, StrictData #-}

-- | Tests the StrictData LANGUAGE pragma.
module Main where

import qualified Control.Exception as E
import System.IO.Unsafe (unsafePerformIO)

data Strict a = S a
data UStrict = US {-# UNPACK #-} Int

data Lazy a = L a

main = do
    -- Should be _|_:
    print $ isBottom $ S dummy
    print $ isBottom $ US dummy

    putStrLn ""

    -- Should not be _|_:
    print $ not $ isBottom $ L dummy

-- A dummy value to return from functions that are _|_.
dummy :: Int
dummy = 1

------------------------------------------------------------------------
-- Support for testing for bottom

bottom :: a
bottom = error "_|_"

isBottom :: a -> Bool
isBottom f = unsafePerformIO $
  (E.evaluate f >> return False) `E.catches`
    [ E.Handler (\(_ :: E.ArrayException)   -> return True)
    , E.Handler (\(_ :: E.ErrorCall)        -> return True)
    , E.Handler (\(_ :: E.NoMethodError)    -> return True)
    , E.Handler (\(_ :: E.NonTermination)   -> return True)
    , E.Handler (\(_ :: E.PatternMatchFail) -> return True)
    , E.Handler (\(_ :: E.RecConError)      -> return True)
    , E.Handler (\(_ :: E.RecSelError)      -> return True)
    , E.Handler (\(_ :: E.RecUpdError)      -> return True)
    ]
