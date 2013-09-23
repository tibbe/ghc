{-# LANGUAGE ScopedTypeVariables, Strict #-}

-- | Tests the Strict LANGUAGE pragma.
module Main where

import qualified Control.Exception as E
import System.IO.Unsafe (unsafePerformIO)

f x = dummy
{-# NOINLINE f #-}

g ~x = dummy
{-# NOINLINE g #-}

data Strict a = S a

data Lazy a = L a  -- TODO: ~ is not supported by the parser yet

main = do
    -- Should be _|_:
    print $ isBottom (f bottom)
    print $ isBottom $ case bottom of
        x -> dummy
    print $ isBottom (let x = bottom in dummy)
    print $ isBottom (let _ = bottom in dummy)
    print $ isBottom $ S bottom

    putStrLn ""

    -- Should not be _|_:
    print $ not $ isBottom (g bottom)
    print $ not $ isBottom $ case (Just bottom) of
        (Just x) -> dummy
    print $ not $ isBottom (let ~x = bottom in dummy)
    print $ not $ isBottom (let ~_ = bottom in dummy)
    print $ not $ isBottom $ L bottom

-- A dummy value to return from functions that are _|_.
dummy :: Int
dummy = 1

------------------------------------------------------------------------
-- Support for testing for bottom

bottom :: a
bottom = error "_|_"

isBottom :: a -> Bool
isBottom ~f = unsafePerformIO $
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
