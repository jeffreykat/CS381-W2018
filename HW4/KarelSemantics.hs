module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = case test t w r of 
                    True -> False
                    False -> True
test (Facing c) w r = (getFacing r) == c
test (Clear d) w r = isClear (relativePos d r) w
test Beeper w r = hasBeeper (getPos r) w
test Empty w r = isEmpty r
                       

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r 
                       in if isEmpty r 
                             then Error ("No beeper to put.")      
                             else OK (incBeeper p w) (decBag r)
stmt Move _ w r = let p = relativePos Front r
                  in if isClear p w 
                        then OK w (setPos p r)
                        else Error ("Blocked at: " ++ show p)
stmt (Turn d ) _ w r = let f = cardTurn d (getFacing r)
                       in if (getFacing r) == f
                          then OK w r
                          else OK w (setFacing f r)
stmt (Block []) _ w r = OK w r
stmt (Block (x:xs)) d w r = case stmt x d w r of
                             (OK w' r') -> stmt (Block xs) d w' r'
                             (Done r') -> Done r'
                             (Error e) -> Error e
stmt (If t s1 s2) d w r = if (test t w r)
                             then stmt s1 d w r
                             else stmt s2 d w r
stmt (Call m) d w r = case lookup m d of 
                       (Just a) -> stmt a d w r
                       _        -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s) d w r = if i > 0
                              then case stmt s d w r of 
                                   (OK w' r') -> stmt (Iterate (i-1) s) d w' r'
                                   (Done r') -> Done r'
                                   (Error e) -> Error e
                              else OK w r
stmt (While t s) d w r = if (test t w r) 
                            then case stmt s d w r of
                                  (OK w' r') -> stmt (While t s) d w' r'
                                  (Done r') -> Done r'
                                  (Error e) -> Error e
                            else OK w r
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
