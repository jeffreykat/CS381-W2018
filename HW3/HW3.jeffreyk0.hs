module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen p) (s,n) = case p of
                     (Up) -> ((p,n),Nothing)
                     (Down) -> ((p,n),Nothing)
cmd (Move l r) s  = case s of
                     (Up,_) -> ((Up,(l,r)), Nothing)
                     (Down,d) -> ((Down,(l,r)),Just (d,(l,r)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] s = (s,[])
prog (p:ps) s = case cmd p s of
                 (s',Nothing) -> prog ps s'
                 (s',Just l) -> (\(s,ps) -> (s,l:ps)) $ prog ps s'
                 

--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
buildBox :: Prog
buildBox = [Pen Up, Move 20 2, Pen Down, Move 20 32, Move 40 32, Move 40 2, Move 20 2]

base :: Prog
base = [Move 18 2, Move 18 0, Move 42 0, Move 42 2, Move 40 2, Pen Up]

centerLine :: Prog
centerLine = [Move 30 2, Pen Down, Move 30 28, Pen Up, Move 20 28, Pen Down, Move 40 28, Pen Up]

roofOne :: Prog
roofOne = [Move 21 32, Pen Down, Move 21 34, Move 39 34, Move 39 32, Pen Up]

roofTwo :: Prog
roofTwo = [Move 22 34, Pen Down, Move 22 35, Move 29 36, Move 31 36, Move 38 35, Move 38 34, Pen Up]

light :: Prog
light = [Move 29 36, Pen Down, Move 29 39, Move 31 39, Move 31 36, Pen Up]

doorBox :: Int -> Int -> Prog
doorBox x y = [Move x y, Pen Down, Move (x+8) y, Move (x+8) (y+8), ]

amazing :: Prog
amazing = buildBox ++ base ++ centerLine ++ roofOne ++ roofTwo ++ light

-- Light: 
-- Word Box: 
-- Right Door: Move 31 3, box 31 3
-- Left Door: 
-- Words: 
-- Windows: 
