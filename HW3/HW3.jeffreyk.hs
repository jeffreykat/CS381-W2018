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
buildBox = [Pen Up, Move 20 1, Pen Down, Move 20 34, Move 40 34, Move 40 1, Move 20 1]

base :: Prog
base = [Move 18 1, Move 18 0, Move 42 0, Move 42 1, Move 40 1, Pen Up]

centerLine :: Prog
centerLine = [Move 30 1, Pen Down, Move 30 30, Pen Up, Move 20 30, Pen Down, Move 40 30, Pen Up]

roofOne :: Prog
roofOne = [Move 21 34, Pen Down, Move 21 35, Move 39 35, Move 39 34, Pen Up]

roofTwo :: Prog
roofTwo = [Move 22 35, Pen Down, Move 22 36, Move 29 37, Move 31 37, Move 38 36, Move 38 35, Pen Up]

light :: Prog
light = [Move 29 37, Pen Down, Move 29 39, Move 31 39, Move 31 37, Pen Up, Move 29 39, Pen Down, Move 30 40, Move 31 39, Pen Up]

doorBox :: Int -> Int -> Prog
doorBox x y = [Move x y, Pen Down, Move (x+6) y, Move (x+6) (y+6), Move x (y+6), Move x y, Pen Up]

windowOne :: Prog
windowOne = [Pen Up, Move 25 23, Pen Down, Move 25 29, Pen Up, Move 27 29, Pen Down, Move 27 23, Pen Up, Move 23 26, Pen Down, Move 29 26, Pen Up]

windowTwo :: Prog
windowTwo = [Move 33 23, Pen Down, Move 33 29, Pen Up, Move 35 23, Pen Down, Move 35 29, Pen Up, Move 31 26, Pen Down, Move 37 26, Pen Up]

details :: Prog
details = box 33 18 ++ [Pen Up, Move 24 16, Pen Down, Move 24 22, Pen Up, Move 28 16, Pen Down, Move 28 22, Pen Up, Move 26 16, Pen Down, Move 28 16, Pen Up]

boxWords :: Prog
boxWords = [Move 21 31, Pen Down, Move 21 33, Move 22 33, Move 22 32, Move 21 32, Pen Up, Move 23 31, Pen Down, Move 23 33, Move 24 33, Move 24 31, Move 23 31, Pen Up, Move 25 33, Pen Down, Move 25 31, Move 26 31, Pen Up, Move 27 31, Pen Down, Move 27 33, Pen Up, Move 29 33, Pen Down, Move 28 33, Move 28 31, Move 29 31, Pen Up, Move 31 31, Pen Down, Move 30 31, Move 30 33, Move 31 33, Pen Up, Move 30 32, Pen Down, Move 31 32, Pen Up, Move 34 31, Pen Down, Move 34 33, Move 35 33, Move 35 32, Move 34 32, Pen Up, Move 34 31, Pen Down, Move 35 31, Move 35 32, Pen Up, Move 36 31, Pen Down, Move 36 33, Move 37 33, Move 37 31, Move 36 31, Pen Up, Move 38 31, Pen Down, Move 39 33, Pen Up, Move 38 33, Pen Down, Move 39 31, Pen Up]

amazing :: Prog
amazing = buildBox ++ base ++ centerLine ++ roofOne ++ roofTwo ++ light ++ doorBox 31 2 ++ doorBox 31 9 ++ doorBox 31 16 ++ doorBox 23 2 ++ doorBox 23 9 ++ doorBox 23 16 ++ doorBox 23 23 ++ windowOne ++ doorBox 31 23 ++ windowTwo ++ boxWords ++ details

