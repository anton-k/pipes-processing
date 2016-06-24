import Data.Monoid

import Graphics.Proc
import Data.Tuple
import Gen
import DrawPipes

import Pipes
import qualified Pipes.Prelude as P

-------------------------
-- value producer that holds the state
-- state is a rotation angle

dxs = osc 0.2

-------------------------
-- code for drawing the state

renderCircle rad x = do 
    transform    
    fill white
    circle 0.1 (0, 0)       
    drawCircle red   rad x
    drawCircle green (rad / 2) (-2 * x)   
    drawCircle blue  (rad * 1.4) (1.3 * x + 0.2)    

drawCircle col rad x = local $ do           
    fill col    
    rotate x
    translate rad               
    circle 0.051 (0, 0)

transform = do
    background black
    translate center
    scale (width / 2, height / 2)

-------------------------
-- setup the scene

width = 400
height = 400

sizes = (width, height)
center = 0.5 *^ sizes

setup = do
    noStroke
    size sizes    

-------------------------
-- render

main = runGen setup (renderCircle 0.4) dxs


