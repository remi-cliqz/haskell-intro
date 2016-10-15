{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

sierpinski :: Int -> Diagram B
sierpinski 1 = triangle 1
sierpinski n =     s
                  ===
               (s ||| s) # centerX
    where s = sierpinski (n-1)

main = mainWith (sierpinski 10)
